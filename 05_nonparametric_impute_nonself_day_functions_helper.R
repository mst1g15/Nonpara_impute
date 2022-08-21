


find_imp <- function(sleep_interval, long_sleep_interval){
#given the sleep extras and the normal sleep interval, identify the imputation intervals 
  
  
  if(nrow(long_sleep_interval)==0){
    imp <- NULL 
  }else{
  
    
  imp <- c()
  #for weekends 
  
    for (i in 1:nrow(long_sleep_interval)){
      
      #check whether the long sleep period overlaps with EACH sleep interval period 
      overlap<- int_overlaps(long_sleep_interval$interval[i], sleep_interval$interval)
      
      #impute on left side? 
      left <- long_sleep_interval$start_time[i] < sleep_interval$mean_start_time
      impleft <- left ==T & overlap==T
      
      #impute on right side?
      right <- long_sleep_interval$end_time[i] > sleep_interval$mean_end_time
      impright <- right==T & overlap==T
      
      check_double <- sum(impleft) > 0 & sum(impright) > 0 & sum(impleft & impright)==0
      #info on left and right side imputation intervals 
      if(sum(impleft)>0){
        left_info <-data.frame("start_time"=long_sleep_interval$start_time[i], 
                                      "day"=long_sleep_interval$day[i], 
                                      "date"=long_sleep_interval$date[i], 
                                      "end_time"=sleep_interval$mean_start_time[impleft])
        left_int=interval(long_sleep_interval$start_time[i],sleep_interval$mean_start_time[impleft])
      }else{
        left_int=interval(NA,NA)
        left_info=NULL
      }        
      
      if(sum(impright)>0){
        right_info <-  data.frame("start_time"=sleep_interval$mean_end_time[impright], 
                                     "day"=long_sleep_interval$day[i], 
                                     "date"=long_sleep_interval$date[i], 
                                     "end_time"=long_sleep_interval$end_time[i])
        right_int <- interval(sleep_interval$mean_end_time[impright],long_sleep_interval$end_time[i])
      }else{
        right_int=interval(NA, NA)
        right_info <- NULL
      }  
      
      if(check_double==TRUE & int_overlaps(right_int, left_int)==TRUE){
        start <- max(right_info$start_time, left_info$start_time)
        end <- min(right_info$end_time, left_info$end_time)
        right_info$start_time=start
        right_info$end_time=end
        left_info <- NULL
        
      
      }

      #if no interval, then the whole sleep extra period is an imputation interval
    #  if(sum(impleft)==0 & sum(impright)==0){
        
    #    imp <- rbind(imp, long_sleep_interval[i,] %>% select("start_time", "day", "date", "end_time"))
        
        
        
     # }else{
        imp <- rbind(imp, right_info, left_info)
      #}
      
    }
  
  }

  return(imp)
}






induce_realistic_missing <- function(Simdata, getmiss, prop=0.4, prop.whole=0.1){
#for a dataset with no missing data, induce missingness using missingness profiles from 
# a real dataset (e.g. 12 month data)
  
  #getmiss - the output from (calc_runs) from the real dataset with missingness 
  
  
  SimdataID <- unique(Simdata$ID)
  n <- length(SimdataID)
  getmissID <- unique(getmiss$ID)
  missing_id <- sample(SimdataID, round(prop*n))
  missing_id_whole <- sample(SimdataID[!(SimdataID %in% missing_id)], round(prop.whole*n))
  
  #complete data
  induced_dat <- c()
  
  for (i in missing_id){
    
    Simdata_p <- Simdata[Simdata$ID==i,]
    
    #get a random person with missingness
    rand <- sample(getmissID, 1)
    getmiss_rand <- getmiss[getmiss$ID==rand,]
    getmiss_rand
    
    nonwear <- getmiss_rand %>% filter(type=="nonwear")
    sleep_extra <-getmiss_rand %>% filter(type=="sleep_extra")
    
    
    if(nrow(nonwear)>0){
      
      #induce nonwear according to the selected person's profile 
      new_dat <- induce_nonwear(Simdata_p, nonwear)
      
    }else{
      
      new_dat <- Simdata_p
    }
    
    
    if(nrow(sleep_extra)>0){
      
      #induce sleep extra according to the selected person's profile 
      done_dat <- induce_sleep_extra(new_dat, sleep_extra)
      
    }else{
      done_dat <- new_dat
    }
    
    induced_dat <- rbind(induced_dat, done_dat)
    
    rm(done_dat)
    rm(new_dat)
    
  }
  
  induced_dat2 <- c()
  
  
  
  for (i in missing_id_whole){
    
    Simdata_p <- Simdata[Simdata$ID==i,]
    
    
    Simdata_p$VM <- 0
    Simdata_p$Steps <- 0
    
    induced_dat2 <- rbind(induced_dat2, Simdata_p)
  }
  
  #complete data
  complete_dat <- Simdata %>% filter(!(ID %in% c(missing_id, missing_id_whole)))
  total_dat <- rbind(induced_dat, induced_dat2, complete_dat)
  
  return(total_dat)
  
}




induce_sleep_extra <- function(Simdata_p, sleepextradat){
  
  
  for (i in 1:nrow(sleepextradat)){
    
    if(sleepextradat$start[i] < nrow(Simdata_p) & sleepextradat$end[i] < nrow(Simdata_p)){
      
      Simdata_p$VM[sleepextradat$start[i]:sleepextradat$end[i]] <- 0
      Simdata_p$Steps[sleepextradat$start[i]:sleepextradat$end[i]] <- 0
      
    }
  }
  
  
  return(Simdata_p)
  
}


induce_nonwear <- function(Simdata_p, nonweardat){
  
  
  for (i in 1:nrow(nonweardat)){
    
    if(nonweardat$start[i] < nrow(Simdata_p) & nonweardat$end[i] < (nrow(Simdata_p)-1)){
      
      
      Simdata_p$VM[nonweardat$start[i]:nonweardat$end[i]] <- 0
      Simdata_p$Steps[nonweardat$start[i]:nonweardat$end[i]] <- 0
      
      #Simdata_p$VM[nonweardat$start[i]-1] <- 600
      #Simdata_p$VM[nonweardat$end[i]+1] <- 600
    }
    
  }
  
  
  return(Simdata_p)
}