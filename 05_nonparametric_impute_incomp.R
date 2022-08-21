#---------------------------------------------------------
#Purpose: nonparametric imputation procedure for more complex and realistic settings 
#         used in full simulation 
#Date: 21 Dec 2021
#-----------------------------------------------------------

simple_analysis <- function(dataset){
  #find mean step per person and its standard error. 
  
  dat_variables <- dataset %>% select(ID, Steps) 
  week_average <- dat_variables %>% group_by(ID) %>% summarize(mean_steps=sum(Steps)/7)
  modeltrue <- summary(lm(week_average$mean_steps~1))
  results <- modeltrue$coefficients[1:2] # estimate of mean and SE
  
  return(results)
  
}


cc_analysis <- function(Simdata, weartimes_all, cc_save_path=NULL, sim){
  #find mean step per person and its standard error. 
  
  daily_steps <- Simdata %>% group_by(ID, Date, mean_steps_B, group) %>% summarize(daily_steps=sum(Steps))
  daily_steps_wear <- daily_steps %>% left_join(weartimes_all, by=c("ID", "Date"))
  
  #check completeness
  daily_steps_wear <- daily_steps_wear %>% ungroup() %>% mutate(complete_day=weartime_R>=540) %>% 
    group_by(ID) %>% mutate(complete_week=sum(complete_day))
  
  #remove participants who don't have at least one complete day 
  daily_steps_wear <- daily_steps_wear %>% filter(complete_week>=1)
  
  
  #remove days which have less than 540 minutes 
  daily_steps_wear <- daily_steps_wear %>% filter(complete_day==TRUE)
  
  week_average <- daily_steps_wear %>% group_by(ID,mean_steps_B) %>% summarize(mean_steps=mean(daily_steps))
  modeltrue <- summary(lm(week_average$mean_steps~1))
  results <- modeltrue$coefficients[1:2] # estimate of mean and SE
  
  if(!is.null(cc_save_path)){
    saveRDS(week_average, paste0(cc_save_path, sim, ".RDS"))    
    
  }
  
  
  
  
  return(results)
  
}




get_missing_details <- function(nonwear, epoch_i){
  #Obtain details about nonwear or sleep extra periods: Date and Day of week, start and end times. 
  #set time to be 1970-01-01
  #split into additional nonwear periods if they go over a day
  
  if(nrow(nonwear)==0){
    nonwear_details <- NULL
  }else{
  
  start_time <-  as.POSIXct(nonwear$start_time, tz="UTC", origin="1970-01-01 UTC")
  start_day <- epoch_i[nonwear$start, "Day.of.Week"]$Day.of.Week
  start_date <- epoch_i[nonwear$start, "Date"]$Date
  
  end_time <- as.POSIXct(nonwear$end_time, tz="UTC", origin="1970-01-01 UTC") 
  end_day <- epoch_i[nonwear$end, "Day.of.Week"]$Day.of.Week
  end_date <- epoch_i[nonwear$end, "Date"]$Date
  
  
  nonwear_details <- data.frame(start_time=start_time, 
                                day=start_day, 
                                date=start_date,
                                end_time=end_time) 
  
  }
  
  
  
  #if they go over a day, split into additional periods 
  not_same <- nonwear$end_date != nonwear$start_date   #check if nonwear sleep goes to next day 

  if(sum(not_same)>0){
    
    additional <- nonwear_details[not_same,]
    
    #additional portion in the following day, starting just after midnight 
    additional$start_time <- rep(as.POSIXct("1970-01-01 00:00:05", tz="UTC"), sum(not_same))
    additional$day <- end_day[not_same]
    additional$date <- end_date[not_same]
    additional$end_time <- end_time[not_same]
    
    #make existing portion end just before midnight
    nonwear_details$end_time[not_same] <- rep(as.POSIXct("1970-01-01 23:59:55", tz="UTC"), sum(not_same))
    
    nonwear_details <- rbind(nonwear_details, additional)

  }
  
    return(nonwear_details)
}




get_sleep_window <- function(runs_details, epoch_i){
#get average sleep window given the person's data on the week 

    #get information on regular sleep  
    sleep <- runs_details%>% filter(type=="sleep")
    sleep_details <- cbind(epoch_i[sleep$start, c("Time")],
                           epoch_i[sleep$start, c("Date")],
                           epoch_i[sleep$start, c("Day.of.Week")], 
                           epoch_i[sleep$end, c("Time")], 
                           epoch_i[sleep$end, c("Date")],
                           epoch_i[sleep$end, c("Day.of.Week")] 
    )
    colnames(sleep_details) <- c("start_time", "start_date", "start_day", "end_time", "end_date", "end_day")
    
    #sleep start times - make date 1st jan 1970
    sleep_details$start_time <- as.POSIXct(sleep_details$start_time, tz="UTC", origin="1970-01-01 UTC")
    #assume intially that the sleep end time is the following day 
    sleep_details$end_time <- as.POSIXct(sleep_details$end_time,tz="UTC", origin="1970-01-01 UTC")+ as_hms("24:00:00")
    
    #if sleep start time is in the AM on the following day, add another 24h to calculate
    # the mean correctly
    condition <- sleep_details$start_time < as.POSIXct(as_hms("06:00:00"), tz="UTC", origin="1970-01-01 UTC")
    sleep_details$start_time[condition] <- sleep_details$start_time[condition] + as_hms("24:00:00")
   
    #add weekday/weekend variable 
     sleep_details <- sleep_details %>% 
      mutate(weekend = ifelse(end_day %in% c("Saturday", "Sunday"), 1, 0))
    
     #make the first start time and last end time NA, since these sleeps are usually incomplete. 
     if(nrow(sleep_details)>4){
       sleep_details$start_time[1] <- NA
       sleep_details$end_time[nrow(sleep_details)] <- NA
     }
    sleep_window <- sleep_details %>% group_by(weekend) %>%
      summarize(mean_start_time=mean(start_time, na.rm=T),
                mean_end_time=mean(end_time, na.rm=T))
    #if weekend has NAs - remove this row 
    if(sum(is.na(sleep_window %>% filter(weekend==1)))>0){
      
      sleep_window <- sleep_window %>% filter(weekend!=1)
    }
    
    #check if both is on 1970-01-02, if so, make both of them 1970-01-01
    checkboth <- date(sleep_window$mean_start_time)== "1970-01-02" & date(sleep_window$mean_end_time)== "1970-01-02"
    sleep_window$mean_start_time[checkboth] <- sleep_window$mean_start_time[checkboth]-as_hms("24:00:00")
    sleep_window$mean_end_time[checkboth] <- sleep_window$mean_end_time[checkboth]-as_hms("24:00:00")
    
    
    #if sleep window rolls into the next day, we can split the sleep window into two chunks. 
    nextday <- date(sleep_window$mean_end_time)-date(sleep_window$mean_start_time) ==1
    
    if(sum(nextday)>0){
      
      additional <- sleep_window[nextday,]
      
      #additional portion in the following day, starting just after midnight 
      additional$mean_start_time <- rep(as.POSIXct("1970-01-01 00:00:05", tz="UTC"), sum(nextday))
      additional$mean_end_time <- additional$mean_end_time  - as_hms("24:00:00")
      
      sleep_window$mean_end_time[nextday] <- rep(as.POSIXct("1970-01-01 23:59:55", tz="UTC"), sum(nextday))
      
      sleep_window <- rbind(sleep_window, additional) %>% arrange(weekend)
      
      #note - very occasionally, this ends up with creating an additional period that is only one epoch long - these are removed later. 
    }
    
    

    return(sleep_window)
}



nonpara_impute <- function(epoch_i, runs_details, donor_demog_i, weartimes_i, M=10, donor_dat, missing_dat, adj_base=NULL, pool_limit=4, output=TRUE){
  #purpose: do nonparametric imputation by identifying donor pools. Impute M times with replacement 
  # input: epoch_i: the epoch-level data
  #        runs_details: the output from calc_runs 
  #        donor_demog_i: the demographic information about the person: Age, BMI 
  #        weartimes_i: the weartimes of that person during the week 
  #        M, the number of imputations required 
  #        donor_dat: the donors available for imputation? #CHECK THIS 
  #        cov_pool: variance-covariance matrix for the donor pool 
  #
  #Output: the the n by M dataframe with M imputed values for the n stepcounts 
  #        a vector of length n providing the upper bound of step counts - to be used in parametric imputation 

      # obtain three sets of crucial information 
      #nonwear periods which need to be imputed 
      
      #get table of dates vs days of week----------------------------
      date_library <- epoch_i %>% select(Date, Day.of.Week) %>% distinct()
                      # this is important later 
  
      #Nonwear Details --------------------------------------------
      nonwear <- runs_details %>% filter(type=="nonwear")
     
      
      nonwear_details <- get_missing_details(nonwear, epoch_i)
     
      #days with less than no minutes of wear: assume no data add a full day to the nonwear-details.
      #if(!is.null(nonwear_details)){
      
          nonday <- weartimes_i[weartimes_i$weartime_R<10, "Date"]
          
          
          
          if(length(nonday)>0){
            nonwear_details <- rbind(nonwear_details, 
                                   data.frame(start_time=as.POSIXct(as_hms("00:00:05"), tz="UTC", origin="1970-01-01 UTC"), 
                                              day= unique(epoch_i[epoch_i$Date==nonday, "Day.of.Week"]$Day.of.Week), 
                                              date=unique(nonday), 
                                              end_time=as.POSIXct(as_hms("23:59:55"), tz="UTC", origin="1970-01-01 UTC")))
                                   
          
         }
          
      #}
      #Sleep extra details --------------------------------------------------
    
      long_sleep <- runs_details%>% filter(type=="sleep_extra")
      
          
      long_sleep_details <- get_missing_details(long_sleep, epoch_i) 
      
      
      #remove nonday from long sleep details to avoid double counting 
      if(!is.null(long_sleep_details) & length(nonday)>0){
        
        long_sleep_details <- long_sleep_details %>% filter(!(date %in% nonday))
      }
       
     
      
      sleep_imp <- NULL
      
      #Get more info on long sleep to do the imputation
      if (nrow(long_sleep)>0){
        
        #Get average sleep window 
        sleep_window <- get_sleep_window(runs_details, epoch_i)
        
        if(output==TRUE){
          cat("sleep window: \n")
          print(sleep_window)
        }
      
        
        if(nrow(sleep_window %>% filter(weekend==1))==0){ #not enough data to get sleep info on weekend - assume 1 hour extra sleep in the morning for weekend
         #indicator for weekend 
          sleep_window2 <- sleep_window
          sleep_window2$mean_end_time=sleep_window2$mean_end_time +as_hms("01:00:00")
          sleep_window2$weekend <- 1
          sleep_window <- rbind(sleep_window, sleep_window2)
          
        }
             
        #indicator for weekend 
        long_sleep_details$weekend <- ifelse(long_sleep_details$day %in% c("Saturday", "Sunday"), 1, 0)
        long_sleep_details$interval <- interval(long_sleep_details$start_time, long_sleep_details$end_time)
        sleep_window$interval <- interval(sleep_window$mean_start_time, sleep_window$mean_end_time)
        
        
        
        sleep_imp <- rbind(sleep_imp, find_imp(sleep_interval=sleep_window %>% filter(weekend==1), 
                                               long_sleep_interval=long_sleep_details %>% filter(weekend==1)))
        sleep_imp <- rbind(sleep_imp, find_imp(sleep_interval=sleep_window %>% filter(weekend==0), 
                                               long_sleep_interval=long_sleep_details %>% filter(weekend==0)))
        
        
      }
      
      #add all bits that need imputing----------------------------------------------------
      
      if(!is.null(nonwear_details )){
        impute <- rbind(nonwear_details %>% select("start_time", "day", "date", "end_time"), sleep_imp)
      }else{
        impute <- sleep_imp
      }
      
      #if there are some areas that need imputing that are just one epoch length, discard. 
      impute <- impute %>%
        filter(difftime(end_time, start_time, units="secs") >5)
      
      
      nimp <- nrow(impute)
      
      if(is.null(nimp)){
        #imputation interval not identified - so far, this has not happened. 
        
        cat("cannot identify appropriate imputation window \n")

        imputed_time=0
        imputed_steps = rep(sum(epoch_i$Steps), M)
        
      }else{
        #If imputation intervals are identified, then add information about them:
      
        
        impute$day <- factor(impute$day, levels= 
                               c("Monday", "Tuesday", "Wednesday", "Thursday","Friday", "Saturday", "Sunday"))
        
        #for parametric approach: obtain imputed time so we can work out an appropriate upper bound 
        
        impute <- impute %>% mutate(intervals=lubridate::interval(impute$start_time, impute$end_time, tz="UTC"))  #get intervals
        
        imputed_time <- impute %>% mutate(intervals=abs(as.numeric(intervals))/60) %>%  #imputed time in minutes
                        select(date, intervals) %>% 
                        group_by(date) %>% 
                        summarise(imputed_time=sum(intervals))  #get imputed time for each date
        
       
      #for each imputation period, find out which days are not eligible for donor pool - overlapping missingness 
      
      if(nimp==1){
        
          impute$nonimp_day<-  impute$day
          
        }else{
          
          for (i in 1:nimp){
        
          #can impute from other days with non-wear if the times do not overlap
          nonimp_day <- impute$day[!is.na(intersect(impute$intervals[i], impute$intervals))] #if they intersect, do not impute 
          
          #if(!(impute$day[i] %in% c("Saturday","Sunday")) & as_hms(impute$start_time[i]) <as_hms("12:00:00")) {   #if it's a weekday 
          #  nonimp_day <-  c(nonimp_day, "Saturday", "Sunday")   #make sure to exclude weekends from nonselfdonors
          #}
          
          
          
          impute$nonimp_day[i] <-  list(nonimp_day)
          
          
          }
          
      }
        
      alldays <- c("Monday", "Tuesday", "Wednesday", "Thursday","Friday", "Saturday", "Sunday")

      nonself_donor <- NULL   #this will be set to a specific person later if nonself donation needed 
      
      #store imputations here 
      impute <- cbind(impute, t(rep(0, M)))

      #print size of pool when self donation happens 
      self_pool_lengths <- c()
      #make decision on approach for each imputation interval, and impute. 
        for (i in 1:nimp){
         
            pool <- alldays[!(alldays %in% impute$nonimp_day[i][[1]])]  #donation pool
            
            
            if(length(pool)<=pool_limit){
              #Nonself Donation  
              if(output==TRUE){
                  cat("donor pool is insufficient \n")
              }
              nonself_intervals <- nonself_donor_interval(start_time=impute$start_time[i], end_time=impute$end_time[i], 
                                           donor_ID=nonself_donor, demog_i=donor_demog_i, donor_dat=donor_dat, missing_dat=missing_dat, 
                                           M, adj_base)
              
              impute[i, 7:(7+M-1)] <- nonself_intervals$steps   #imputed steps 
              nonself_donor <- nonself_intervals$donor_id   #get selected donor and use it later 
              
              
            }else{
              #Self donation 
              self_pool_lengths <- c(self_pool_lengths, length(pool))
              pool_dates <- date_library[date_library$Day.of.Week %in% pool, "Date"]$Date
              imp_portion <- epoch_i %>% filter(Date %in% pool_dates) 
              #put in the "standard" date and time zone 
              imp_portion$Time <- as.POSIXct(imp_portion$Time, tz="UTC", origin= "1970-01-01")
              
              
              pool_step <-imp_portion %>% group_by(Date) %>% 
                filter(Time > impute$start_time[i] & Time < impute$end_time[i]) %>%
                summarize(interval_step=sum(Steps))
              
              imp_step <- sample(pool_step$interval_step, M, replace=T)
              
              impute[i, 7:(7+M-1)] <- imp_step
              
          }
            
          
            
          }
          
          
          
        }
        
      
      if(output==TRUE){
           print(impute)
      }
      total_steps <- sum(epoch_i$Steps)
      imputed_steps <- total_steps + colSums(impute[,-c(1:6)])

      
      

return(list(Imputed_Steps=imputed_steps, Imputed_Time=imputed_time, Self_Pool_Lengths=self_pool_lengths))
}



################################

impute_and_analyse <- function(Simdata, sim, M=10, passive=log, path="upperbounds_sim_", imputations_save_path=NULL, missing_save_path=NULL, 
                               cc_save_path=NULL, adj_base=NULL, output=TRUE){
  
    IDs <- unique(Simdata$ID)
    n <- length(IDs)
    
    # Identify Donor Pool -----------------------------------------------------
    
    #get info on runs, missing runs, and weartimes for each day 
    runs_all <- c()
    missing_dat <- c()
    weartimes_all <- data.frame()

    for(i in 1:n){
      #for each patient 
      ID_i <- IDs[i]
      testperson <- Simdata[Simdata$ID==ID_i,]
      
      #don't perform calc_runs for patients with NO data 
      if(all(!is.na(testperson$Date))){
        
        
        runs_i <- calc_runs(testperson)[[1]]   #get runs info 
        runs_all <- rbind(runs_all, cbind(ID=ID_i, runs_i))
        runs_missing <- runs_i %>% filter(type %in% c("nonwear", "sleep-extra"))
        if(nrow(runs_missing)>0){
          missing_dat <- rbind(missing_dat, cbind(ID=ID_i, runs_missing))
        }        
        
      }
      
      
      
      dates <- unique(testperson$Date)   #compute weartime for each day 
      
      if(all(is.na(testperson$Date))){
        weartimes_all <- rbind(weartimes_all, data.frame(ID=ID_i, Date=rep(NA, 7), weartime_R=rep(0, 7)))
      }else{
      
      for (k in 1:length(dates)){
        testday <- testperson[testperson$Date==dates[k], ]
        weart <- calc_weartime(testday$VM)
        weartimes_all <- rbind(weartimes_all, data.frame(ID=ID_i, Date=dates[k], weartime_R=weart))
      }
      
      
      }
    }
    
    
    #get people with no sleep extra or nonwear
    nomiss <- runs_all %>% group_by(ID) %>% filter(sum(type=="sleep extra")==0 & sum(type=="nonwear")==0) %>% 
      select(ID) %>% distinct()
    
    #then get people who additionally have all days with at least 540min weartime
    donorpool_ID <- weartimes_all %>% filter(ID %in% nomiss$ID) %>% group_by(ID) %>% filter(sum(weartime_R>540)==7) %>%
      select(ID) %>% distinct()
    
    donorpool <- Simdata %>% filter(ID %in% donorpool_ID$ID)
    
    
    donor_dat_m <- donorpool %>% filter(Gender=="M")
    donor_dat_f <- donorpool %>% filter(Gender=="F")
    
    #print size of donor pool
    if (output==TRUE){
        print(donor_dat_m %>% select(ID) %>% distinct() %>% count())    
        print(donor_dat_f %>% select(ID) %>% distinct() %>% count())    
    }
    
    donor_dailymeans <- donorpool %>% group_by(ID) %>% summarise(meanstep=sum(Steps)/7)   #mean daily step counts 
    
    
    #Donor Pool Identified---------------------------------------------------------------------
    
  #store results 
  all_results <- data.frame(sim=rep(sim,3), Method=c("Ignore", "Complete Case", "Non-para"), 
                            mean=rep(NA,3), se=rep(NA,3))
  
    
  #analysis of data ignoring missingness 
  ignore_results <- simple_analysis(Simdata)  
  all_results[1, "mean"] <- ignore_results[1]
  all_results[1, "se"] <- ignore_results[2]
  
  if(!is.null(cc_save_path)){
    completecase_results <- cc_analysis(Simdata, weartimes_all, cc_save_path, sim)
    
    all_results[2, "mean"] <- completecase_results[1]
    all_results[2, "se"] <- completecase_results[2]
  }
  
  imputed_dataset <- data.frame()
  upperbounds <- data.frame()
  missing_all <- data.frame()
  
  all_self_pool_lengths <- c()
  for (i in IDs){
    
    if(output==TRUE){
        cat("**************************************************", i, "\n")
    }  
      
    #get info on this person 
    epoch_i <- Simdata[Simdata$ID==i, ]
    runs_i <- runs_all[runs_all$ID==i,]
    weartimes_i <- weartimes_all[weartimes_all$ID==i,]
    
    if("sleep_extra" %in% runs_i$type & "nonwear" %in% runs_i$type){
      missing_class <- "sleep_extra and nonwear"
    }else if("sleep_extra" %in% runs_i$type){
      missing_class <- "sleep_extra"
    }else if("nonwear" %in% runs_i$type){
      missing_class <- "nonwear"
    }else{
      missing_class <- "no missingness"
    }
    
    if(!is.null(adj_base)){
      demog_i <- epoch_i[1, c("ID", "Age", "Gender", "bmib", "mean_steps_B")]
                         
    }else{
          demog_i <- epoch_i[1, c("ID", "Age", "Gender", "bmib")]
    }
    
    
    #daily step counts with upper bounds 
    daily_steps <- epoch_i %>%
      select(ID, Date, Day.of.Week, Day_order, Steps) %>% 
      group_by(ID, Date, Day.of.Week, Day_order) %>% 
      summarize(raw_daily_step=sum(Steps)) %>%
      mutate(raw_daily_log= passive(raw_daily_step)) %>%
      mutate(upperbound=passive(raw_daily_step)) %>%
      mutate(upperbound_max =passive(raw_daily_step)) %>%
      mutate(ID=i) %>% relocate(ID)
    
    daily_steps[daily_steps==-Inf] <-0
    
    #total step counts
    total_steps <- sum(daily_steps$raw_daily_step)

    
    
    impute_run <- runs_i %>% filter(type %in% c("nonwear", "sleep_extra"))
    nonwear_run <- runs_i %>% filter(type %in% c("nonwear"))
    
   if (sum(weartimes_i$weartime_R<300)>5){
      #not enough data to do self donation, so impute the whole week using a nonself donor
     if(output==TRUE){
           cat("whole week imputation \n")
     }
      
     missing_type <- ">5 days"
     
      if(demog_i$Gender=="M"){
        imputedsteps <- nonself_donor_week(demog_i, donor_dat_m, M=M, adj_base, donor_dailymeans=donor_dailymeans)
      }else{
        imputedsteps <- nonself_donor_week(demog_i, donor_dat_f, M=M, adj_base,  donor_dailymeans=donor_dailymeans)
        
      }
     
     if(output==TRUE){
        cat(imputedsteps, "\n")
     }
      #upperbounds for all days are 10.5 
      daily_steps$upperbound <-passive(36315)
      daily_steps$upperbound_max <- passive(36315)
      
    } else if(nrow(impute_run)==0){
      
      if(output==TRUE){
            cat("no missingness \n")
        }
      missing_type <- "0 days"
      
      imputedsteps=rep(total_steps, M)
      
    }else{
      #enough data to do self donation, or a combination of self and nonself donation 
      missing_type <-"between 1 and 5 days"
      
      if(demog_i$Gender=="M"){
        nonpara_steps <- nonpara_impute(epoch_i=epoch_i, runs_details=runs_i, donor_demog_i=demog_i, weartimes_i, M=M, 
                                        donor_dat=Simdata %>% filter(ID!=i & Gender=="M"), missing_dat, adj_base, pool_limit=4, 
                                        output)
      }else{
        nonpara_steps <- nonpara_impute(epoch_i=epoch_i, runs_details=runs_i,  donor_demog_i=demog_i, weartimes_i, M=M, 
                                        donor_dat=Simdata %>% filter(ID!=i & Gender=="F"), missing_dat, adj_base, pool_limit=4, 
                                        output)
        
      }
      
      
      imputedsteps <- nonpara_steps[[1]]
      imputedtime <- nonpara_steps[[2]] 
      self_pool_lengths <- nonpara_steps[[3]]
      daily_steps[daily_steps$Date %in% imputedtime$date, "upperbound"]$upperbound <- passive(daily_steps[daily_steps$Date %in% imputedtime$date, "raw_daily_step"]$raw_daily_step + imputedtime$imputed_time*12*5)
      daily_steps[daily_steps$Date %in% imputedtime$date, "upperbound_max"]$upperbound_max <- passive(36315)
      
          all_self_pool_lengths <- c(all_self_pool_lengths, self_pool_lengths)

    }
    
    imputed_dataset <- rbind(imputed_dataset, data.frame(ID=i, raw_steps=as.numeric(total_steps), imputed_steps=as.numeric(imputedsteps), M=1:M))
    
    if (!is.null(path)){
      upperbounds <- rbind(upperbounds, daily_steps)
    }
    
    if (!is.null(missing_save_path)){
      missing_all <- rbind(missing_all, c(ID=i, missing_type=missing_type, missing_class=missing_class))
    }

    
  }
  
  
  if (!is.null(imputations_save_path)){
      saveRDS(imputed_dataset, paste0(imputations_save_path, sim, ".RDS"))
  }
  
  
  if (!is.null(missing_save_path)){
    saveRDS(missing_all, paste0(missing_save_path, sim, ".RDS"))
  }
  
  results_byM <- imputed_dataset %>% 
    group_by(M, ID) %>% 
    summarise(mean_perID = mean(imputed_steps/7)) %>%
    group_by(M) %>%
    summarize(mean=mean(mean_perID), var=var(mean_perID))
  
  num <- length(IDs)
  
  coefs <- as.list(results_byM$mean) 
  vars <- as.list(results_byM$var/num)
  results <- MIcombine(coefs, vars)
  all_results[3, "mean"] <-results$coefficients
  all_results[3, "se"] <- sqrt(results$variance)

  
  if (!is.null(path)){
    write.dta(upperbounds, paste0(path, sim, ".dta"))
  }
  
  return(all_results)
  
}


nonself_donor_week <- function(demog_i, donor_dat, M=10, adj_base=NULL, donor_dailymeans=NULL){
  #impute whole week with a non-self donor 
  
  donor_id <- unique(donor_dat$ID)
  
  if(!is.null(adj_base)){
    meanstep_12M <- donor_dailymeans %>% filter(ID %in% donor_dat$ID) 

    #donor_demog <- donor_dat %>% select(Age, bmib, ID, mean_steps_B, av_b_wear) %>% distinct() 
    donor_demog <- donor_dat %>% select(Age, bmib, ID) %>% distinct() %>% left_join(meanstep_12M, by="ID")
    
  }else{
    donor_demog <- donor_dat %>% select(Age, bmib, ID) %>% distinct()
  }
  
  
  cov_pool <-  cov(donor_demog %>% ungroup() %>% select(-ID))
  
  
  if(!is.null(adj_base)){
    #dist <- mahalanobis(donor_demog[,c("Age", "bmib", "mean_steps_B", "av_b_wear")], as.numeric(demog_i[, c("Age", "bmib", "mean_steps_B", "av_b_wear")]), cov_pool)
    
    dist <- mahalanobis(donor_demog[,c("Age", "bmib", "meanstep")], as.numeric(demog_i[, c("Age", "bmib", "mean_steps_B")]), cov_pool)
    
  }else{
    dist <- mahalanobis(donor_demog[,c("Age", "bmib")], as.numeric(demog_i[, c("Age", "bmib")]), cov_pool)
    
  }  
  
  inv_dist <- (1/dist)
  inv_dist[inv_dist=="Inf"]<-0
  weight <- inv_dist/sum(inv_dist)
  
  selected_donors <- sample(donor_id, M, prob=weight, replace=T)
  
  #get daily step counts for selected donors 
  imputed_daily <- donor_dat %>% filter(ID %in% selected_donors) %>% 
    group_by(ID, Day.of.Week) %>% summarize(dailystep=sum(Steps)) %>% #get daily sum step of imputed
    ungroup()
  
  
  daily_total <- c()
  
  for (i in 1:M){
    
    daily_dat <- imputed_daily %>% filter(ID == selected_donors[i])
    
    daily_dat_w <- imputed_daily %>% filter(!(Day.of.Week %in% c("Saturday", "Sunday")))
    daily_dat_we <- imputed_daily %>% filter(Day.of.Week %in% c("Saturday", "Sunday"))
    
    
    
    daily_step <- c(sample(daily_dat_w$dailystep, 5, replace=T), 
                    sample(daily_dat_we$dailystep, 2, replace=T))
    daily_total <- rbind(daily_total, data.frame(ID=selected_donors[i], M=i, meanstep=daily_step))
    
    
  }
  
  
  
  #daily_total <-  data.frame(ID=demog_i$ID,  M=1:M, meanstep=unlist(daily_dat)) 
  
  imputed_total <- daily_total %>% group_by(M) %>% summarize(tot_step=sum(meanstep)) 
  result <- imputed_total$tot_step
  
  
  
  
  return(result)
}



# # nonpara_steps <- nonpara_impute(epoch_i, runs_i, demog_i, weartimes_i, M=10, 
#                                 donor_dat_m %>% filter(ID!=i),
#                                 cov_m)

nonself_donor_interval <- function(start_time, end_time, 
                                   donor_ID=NULL,
                                   demog_i, donor_dat, missing_dat, M=10, adj_base=NULL){
  #impute interval with a non-self donor 
  
  #IDs not eligible for nonself donation since they have missingness in the same interval: 
  ineligible <- !is.na(intersect(interval(start_time, end_time), interval(missing_dat$start_time, missing_dat$end_time)))
  ineligible_IDs <- missing_dat[ineligible, "ID"]
  
  donor_dat <- donor_dat %>% filter(!(ID %in% ineligible_IDs))
  
  if(!is.null(adj_base)){
    donor_demog <- donor_dat %>% select(Age, bmib, ID, mean_steps_B) %>% distinct()
  }else{
      donor_demog <- donor_dat %>% select(Age, bmib, ID) %>% distinct()
  }
  
  covars <- ncol(donor_demog)
  cov_pool <- cov(donor_demog %>% ungroup() %>% select(-ID))
  
  #make sure cov_pool is invertible
  check_inverse <- try(solve(cov_pool),  silent=F)

  while(is.matrix(check_inverse)==FALSE){
    cov_pool <- cov(donor_demog %>%ungroup() %>% select(-ID)) + 0.00001*diag(covars-1)
    check_inverse <- try(solve(cov_pool),  silent=F)
  }

  if(is.null(donor_ID) & is.null(donor_dat)){
    cat("Error: need either a donor ID or need entire donor dat")
  }
  
  if(!is.null(donor_ID)){
    selected_donor=donor_ID
    selected_donor_dat <- donor_dat[donor_dat$ID==selected_donor,]
    selected_donor_dat$Time <- as.POSIXct(selected_donor_dat$Time,tz="UTC", origin= "1970-01-01") 
    interval_pool <- selected_donor_dat %>% group_by(Date) %>% filter(Time> start_time & Time < end_time) %>% summarise(steps=sum(Steps))
    selected_intervals <- sample(interval_pool$steps, M, replace=T)
    
    
  }else{
  
    
    if(!is.null(adj_base)){
      dist <- mahalanobis(donor_demog[,c("Age", "bmib", "mean_steps_B")], as.numeric(demog_i[, c("Age", "bmib", "mean_steps_B")]), cov_pool)
      
    }else{
            dist <- mahalanobis(donor_demog[,c("Age", "bmib")], as.numeric(demog_i[, c("Age", "bmib")]), cov_pool)

    }
    
      inv_dist <- 1/dist
      inv_dist[inv_dist==Inf] <- 0
      weight <- inv_dist/sum(inv_dist)
      interval_pool<- tibble(NULL)
      #select one donor
      while(nrow(interval_pool)==0){
        selected_donor <- sample(donor_demog[,"ID"]$ID, 1, prob=weight)
        
        selected_donor_dat <- donor_dat[donor_dat$ID==selected_donor,]
        selected_donor_dat$Time <- as.POSIXct(selected_donor_dat$Time,tz="UTC", origin= "1970-01-01") 
        interval_pool <- selected_donor_dat %>% group_by(Date) %>% filter(Time> start_time & Time < end_time) %>% summarise(steps=sum(Steps))
      }
      selected_intervals <- sample(interval_pool$steps, M, replace=T)
      
      
      
  }
  
  
  
  return(list(steps=selected_intervals, donor_ID=selected_donor))
}



