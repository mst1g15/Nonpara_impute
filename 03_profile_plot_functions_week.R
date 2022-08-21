#---------------------------------------------------------
#Purpose: Functions that create profile plots across the week 
#          
#Date: 25 June 2021
#-----------------------------------------------------------

spike_tol <- function(nonwear_runs, intervals, n){
  #input: nonwear_runs: dataframe with start, end and length of nonwear runs 
  #       intervals: lengths of time between non-wear periods in minutes 
  #       n: number of nonwear periods 
  
  
  index <- which(intervals<=2)  #find intervals that last less than 2 minutes (spikes)
  index_init <- cbind(begin=index, end=index+1)  #create start and end of these spikes
  
  keep <- which(!(1:n %in% c(index_init))) #these intervals are not spikes - keep them separate. 
  
  #sometimes, there will be multiple spikes within a nonwear period. We need the final 
  #nonwear period to ignore all these spikes. 
  #we do this by removing repeated entries in the index_init. (end and start will be the same if we have adjacent spikes)
  rep_remove <- rle(sort(index_init))$values[rle(sort(index_init))$lengths==1]
  index_final <- matrix(rep_remove, ncol=2, byrow = T)
  
  
  nonwear_runs_keep <-nonwear_runs[keep,]
  
  #nonwear with spikes removed 
  nonwear_runs_nospike<- data.frame(start=nonwear_runs$start[index_final[,1]],
                                end=nonwear_runs$end[index_final[,2]],
                           length=(nonwear_runs$end[index_final[,2]]-nonwear_runs$start[index_final[,1]])*5/60) #length in minutes
  #combine keep and nonwear with spikes removed 
  all <- rbind(nonwear_runs_keep, nonwear_runs_nospike)
  all <- all[order(all$start),]
  return(all)
}



check_inactive <- function(data, start, end, length, threshold=600, int=12){
  #purpose: check whether a period of zero VM should be classified as inactive 
  #         (people are just taking a nap or watching TV) or they have actually removed
  #         their device, which can be identified by a large VM on removal/putting on device
  #Input:  data: vector of VM 
  #        start: the epoch number indicating when the period starts
  #        end: the epoch number indicating when the period ends 
  #        threshold: the VM which is the threshold for identifying when people put on/off device
  #        interval: the interval of epochs to search for this threshold value.
  
  if( start<=int | (length(data)-end) <= 12 ){   #if the non-wear is at the start of the week or end of the week
    type="sleep"
  }else{
    
    if(length>120){   #if longer than 2 hours, nonwear.
      type="nonwear"    
    }else{
  
    check_left <- sum(data[(start-int):start]>threshold)>0     #check if there's any VM exceeding the threshold within interval
    check_right <- sum(data[end:(end+int)]>threshold)>0           #check if there's any VM exceeding the threshold within interval
  
    removal <- check_left | check_right         #if either left or right as some threshold-exceeding VM .... 
  
  #TODO: include some check of how long this is? 
  
    type <- ifelse(removal==T, "nonwear" ,"inactive")   #.... change "nonwear" to "inactive"
    }
  }
  
  return(type)
}



add_active <- function(data){
  #purpose: add periods in between non-wear as "active" -- mostly helpful to identify active periods from which to compute the
  #          average activity window 
  #Input:  data: dataset detailing the start, end, length and type of inactive periods. 
 
  n <- nrow(data) 
  add <- data.frame(start=NULL, end=NULL, length=NULL, type=NULL)   #this will be filled in 
  
  for (i in 1:(n-1)){
    if(data$end[i] != data$start[i+1]+1) {
      
      add <- rbind(add, data.frame(start=data$end[i]+1, 
                                   end=data$start[i+1]-1, 
                                   length=(data$start[i+1]-1 - data$end[i]+1)*5/60, 
                                   type="active"))   # add in betweens 
      
    }
  }
  
  # #add last period  ---------- don't think this is needed and seems to create negatives
  # if(data$end[n] != 120948){
  #   add <- rbind(add, data.frame(start=data$end[n] + 1, 
  #                                end=120948, 
  #                                length= 120948 - (data$end[n] + 1), 
  #                                type="active"))
  # }
  # 
  all_runs <- rbind(data, add) %>% arrange(start)
  

  return(all_runs)
  
}





calc_runs <- function(data_week, sleep_time=5, sleep_extra=15){
    #Purpose: identify runs of zero VM epochs longer than 60 minutes
    #          but shorter than 5 hours (which we assume is sleep?)
   
    #Input: data is the VM for a particular ID on a particular day 
  
    #Output: dataframe where each row is a non-wear period. 
    #         Start, end and length of period is provided 
   
  data <- data_week$VM
   nonwear_runs <- with(rle(data==0), data.frame(nonwear=values,    #either TRUE (run of zeros) or FALSE (run of non-zeros)
                        start=cumsum(lengths)-lengths+1,    #get start of run
                        end=cumsum(lengths),                #end of run
                        length=lengths*5/60)) %>%           #length of run in minutes 
          filter(nonwear==TRUE) %>% 
          filter(length>60)  %>%     #get periods longer than 60 min 
          select(-nonwear)
   
   n <- nrow(nonwear_runs)
   
   
   #if you have at least two non-wear periods: 
   if(n>1){
      intervals <- (nonwear_runs$start[2:n] -nonwear_runs$end[1:(n-1)])*5/60    #get time between nonwear-periods in minutes 
     
      
      if (sum(intervals < 2) >0 ) {    #if you have spikes
        nonwear_nospike <- spike_tol(nonwear_runs, intervals, n)  #disregard spikes with spike_tol function 
      }else{  #if no spikes 
        nonwear_nospike <- nonwear_runs
      }
      
   }else{
     nonwear_nospike <- nonwear_runs
   }
    
   #distinguish between sleep and non-wear periods 
   nonwear_nospike <- nonwear_nospike %>%
     #mutate(wear_start=data$Weartime_R[start], 
      #      wear_end=data$Weartime_R[end]) %>%
     mutate(type=case_when( length>60*sleep_extra  ~ "sleep_extra", 
                            length>=60*sleep_time & length<60*sleep_extra~ "sleep", 
                            length<60*sleep_time ~"nonwear"))
   
   
   #check whether the nonwear periods are genuinely nonwear or inactive
   nonwear_rows <- which(nonwear_nospike$type=="nonwear")   # 
   
   for ( i in nonwear_rows){
     nonwear_nospike[i, "type"] <- check_inactive(data, nonwear_nospike[i, "start"], nonwear_nospike[i, "end"], nonwear_nospike[i, "length"])
   }
   
   #add active periods 
   
   if (nrow(nonwear_nospike)>1){
     all_runs <- add_active(nonwear_nospike)
   }else{
     all_runs <- nonwear_nospike
   }
   
   #add times 
   all_runs$start_date <- (data_week[all_runs$start,]$Date)
   all_runs$start_time <- data_week[all_runs$start,]$Time
   
   all_runs$end_date <- (data_week[all_runs$end,]$Date)
   all_runs$end_time <- data_week[all_runs$end,]$Time
   
   
   
   #calculate active window: 
   # nonwear_date <- data_week %>% filter(Weartime_R<540) %>% select(Date) %>% distinct()
   #  active_dat <-  all_runs %>%
   #    filter(type=="active") %>%
   #    filter(!((start_date) %in% nonwear_date$Date)) %>%    #ignore days that have non-wear or less than 540 weartime 
   #    filter(!(end_date %in% nonwear_date$Date)) %>%
   #    filter(length>=540)
   #  k <- nrow(active_dat)
   #  if (k>1){
   #      start_time_convert <- as.POSIXct(active_dat$start_time[1:(k-1)])
   #      start_dat <- cbind(active_dat[1:(k-1), c("start_date", "end_date")], start_time_convert)
   #      mean_start <- as_hms(mean(start_time_convert))
   #      #there can be NAs here if the device gets switched off permaturely.
   #      late <- ifelse( na.omit(active_dat$end_time[2:k]) < as_hms("12:00:00"), 86400, 0)  #if the end of the window is the next day, have to account for this.
   #      end_time_convert <- na.omit(as.POSIXct(active_dat$end_time[2:k])) +late
   #      end_dat <-  cbind(na.omit(active_dat[2:k, c("start_date", "end_date")]), end_time_convert)
   #      mean_end <- as_hms(mean( end_time_convert))
   # 
   #      if(mean_end < as_hms("06:00:00")) {
   #        average_active <- cbind(Date=nonwear_date, start_time=mean_start, end_time=as_hms("23:59:59"), Time=0, VM=0)
   #        average_active <- rbind(average_active, cbind(Date=nonwear_date, start_time=as_hms("00:00:00"), end_time=mean_end, Time=0, VM=0))
   #        
   #      }else{
   #        average_active <- cbind(start_time=mean_start, end_time=mean_end)
   #      }  
   #  
   #  }else{
   #    average_active <- NULL
   #    start_dat <- NULL
   #    end_dat <- NULL
   #  }
   

    return(list(all_runs))#, average_active, start_dat, end_dat))
   
}
#------------------------------------------------------

#Function for plotting nonwear/sleep/inactive lines 
make_lines <- function(segments){
  #purpose: split nonwear segments across days so they can be plotted in the weekly plots
  #input: segments is the first output from calc_runs which provides start and end dates/times of each non-wear period 
  
  #identify segments that span more than one day 
  not_within_date <- which(as.character(segments$start_date)!=as.character(segments$end_date))
  
  within_date <-which(as.character(segments$start_date)==as.character(segments$end_date))
  
  #add segments contained within one day 
  add0 <- segments[within_date,] %>%
    select(-c(start, end, end_date)) %>%
    rename(Date=start_date)
  
  #add start until end of day 
  add1 <- segments[not_within_date,] %>% select(Date=start_date, start_time, length, type) %>%
    mutate(end_time = as_hms(c("23:59:55")))  #not sure if the as_hms should be removed 
  
  #add beginning of next day until end of segment 
  add2 <- segments[not_within_date,] %>% select(Date=end_date, end_time, length, type) %>%
    mutate(start_time = as_hms(c("00:00:00")))   #not sure if the as_hms should be removed 
  
  plot_segments <- rbind(add0, add1, add2)
  
  return(plot_segments)
}





plot_week <- function(testperson, recomp_wear=F){
  #plot the whole week 
  testperson$Time <- as_hms(as.character(testperson$Time))
  #re-calculate wear times if needed for plot labels (relevant for simulations with induced missingness)
  if(recomp_wear==T){
    days <- unique(testperson$Date)
    weartime_days <- data.frame(Date=days, weartime_new=sapply(Date, function(x){calc_weartime(testperson[testperson$Date==x, "VM"]$VM)}))
    testperson <- left_join(testperson, weartime_days, by="Date")
    testperson$Date <- factor(testperson$Date, labels=unique(paste(substr(testperson$Day.of.Week, 1, 3), 
                                                                   testperson$Date, 
                                                                   "   Wear time =", round(testperson$weartime_new, 2))))
  }else{
    #otherwise, use originally calculated wear times 
    testperson$Date <- factor(testperson$Date, labels=unique(paste(substr(testperson$Day.of.Week, 1, 3), 
                                                                   testperson$Date, 
                                                                   "   Wear time =", round(testperson$Weartime_R, 2))))
  }
  
  
  testperson_nonwear <-calc_runs(testperson)
  nonwear <- make_lines(testperson_nonwear[[1]])
  #nonwear_wrap <- testperson_nonwear[[2]]
  #current_start_time <- left_join(testperson_nonwear[[3]], testperson %>%select("ID", "Date", "Day.of.Week", "Weartime_R") %>% distinct(), 
  #                         by=c("start_date"="Date"))
  
  #start_times <-  rbind(start_times, current_start_time)
  
  #current_end_time <- left_join(testperson_nonwear[[4]], testperson %>%select("ID", "Date", "Day.of.Week", "Weartime_R") %>% distinct(), 
  #                         by=c("start_date"="Date")) 
  #end_times <-  rbind(end_times, current_end_time)
  
  cols <- c("active"="black", 
            "inactive"="grey",
            "nonwear"="red",
            "sleep"="royalblue", 
            "sleep_extra"="purple"
            )
  
  
  sleep_seg <- nonwear %>%
    filter(type=="sleep") 
  
  
  sleep_extra_seg <- nonwear %>%
    filter(type=="sleep_extra") 
  
  
  nonwear_seg <- nonwear %>%
    filter(type=="nonwear")
  
  inactive_seg <- nonwear %>%
    filter(type=="inactive")
  
  p <- ggplot(data=testperson, aes(Time, VM, color="active")) +
    geom_point()  + ylim(0, 1900) +
    geom_segment(data=sleep_seg, aes(x = start_time , y = 0, 
                                     xend =end_time, yend=0, colour="sleep"), size=2)+
    facet_wrap(~Date,scales="free_y", ncol=1, labeller=label_value)
  
  if(nrow(nonwear_seg)>0){
    
    p <- p+ geom_segment(data=nonwear_seg, aes(x = start_time , y = 0, 
                                               xend =end_time, yend=0, color="nonwear",), size=2)
  }
  
  
  
  if(nrow(inactive_seg)>0){
    p <- p+  geom_segment(data=inactive_seg, aes(x = start_time , y = 0, 
                                                 xend =end_time, yend=0, colour="inactive"), size=2,
                          )}
  
  if(nrow(sleep_extra_seg)>0){
    p <- p+  geom_segment(data=sleep_extra_seg, aes(x = start_time , y = 0, 
                                                    xend =end_time, yend=0, colour="sleep_extra"), size=2,
                          )}
  
  p <- p + scale_color_manual(name="", values=cols) + theme(legend.position="bottom")
}


#Functions to identify non-wear epochs (that are not sleep)--------------------

calc_weartime <- function(data){
  #Purpose: identify runs of zero VM epochs longer than 60 minutes
  #          but shorter than 5 hours (which we assume is sleep?)
  
  #Input: data is the VM for a particular ID on a particular day 
  
  #Output: dataframe where each row is a non-wear period. 
  #         Start, end and length of period is provided 
  
  nonwear_runs <- with(rle(data==0), data.frame(nonwear=values,    #either TRUE (run of zeros) or FALSE (run of non-zeros)
                                                start=cumsum(lengths)-lengths+1,    #get start of run
                                                end=cumsum(lengths),                #end of run
                                                length=lengths*5/60)) %>%           #length of run in minutes 
    filter(nonwear==TRUE) %>% 
    filter(length>60)  %>%     #get periods longer than 60 min 
    select(-nonwear)
  
  n <- nrow(nonwear_runs)
  
  
  #if you have at least two non-wear periods: 
  if(n>1){
    intervals <- (nonwear_runs$start[2:n] -nonwear_runs$end[1:(n-1)])*5/60    #get time between nonwear-periods in minutes 
    
    
    if (sum(intervals < 2) >0 ) {    #if you have spikes
      nonwear_nospike <- spike_tol(nonwear_runs, intervals, n)  #disregard spikes with spike_tol function 
    }else{  #if no spikes 
      nonwear_nospike <- nonwear_runs
    }
    
  }else{
    nonwear_nospike <- nonwear_runs
  }
  
  
  if(nrow(nonwear_nospike)==0){   #can happen when there is no wear, or if the person wore it non-stop
    if (sum(data)>1000){  #the person wore it non-stop
      weartime <- 1440
    }else{
      weartime <- 0
    }
  }else{
    weartime <- 1440 - sum(nonwear_nospike$length)
  }  
    
  
  if(weartime <0){
    weartime <- 0
  }
  
  return(weartime)
}

