library(hms)
library(stringr)


#
#       Functions
#
#

#This function finds the mean, median and the 1st and 3rd quartile of the data for every minute, 
#where the data have dates in the form "YYYY-MM-DD HH:MM:SS" "D-M-YYYY H:M"
XVarsData <- function(data){
  
  minutes <- 60     #Minutes in hour
  hours <- 24       #Hours in day
  day <- minutes*hours 
  daysInData <- dim(data)[1]/day   #This calculates the number of days in the data
  
  #Prepare the vectors 
  time <- integer(day)
  medians <- integer(day)
  averages <- integer(day)
  quart1 <- integer(day)
  quart3 <- integer(day)
  inds <- integer(day)
  
  #Put the time data into the desired format
  data$date_time <- GetTime(data$date_time)
  
  #Need to obtain the x variables for every minute of the day
  for (h in 1:hours-1){ #-1, to range from 0 to 23
    for (m in 1:minutes-1){ #Logic analogue to hours
      hms_code <- 60*m+60*60*h
      time_cur <- as_hms(hms_code)
      time_cur <- toString(time_cur)  #Stringify the time in the format "HH:MM:00"
      time_cur_s <- substr(time_cur,1,5)
      data_cur <- data$visits_index[data$date_time==time_cur_s]
      data_cur <- sort(data_cur) #Take the visits_index that have a corresponding time representation and sort (deletes NAs)
      data_quants <- quantile(data_cur)
      
      #Add the statistics to the vectors
      ind <- 1+(h*60)+m
      time[ind] <- time_cur
      medians[ind] <- median(data_cur)
      averages[ind] <- mean(data_cur)
      quart1[ind] <- data_quants[2]
      quart3[ind] <- data_quants[4]
    }
  }
  tog <- cbind(time,medians,averages,quart1,quart3)  #make dataframe of the variables combined
  tog_df <- as.data.frame(tog)
  
  tog_df$medians <- as.numeric(tog_df$medians)       #make the following variables numeric
  tog_df$averages <- as.numeric(tog_df$averages)     #opposed to characters
  tog_df$quart1 <- as.numeric(tog_df$quart1)
  tog_df$quart3 <- as.numeric(tog_df$quart3)
  
  return (tog_df)
}


Ad_locations <- function(data){
  
  data_dim = dim(data)[1]
  numbs <- 1:data_dim
  data["numbs"] <- numbs
  
  ad_ind <- data$numbs[data$ads==1]
  ad_start <- c()
  ad_end <- c()
  
  i <- 1
  ad_finished <- 1
  while(i<=length(ad_ind)){
    if(ad_finished==1){
      ad_start <- rbind(ad_start,ad_ind[i])
      i = i+7
      if (i>length(ad_ind)){
        ad_end <- rbind(ad_end,ad_ind[length(ad_ind)])
        break
      }
      if (ad_ind[i]-ad_ind[i-1]>1){
        ad_end <- rbind(ad_end,ad_ind[i-1])
        ad_finished <- 1
      } else{
        ad_finished <- 0
      }
    } else {
      i = i+1
      if (i>length(ad_ind)){
        ad_end <- rbind(ad_end,ad_ind[i-1])
        break
      }
      if (ad_ind[i]-ad_ind[i-1]>1){
        ad_end <- rbind(ad_end,ad_ind[i-1])
        ad_finished <- 1
      } else {
        ad_finished <- 0
      }
    }
  }
  combined <- as.data.frame(cbind(ad_start,ad_end))
  colnames(combined)<-c("start","end")
  return (combined)
}

#This function converts dates with time into HH:MM
GetTime <- function(time){
  time <- sub(".* ", "", time)     #Deletes the year/month/day
  time_s <- str_split(time[1],":") #Splits HH:MM:SS into separate parts
  if(length(time_s[[1]])>2){       #if we have 3 parts, then we need to take the HH:MM part
    times <- length(time)
    time_new <- integer(times)
    for (i in 1:times){
      time_new[i] = substr(time[i],1,5)
    }
  } else{                         #We only have 2 parts, so we already have the HH:MM format which we want
    time_new <- time
  }
  return (time_new)
}