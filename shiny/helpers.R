library(ggplot2)
library(tidyverse)
library(dplyr)
library(lubridate)
library(forcats)

readData <- function(fileName){
  df = read_csv(fileName) # 201806-df-tripdata.csv
  
  df <- df %>% mutate(minute=tripduration/60)
  df <- subset(df,minute<120 & minute>0)
  
  colnames(df)[colnames(df)=="start station longitude"] <- "start_long"
  colnames(df)[colnames(df)=="start station latitude"] <- "start_lat"
  colnames(df)[colnames(df)=="start station name"] <- "start_name"
  colnames(df)[colnames(df)=="start station id"] <- "start_id"
  colnames(df)[colnames(df)=="end station name"] <- "end_name"
  
  startCount <- df %>% 
    group_by(`start_id`) %>% 
    count()  %>% arrange(desc(n))
  
  
  df1 = merge(df, startCount, "start_id")
  df1$n <- as.character(df1$n)
  #stations <- unique(df1$start_name)
  return (df1)
}

stationTimeSlot <- function(df, start, startName){
  df1 <- df %>% subset(start_id == start)
  df_timeslot <- mutate(df1,hour = hour(as.POSIXct(starttime)))
  timeslot <- ggplot(data=df_timeslot,mapping=aes(as.factor(hour))) + 
                geom_bar(color='Black',fill='Orange') + 
                ggtitle(sprintf("Usage Statistics of Different Time slot of \"%s\"", startName)) + xlab("Hour")
  
  return(timeslot)
}

nameTopEnd <- function(df,start,k){
  df1 <- df %>% subset(start_name == start)
  
  endstations <- df1 %>% 
    group_by(end_name) %>% 
    summarize(Freq=n()) %>% arrange(desc(Freq))
  
  stations <- top_n(endstations,k)$end_name #top k end stations
  #print(stations)
  df2 <- df1 %>%
    filter(end_name %in% stations)
  
  g <- ggplot(df2, aes(x = fct_rev(fct_infreq(end_name)),fill = usertype)) + 
    geom_bar(position = "dodge") + xlab("end station")
  #coord_flip()
  title <- df1$start_name
 
  #g <- g + ggtitle('Top Destination From' + ))
  g <- g + ggtitle(sprintf("Top %d Destination From \"%s\"", k, start))
  g <- g + coord_flip()
  return(g)
}
  
plotTimeDuration <- function(df,start,end){
  df1 <- df %>% subset(start_name==start & end_name == end)
  g <- ggplot(data=df1,mapping=aes(x=as.factor(gender),y=minute,color=as.factor(gender))) +
        geom_boxplot() +
        labs(title = "Distribution on time duration (minute) on this trip\n", x = "Gender", y = "Minutes", color = "Gender\n") +
        scale_color_manual(labels = c("Unknown", "Male", "Female"), values = c("red","blue","green"))
  g <- g + coord_flip()
  return(g)
}


