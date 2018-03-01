#Loading the required libraries#
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(data.table)
library(chron)
#---------------------------------------------------------------#
#Calling the raw file#
Uber_raw <- read.csv("Uber Request Data.csv",header = TRUE, sep = ",", as.is = TRUE)
str(Uber_raw)
#---------------------------------------------------------------#
#The raw file has different date and time formats in it, hence we will first#
#separate date and time and then make them into single format#
#using tidyr's separate function to create new columns requestdate, requesttime, dropdate & droptime#
#we have retained the original columns here#
Uber_raw <- separate(Uber_raw, Request.timestamp, c("requestdate","requesttime"),sep = " ", remove = FALSE)
Uber_raw <- separate(Uber_raw, Drop.timestamp, c("dropdate","droptime"),sep = " ", remove = FALSE)
#---------------------------------------------------------------#
#changing multiple formats of date and time to single format#
#using the parse date time function of lubridate package we are creating a single format for date and time#
uber_req_date <- as.data.frame(parse_date_time(x = Uber_raw$requestdate, orders = c("%d/%m/%Y","%d-%m-%Y")))
uber_drop_date <- as.data.frame(parse_date_time(x = Uber_raw$dropdate, orders = c("%d/%m/%Y","%d-%m-%Y")))
uber_req_time <- as.data.frame(parse_date_time(x = Uber_raw$requesttime, orders = c("HM","HMS")))
uber_drop_time <- as.data.frame(parse_date_time(x = Uber_raw$droptime, orders = c("HM","HMS")))
#---------------------------------------------------------------#
#Adding all the newly generated columns to make a new dataframe uber_raw_dt_add#
uber_raw_dt_add <- cbind(Uber_raw,uber_req_date,uber_req_time,uber_drop_date,uber_drop_time)
#---------------------------------------------------------------#
#Re- parsing the time column as default date of 01/01/1900 was added with the time columns#
#Four new columns were added namely requestdate1, requesttime1, dropdate1 & droptime1# 
uber_raw_dt_add <- separate(uber_raw_dt_add, 'parse_date_time(x = Uber_raw$requesttime, orders = c("HM", "HMS"))', c("requestdate1","requesttime1"),sep = " ", remove = TRUE)
uber_raw_dt_add <- separate(uber_raw_dt_add, 'parse_date_time(x = Uber_raw$droptime, orders = c("HM", "HMS"))', c("dropdate1","droptime1"),sep = " ", remove = TRUE)
#--------------------------------------------------------------#
#dropping or removing the columns that were created during parsing and are not required#
uber_raw_drop_colms <- uber_raw_dt_add[,-c(6,7,9,10,12,15)]
#renaming the derived columns to requestdate, requesttime, dropdate & droptime#
uber_raw_final <- setnames(uber_raw_drop_colms, old = c('parse_date_time(x = Uber_raw$requestdate, orders = c("%d/%m/%Y", "%d-%m-%Y"))','requesttime1','parse_date_time(x = Uber_raw$dropdate, orders = c("%d/%m/%Y", "%d-%m-%Y"))','droptime1'), new = c('requestdate','requesttime','dropdate','droptime'))
#--------------------------------------------------------------#
#using the weekdays function from data.table package we are adding the day of the week#
day_of_week_reqdate <- as.data.frame(weekdays(as.Date(uber_raw_final$requestdate)))
day_of_week_dropdate <- as.data.frame(weekdays(as.Date(uber_raw_final$dropdate)))
#--------------------------------------------------------------#
#adding the newly created day column to the main dataframe#
uber_raw_day_added <- cbind(uber_raw_final,day_of_week_reqdate,day_of_week_dropdate)
#--------------------------------------------------------------#
#using the chron package we are adding the time of the week#
uber_raw_day_added$requesttime <- chron(times = uber_raw_day_added$requesttime)
uber_raw_day_added$droptime <- chron(times = uber_raw_day_added$droptime)
#--------------------------------------------------------------#
#getting the hour part from the time columns using the sub string function#
request_time_hour <- as.data.frame(substr(uber_raw_day_added$requesttime, start = 1, stop = 2))
drop_time_hour <- as.data.frame(substr(uber_raw_day_added$droptime, start = 1, stop = 2))
#--------------------------------------------------------------#
#adding the newly created time columns to the main data frame#
uber_raw_hour_added <- cbind(uber_raw_day_added,request_time_hour,drop_time_hour)
#--------------------------------------------------------------#
#renaming the derived columns to requestday, requesthour, dropday & drophour#
uber_final <- setnames(uber_raw_hour_added, old = c('weekdays(as.Date(uber_raw_final$requestdate))','weekdays(as.Date(uber_raw_final$dropdate))','substr(uber_raw_day_added$requesttime, start = 1, stop = 2)','substr(uber_raw_day_added$droptime, start = 1, stop = 2)'), new = c('requestday','dropday','requesthour','drophour'))
#creating a csv file to use in tableau#
write.csv(uber_raw_hour_added,"Uber_final.csv", row.names = FALSE)
#----------------------END-OF-CODE-----------------------------#