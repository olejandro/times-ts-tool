# Copyright 2019 Energy Modelling Lab ApS
# Copyright 2020 Olexandr Balyk
# 
# This file is part of TIMES-TS-Tool.
# 
# TIMES-TS-Tool is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# TIMES-TS-Tool is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with TIMES-TS-Tool.  If not, see <https://www.gnu.org/licenses/>.


library(zoo)
library(xts)
library(timeDate)
source("write.R")


categorise_ts <- function(year, timezone = "UTC", hour_by_type = FALSE,
                          ts_defs = NULL){

  #definition of seasons
  sbm <- read.csv('input/dict/month-season.csv', stringsAsFactors = FALSE, encoding="UTF-8")

  #definition of days
  dbt <- read.csv('input/dict/day-type.csv', stringsAsFactors = FALSE, encoding="UTF-8")

  #Read a csv file with holidays
  holidays <- read.csv('input/dict/holidays.csv', stringsAsFactors = FALSE, encoding="UTF-8")
  
  #Keep holidays specific to the year
  holidays <- holidays[grepl(year,holidays$Date),]
  
  #Keyword for non-work days, i.e. holidays / forced holidays, etc.
  nonWorkday <- "NW"

  #!Due to summer time, starting and ending hours of week-ends would be classified differently
  #!compared to the original TS tool. Nothing special is done to time shift in Ramses, 
  #!because all the series are in the same time zone. Therefore UTC is used to avoid the shift.

  #create hourly index for the whole year, assuming UTC time zone
  time_index <- seq(from = as.POSIXct(paste(year,"01-01 00:00",sep="-"), tz=timezone),
                    to = as.POSIXct(paste(year,"12-31 23:00",sep="-"), tz=timezone), by = "hour")

  #create a data frame to categorise hours
  ts_cats <- xts(data.frame(matrix(ncol=3, nrow=length(time_index))),order.by = time_index)
  colnames(ts_cats) <- c("Season","Day","Hour")

  #Replace NA with season value
  for (aMonth in sbm$Month)
    {
    index <- which(.indexmon(ts_cats)==aMonth)
    ts_cats$Season[index] <- sbm[sbm[,"Month"] == aMonth,"Season"]
    }

  #Replace NA with day value
  if (nrow(dbt) == 31){
    for (aDay in dbt$Day)
    {
      index <- which(.indexmday(ts_cats)==aDay)
      ts_cats$Day[index] <- dbt[dbt[,"Day"] == aDay,"Type"]
    }
  }else{
    for (aDay in dbt$Day){
      index <- which(.indexwday(ts_cats)==aDay)
      ts_cats$Day[index] <- dbt[dbt[,"Day"] == aDay,"Type"]
    }
    }

  #Check whether there is inconsistency between Day/Type definition and NW assignment

  if(nonWorkday %in% dbt$Type)
    {
    #Include holidays as a non-work day
    ts_cats$Day[holidays$Date] <- nonWorkday
    }                                            
  
  #Assign the hour type
  hbt <- read.csv('input/dict/hour-type.csv', stringsAsFactors = FALSE, encoding="UTF-8")
    
  #Replace NA with hour value
  for (anHour in hbt$Hour)
    {
    index <- which(.indexhour(ts_cats)==anHour)
    ts_cats$Hour[index] <- hbt[hbt[,"Hour"] == anHour,"Type"]
    }

  # Update ts definition
  if (!is.null(ts_defs)) {
    
    DayNite <- hbt
    
    update_ts_defs(ts_defs,coredata(ts_cats),sbm,dbt,DayNite)
  }

  return(coredata(ts_cats))
  }