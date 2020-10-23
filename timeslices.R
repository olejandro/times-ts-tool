# Copyright 2019 Energy Modelling Lab ApS
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
source("funcs.R")
discretize <- arules::discretize


categorise_ts <- function(aggr_data, year, timezone = "UTC", hour_by_type = FALSE,
                          syssettings = NULL){

  #read criterias used to define time slices
  brk_lvls <- read.csv('input/breaks-season.csv', stringsAsFactors = FALSE, encoding="UTF-8")

  #definition of seasons
  sbm <- read.csv('input/dict/month-season.csv', stringsAsFactors = FALSE, encoding="UTF-8")

  #definition of days
  dbt <- read.csv('input/dict/day-type.csv', stringsAsFactors = FALSE, encoding="UTF-8")

  #Read a csv file with holidays
  holidays <- read.csv('input/dict/holidays.csv', stringsAsFactors = FALSE, encoding="UTF-8")
  #Keep holidays specific to the year
  holidays <- holidays[grepl(year,holidays$Date),]

  #Definition of hours by level
  hbl <- read.csv('input/dict/hour-map.csv', stringsAsFactors = FALSE, encoding="UTF-8")
  labels = c("low","medium","high")

  #Definition of hours
  h_order <- read.csv('input/dict/order.csv')

  #!Due to summer time, starting and ending hours of week-ends would be classified differently
  #!compared to the original TS tool. Nothing special is done to time shift in Ramses, 
  #!because all the series are in the same time zone. Therefore UTC is used to avoid the shift.

  #create hourly index for the whole year, assuming UTC time zone
  time_index <- seq(from = as.POSIXct(paste(year,"01-01 00:00",sep="-"), tz=timezone),
                    to = as.POSIXct(paste(year,"12-31 23:00",sep="-"), tz=timezone), by = "hour")

  #add hourly index to time series
  aggr_data <- xts(aggr_data, order.by = time_index)

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
  for (aDay in dbt$Day)
    {
    index <- which(.indexwday(ts_cats)==aDay)
    ts_cats$Day[index] <- dbt[dbt[,"Day"] == aDay,"Type"]
    }

  #Check whether there is inconsistency between Day/Type definition and NW assignment

  if("NW" %in% dbt$Type)
    {
    NW <- "NW"
    } else
      {
        print("Please specify a consistent label for a holiday!")
        exit
        }                                            

  #Include holidays as a non-work day
  ts_cats$Day[holidays$Date] <- NW
  
  #Include additional days as non-work days
  #The day after Kristi himmelfartsdag
  ts_cats$Day[as.character(as.Date(holidays[holidays[,"Holiday"]
                                          == "Kristi himmelfartsdag","Date"])+1)] <- NW
  #23.12-31.12
  ts_cats$Day[paste(paste(year, "12-23", sep = "-"),
                    paste(year, "12-31", sep = "-"), sep = "/")] <- NW

  #calculate means per column
  means <- apply(aggr_data, 2, mean)

  #calculate level intervals
  for (season in unique(brk_lvls$Season))
    {
    for (i in names(means)) 
      {
      brk_lvls[brk_lvls[, "Season"] == season,i] <- 
        brk_lvls[brk_lvls[, "Season"] == season,i] * means[i]
      }
    }

  #categorise data points in time series
  #create a dataframe to hold the categories
  ts_levels <- data.frame(matrix(nrow = length(time_index), ncol = ncol(aggr_data)))
  colnames(ts_levels) <- colnames(aggr_data)
  ts_levels <- xts(ts_levels, order.by = time_index)

  #fill in the data frame with categories
  for (serie in colnames(aggr_data)) 
    {
    for (season in unique(brk_lvls$Season))
      {
      ts_levels[ts_cats$Season == season, serie] <- as.character(
        discretize(aggr_data[ts_cats$Season == season, serie],
                   breaks=brk_lvls[brk_lvls$Season == season, serie],
                   method = "fixed", labels = labels, right = TRUE))
      }
    }

  #Create a dataframe for filling in combinations of criteria for hours (i.e. because of "all")
  hbl_full <- data.frame(matrix(nrow=length(labels)^ncol(hbl[-1])*length(hbl$Hour),
                                ncol=ncol(hbl)))

  colnames(hbl_full) <- colnames(hbl)

  chunk <- length(labels)^ncol(hbl[-1])
  lvls <- length(labels)

#fill in the data frame with values
for (r in 1:nrow(hbl))
{
  hbl_full$Hour[(1+chunk*(r-1)):(chunk*r)] <- hbl$Hour[r]
  for(c in 1:ncol(hbl[-1]))
  {
    if (hbl[r,c+1] %in% labels) {
      for (i in (1:lvls^(ncol(hbl[-1])-c))) 
      {
        hbl_full[((chunk*(r-1)+1+(i-1)*lvls^c):
                    (chunk*(r-1)+((i-1)*lvls+1)*lvls^(c-1))),(c+1)] <- hbl[r,c+1]
      }
    } else {
      for (i in (1:lvls^(ncol(hbl[-1])-c))) 
      {
        for (lbl in labels)
        {
          shift <- match(lbl,labels)-1
          hbl_full[((chunk*(r-1)+1+shift*lvls^(c-1)+(i-1)*lvls^c):
                      (chunk*(r-1)+((i-1)*lvls+1)*lvls^(c-1)+shift*lvls^(c-1))),(c+1)] <- lbl
        }
      }
    }
  }
}

  #Remove rows where NA is present
  hbl_full <- hbl_full[complete.cases(hbl_full), ]

  #Remove duplicate entries (keep first)
  hbl_full <- hbl_full[rownames(unique(hbl_full[-1])),]

  #Reset the data frame index
  rownames(hbl_full) <- 1:nrow(hbl_full)

  #categorise hours by type
  #Time index is added as a column Date, so that order of the hours can be recreated. 
  temp <- merge(data.frame(Date=as.numeric(index(ts_levels)), coredata(ts_levels)),
                hbl_full,by=colnames(ts_levels), x.all=TRUE)

  #Assign the hour type
  if (hour_by_type)
    {
    #definition of hours by type
    hbt <- read.csv('input/dict/hour-type.csv', stringsAsFactors = FALSE, encoding="UTF-8")
    
    #Replace NA with hour value
    for (anHour in hbt$Hour)
      {
      index <- which(.indexhour(ts_cats)==anHour)
      ts_cats$Hour[index] <- hbt[hbt[,"Hour"] == anHour,"Type"]
      }
    } else 
      {
        ts_cats$Hour <- temp$Hour[order(temp$Date, decreasing = FALSE)]
      }
  
  # Update SysSettings
  if (!is.null(syssettings)) {
    if (hour_by_type) {
      DayNite <- hbt
    } else {
      DayNite <- hbl
    }
    update_syssettings(syssettings,coredata(ts_cats),sbm,dbt,DayNite)
  }

  return(coredata(ts_cats))
  }