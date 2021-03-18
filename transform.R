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


library(dplyr)
library(tidyr)

#Combine the season/day/hour categories to Label all the hours
map_ts <- function(ts_cats)
  {
  ts_cats <- as.data.frame(ts_cats)
  DayNite_level <- paste(ts_cats$Season,ts_cats$Day,ts_cats$Hour, sep="") 
  Weekly_level <- paste(ts_cats$Season,ts_cats$Day, sep="")
  Season_level <- as.character(ts_cats$Season)
  ts_map <- cbind(DayNite_level,
                  Weekly_level,
                  Season_level)
  colnames(ts_map) <- c("DayNite","Weekly","Season")
  return(ts_map)
  }

#Number of the same hour type
ts_duration <- function(ts_hour)
  {
  ts_count <- as.data.frame(summary.factor(ts_hour))
  ts_duration <- ts_count %>%
    rename(TS_Duration=1) %>%
    mutate(TimeSlice=row.names(.),
      TS_Duration=TS_Duration/sum(TS_Duration))
  return(ts_duration)
  }

ts_yrfr_data <- function(ts_hour,regions)
  {
  yrfr_data <- ts_duration(ts_hour) %>%
    merge(regions) %>%
    rename(Region=3) %>%
    mutate(Attribute = "YRFR") %>%
    pivot_wider(names_from = Region, values_from = TS_Duration)
  return(yrfr_data)
  }

transform_data <- function(dataset,transformation,serie,ts_hour)
  {
  # Determine operations to perform on the series
  operations <- unlist(strsplit(transformation, "_"))
  adjustment <- operations[1]
  aggregation <- operations[2]
  
  # Perform timeserie aggregation
  if (aggregation=="avg")
    {
    aggr_serie <- ts_cat_avg(serie,ts_hour)
    } else if (aggregation=="afa")
      {
      aggr_serie <- ts_cat_afa(serie,ts_hour)
      } else if (aggregation=="shr")
        {
        aggr_serie <- ts_cat_shr(serie,ts_hour)
        } else if (aggregation=="sbd")
          {
          aggr_serie <- ts_cat_sbd(serie,ts_hour)
          }
  
  # Add timeslice duration info
  aggr_serie <- merge(aggr_serie,ts_duration(ts_hour))
  dataset <- merge(dataset,aggr_serie[-3])
  
  # Perform timeserie adjustment
  if (adjustment=="scale")
    {
    dataset <- mutate(dataset, Value = Value/sum(aggr_serie$TS_Value*aggr_serie$TS_Duration))
    dataset <- mutate(dataset, TS_Value = TS_Value * Value)
    } else if (adjustment=="mult")
      {
      dataset <- mutate(dataset, TS_Value = TS_Value * Value)
      } else if (adjustment=="none")
        {
        }
      
  return(select(dataset,-Value))
  }

#Calculate average of every hour type
ts_cat_avg <- function(ts_data,ts_map)
  {
  ts_data_avg <- as.data.frame(aggregate(ts_data, by = ts_map, FUN=mean)) %>%
    rename(TimeSlice=1,TS_Value=2)
  return(ts_data_avg)
  } 

#Calculate sum of every hour type
ts_cat_sum <- function(ts_data,ts_map)
  {
  ts_data_sum <- as.data.frame(aggregate(ts_data, by = ts_map, FUN=sum)) %>%
    rename(TimeSlice=1,TS_Value=2)
  return(ts_data_sum)
  }
  
#Calculate share of sum of every hour type
ts_cat_shr <- function(ts_data, ts_map)
  {
  ts_data_shr <- ts_cat_sum(ts_data,ts_map) %>%
    mutate(TS_Value=TS_Value/sum(ts_data))
  return(ts_data_shr)
  }

#Calculate time slice specific availibility factor (average ts cat / max ts_data)
ts_cat_afa <- function(ts_data,ts_map)
  {
  ts_data_afa <- ts_cat_avg(ts_data,ts_map) %>%
    mutate(TS_Value=TS_Value/max(ts_data))
  return(ts_data_afa)
}

#Calculate share of sum of every hour type divided by its duration
ts_cat_sbd <- function(ts_data, ts_map)
{
  duration <- ts_duration(ts_map)
  ts_data_sbd <- ts_cat_shr(ts_data,ts_map) %>%
    merge(duration) %>%
    mutate(TS_Value=TS_Value/TS_Duration) %>%
    select(-TS_Duration)
  return(ts_data_sbd)
}