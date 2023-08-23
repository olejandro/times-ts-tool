# Copyright 2019 Energy Modelling Lab ApS
# Copyright 2020 Olexandr Balyk
# Copyright 2021 University College Cork
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


library(openxlsx)
library(xts)
library(dplyr)
source("calculate.R")
source("read.R")

fetch_timeseries <- function() {
  
  # Overview of data by source ----
  timeseries_data <- "input/times-input.csv"
  
  # Settings ----
  
  year <- 2018
  tz <- "UTC"
  
  #create hourly index for the whole year, assuming UTC time zone
  time_index <- seq(from = as.POSIXct(paste(year,"01-01 00:00",sep="-"), tz=tz),
                    to = as.POSIXct(paste(year,"12-31 23:00",sep="-"), tz=tz),
                    by = "hour")
  
  # Fetch load profiles ----
  
  df <- read.csv(timeseries_data)

  
  #Drop index column
  timeseries <- df[,-1]
  
return(timeseries)
}
