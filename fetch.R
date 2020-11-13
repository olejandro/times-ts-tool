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


library(openxlsx)
library(dplyr)
source("calculate.R")
source("read.R")

fetch_timeseries <- function() {
  
  # Overview of data by source ----
  # Plexos ENTSO-E data:
  solar_data <- filepath("input/timeseries/Solar","IE")
  onshore_data <- filepath("input/timeseries/Onshore","IE")
  offshore_data <- filepath("input/timeseries/Offshore","IE")

  # Fetch PLEXOS ENTSO-E data ----
  entsoe_solar <- read.csv(solar_data, header = TRUE)
  # Drop the last record => it is from a different year
  entsoe_solar <- entsoe_solar[-1,]
  
  entsoe_onshore <- read.csv(onshore_data, header = TRUE)
  entsoe_offshore <- read.csv(offshore_data, header = TRUE)
  
  selectedYear <- "X1"
  
  # Combine into a singe dataframe one data year per dataset
  from_entsoe <- cbind(entsoe_solar[selectedYear],
                       entsoe_onshore[selectedYear],
                       entsoe_offshore[selectedYear])
  
  # Give names to the columns with data
  colnames(from_entsoe) <- c("solar_2018",
                          "onshore_2018",
                          "offshore_2018")

# Fetch Ramses data ----
# Load some of the Ramses data  
# from_One <- readWorkbook(One_data,
#                             sheet = "TVAR",
#                             startRow = 11, 
#                             colNames = FALSE,
#                             rowNames = FALSE,
#                             cols = c(8,10,11,13,14,15))
  


  # Combine all the data ----
  timeseries <-cbind(from_entsoe#,
                   )
  
return(timeseries)
}
