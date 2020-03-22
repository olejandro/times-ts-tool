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
source("calc_funcs.R")
source("read_funcs.R")

fetch_timeseries <- function() {

# Overview of data by source ----
# Ramses: heat demand, wind onshore, wind offshore, PV
  Ramses_data <- "input/timeseries/Ramses_data.xlsx"  
# Energinet: power demand, electricity prices in Norway, Sweden and Germany
  Energinet_data <- "input/timeseries/Markeddata 2011 prices and exchange_NY.xlsx"
  extra_data <- "input/timeseries/Markeddata 2011 power demand.xlsx"
# DMI: air and soil temperature which are used to calculate COP for HPs
  air_temp_data <- "input/DMI/TR13-19_DRY/temperature/"
  soil_temp_data <- "input/DMI/TR13-19_DRY/soil temperature/"
# Other: industry profiles, solar heating and resedential heating availability
  Other_data <- "input/timeseries/other.csv"

# Fetch Ramses data ----
# Load some of the Ramses data  
from_Ramses <- readWorkbook(Ramses_data,
                            sheet = "TVAR",
                            startRow = 11, 
                            colNames = FALSE,
                            rowNames = FALSE,
                            cols = c(8,10,11,13,14,15))

# Give names to the columns with loaded Ramses data
colnames(from_Ramses) <- c("Heat_demand",
                           "WindOnshore_DKE",
                           "WindOnshore_DKW",
                           "WindOffshore_DKE",
                           "WindOffshore_DKW",
                           "PV")

# Fetch Energinet data ----

# Load some of the Energinet data 
from_Energinet <- readWorkbook(Energinet_data,
                               sheet = "Markedsdata (13)",
                               startRow = 4, 
                               colNames = FALSE,
                               rowNames = FALSE,
                               cols = c(5,7,8,11))

# Give names to the columns with loaded Energinet data
colnames(from_Energinet) <- c("Price_Norway",
                              "Price_Sweden_3",
                              "Price_Sweden_4",
                              "Price_Germany")

from_Energinet <- as.data.frame(apply(from_Energinet,2,function(x) x/3.6))

# Add a column for power demand data 
from_Energinet$Power_demand <-NA

# Read power demand data from Energinet
Power_demand <- readWorkbook(extra_data,
                             sheet = "Sheet1",
                             startRow = 5,
                             colNames = FALSE, 
                             rowNames = FALSE,
                             cols = c(2))

# Add power demand data to other data from Energinet
from_Energinet$Power_demand <- Power_demand$X1

# Fetch and transform DMI data ----
# Read temperature data and assign it to DKE or DKW based on station id
air_temp_DKE <- read_DMI(air_temp_data, "6156", 3)
air_temp_DKW <- read_DMI(air_temp_data, "6060", 3)
soil_temp_DKE <- read_DMI(soil_temp_data, "6156", 3)
soil_temp_DKW <- read_DMI(soil_temp_data, "6065", 3)

# Define constants for calculating COP
outlet_temp  <- 55
ASHP_const1 <- -0.0727 
ASHP_const2 <- 6.0522
GSHP_const1 <- -0.1126
GSHP_const2 <- 8.4587

# Make an empty dataframe
from_DMI <- data.frame(matrix(nrow = 8760)) 
colnames(from_DMI) <- c("COP_ASHP_DKE")

# Calculate COPs based on temperature data and constants
from_DMI$COP_ASHP_DKE <- apply(air_temp_DKE, MARGIN = 1,
                              FUN=function(x) HP_COP(x,outlet_temp,
                                                     ASHP_const1, ASHP_const2))
from_DMI$COP_ASHP_DKW <- apply(air_temp_DKW, MARGIN = 1,
                              FUN=function(x) HP_COP(x,outlet_temp,
                                                     ASHP_const1, ASHP_const2))
from_DMI$COP_GSHP_DKE <- apply(days2hours(soil_temp_DKE), MARGIN = 1,
                              FUN=function(x) HP_COP(x,outlet_temp,
                                                     GSHP_const1, GSHP_const2))
from_DMI$COP_GSHP_DKW <- apply(days2hours(soil_temp_DKW), MARGIN = 1,
                              FUN=function(x) HP_COP(x,outlet_temp,
                                                     GSHP_const1, GSHP_const2))
from_DMI <- apply(from_DMI, MARGIN = 2, FUN=function(x) mean(x)/x) 

# Fetch other data ----
# Load Other data
from_Other <- read.csv(Other_data, header = TRUE)

# Calculate heat savings profile ----

hw_demand <- min(from_Ramses$Heat_demand)
annual_hw_demand <- hw_demand * nrow(from_Ramses)
annual_heat_demand <- sum(from_Ramses$Heat_demand)

calculated <- as.data.frame(from_Ramses$Heat_demand) %>%
  rename(Heat_Savings="from_Ramses$Heat_demand") %>%
  mutate(Heat_Savings=(Heat_Savings-hw_demand)/(annual_heat_demand-annual_hw_demand))
  

# Combine all the data ----
timeseries <-cbind(from_Ramses,
                   from_Energinet,
                   from_DMI,
                   from_Other,
                   calculated)

# Substitute NAs with zeros
#timeseries[is.na(timeseries)] <- 0

return(timeseries)
}
