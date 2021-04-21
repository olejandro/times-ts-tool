# Copyright 2019 Energy Modelling Lab ApS
# Copyright 2020, 2021 Olexandr Balyk
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
  # Plexos ENTSO-E data:
  entsoe_solar_data <- filepath("input/timeseries/Solar","IE")
  entsoe_onshore_data <- filepath("input/timeseries/Onshore","IE")
  entsoe_offshore_data <- filepath("input/timeseries/Offshore","IE")
  
  # Renewables ninja
  ninja_solar_data <- filepath("input/timeseries/renewables-ninja","pv")
  ninja_wind_data <- filepath("input/timeseries/renewables-ninja","wind")
  
  # Transport data
  transport_data <- "input/misc/transport.xlsx"
  
  # Chimera data
  chimera_data <- "input/timeseries/chimera/chimera-series.csv"
  
  # Data Centre data
  dcs_data <- "input/timeseries/dcs.csv"
  
  # Other: industry profiles, solar heating and resedential heating availability
  Other_data <- "input/timeseries/other.csv"

  # # Fetch PLEXOS ENTSO-E data ----
  # entsoe_solar <- read.csv(entsoe_solar_data, header = TRUE)
  # # Drop the last record => it is from a different year
  # entsoe_solar <- entsoe_solar[-1,]
  # 
  # entsoe_onshore <- read.csv(entsoe_onshore_data, header = TRUE)
  # entsoe_offshore <- read.csv(entsoe_offshore_data, header = TRUE)
  # 
  # # a) Use a single year ----
  # selectedYear <- "X1"
  # 
  # # Combine into a singe dataframe one data year per dataset
  # from_entsoe <- cbind(entsoe_solar[selectedYear],
  #                      entsoe_onshore[selectedYear],
  #                      entsoe_offshore[selectedYear])
  # 

  # # b) Use multi year mean ----
  # entsoe_solar <- rowMeans(entsoe_solar[sapply(entsoe_solar, is.numeric)])
  # entsoe_onshore <- rowMeans(entsoe_onshore[sapply(entsoe_onshore, is.numeric)])
  # entsoe_offshore <- rowMeans(entsoe_offshore[sapply(entsoe_offshore, is.numeric)])
  # 
  # 
  # from_entsoe <- as.data.frame(cbind(entsoe_solar,
  #                      entsoe_onshore,
  #                      entsoe_offshore))
  
  # # Give names to the columns with data
  # colnames(from_entsoe) <- c("solar",
  #                         "onshore",
  #                         "offshore")
  
  # # Fetch renewables.ninja data ----
  # # Specify timezone of the series
  # ninja_tz <- "UTC"
  # 
  # ninja_solar <- read.csv.zoo(ninja_solar_data, header = TRUE, tz=ninja_tz)
  # ninja_wind <- read.csv.zoo(ninja_wind_data, header = TRUE, tz=ninja_tz)
  # 
  # # Keep only IE data
  # 
  # ninja_solar <- ninja_solar[, grepl("IE", names(ninja_solar)), drop=FALSE]
  # ninja_wind <- ninja_wind[, grepl("IE", names(ninja_wind)), drop=FALSE]
  # 
  # # Combine into a singe dataframe
  # from_ninja <- cbind(ninja_solar,
  #                     ninja_wind)
  # 
  # colnames(from_ninja) <- c("solar",
  #                           "offshore",
  #                           "onshore")
  # 
  # # a) Use a single year ----
  # keepYear <- 2016
  # 
  # # Create index for subsetting
  # keepYearIndex <- seq(from = as.POSIXct(paste(keepYear,"01-01 00:00",sep="-"), tz=ninja_tz),
  #                      to = as.POSIXct(paste(keepYear,"12-31 23:00",sep="-"), tz=ninja_tz), by = "hour")
  # 
  # # Skip 29.02 if present
  # if (!is.na(as.Date(paste(keepYear, 2, 29, sep = "-"), format="%Y-%m-%d"))) {
  #   keepYearIndex <- keepYearIndex[which(keepYearIndex < paste(keepYear,"02-29 00:00:00",sep="-") | 
  #                                        keepYearIndex > paste(keepYear,"02-29 23:00:00",sep="-"))]
  # }
  # 
  # # Keep only selected year and remove time index
  # from_ninja <- as.data.frame(coredata(from_ninja[keepYearIndex]))

  # b) Use multi year mean ----
  
  #...

  # Fetch Transport data ----
  
  # Generate transport demand time series
  transport_year <- 2018
  tz <- "UTC"
  
  dayLoadRows <- seq(19,26,1)
  dayLoadCols <- c(2,5)
  
  hourLoadRows <- seq(32,56,1)
  hourLoadCols <- c(2,5)
  
  hourShares <- readXlRange(xlFilePath = transport_data,
                            rows = hourLoadRows,
                            cols = hourLoadCols)
  
  dayShares <- readXlRange(xlFilePath = transport_data,
                           rows = dayLoadRows,
                           cols = dayLoadCols)
  
  #create hourly index for the whole year, assuming UTC time zone
  time_index <- seq(from = as.POSIXct(paste(transport_year,"01-01 00:00",sep="-"), tz=tz),
                    to = as.POSIXct(paste(transport_year,"12-31 23:00",sep="-"), tz=tz),
                    by = "hour")
  
  #create a data frame to categorise hours
  transport_series <- xts(data.frame(matrix(ncol=length(colnames(dayShares)),
                                            nrow=length(time_index))),
                          order.by = time_index)
  
  colnames(transport_series) <- colnames(dayShares)
  
  # Assign day shares to the dataframe
  for (i in index(dayShares)) {
    if (i == 7) {
      dayIndex <- 0
    } else {
      dayIndex <- i
    }
    for (aColName in colnames(dayShares)){
      transport_series[.indexwday(transport_series) == dayIndex,aColName] <- 
        dayShares[i,aColName] 
    }
  }
  
  # Multiply day shares in the dataframe by hour shares
  for (i in index(hourShares)) {
    for (aColName in colnames(hourShares)){
      transport_series[.indexhour(transport_series) == (i-1),aColName] <- 
        hourShares[i,aColName] * transport_series[.indexhour(transport_series) == (i-1),aColName]
    }
  }
  
  transport_series <- as.data.frame(coredata(transport_series))
  
  transport <- transport_series[,c(1,2,3)]
  
  colnames(transport) <- c("transport_national","transport_dublin",
                           "transport_greater_dublin")
  
  transport[,"transport_average1"] <- (transport_series["Urban towns"] +
                                         transport_series["Rural areas"] +
                                         transport_series["Other urban areas"])/3
  
  transport[,"transport_average2"] <- (transport_series["Regional cities"] +
                                         (transport_series["Urban towns"] +
                                            transport_series["Rural areas"] +
                                            transport_series["Other urban areas"])/3)/2
  

  # Fetch Chimera data ----
  
  from_chimera <- read.csv(chimera_data,header=TRUE, sep=",",stringsAsFactors=FALSE)
  
  # Fetch data centre data ----
  from_dcs <- read.csv(dcs_data, header = TRUE)
  
  # Fetch other data ----
  # Load Other data
  from_Other <- read.csv(Other_data, header = TRUE)
  
  # Calculate heat savings profile ----
  
  hw_demand <- min(from_Other$Heat_demand)
  annual_hw_demand <- hw_demand * nrow(from_Other)
  annual_heat_demand <- sum(from_Other$Heat_demand)
  
  heat_savings <- as.data.frame(from_Other$Heat_demand) %>%
    rename(Heat_Savings="from_Other$Heat_demand") %>%
    mutate(Heat_Savings=(Heat_Savings-hw_demand)/(annual_heat_demand-annual_hw_demand))
  
  space_heating <- as.data.frame(from_Other$Heat_demand) %>%
    rename(Space_Heating="from_Other$Heat_demand") %>%
    mutate(Space_Heating=(Space_Heating-hw_demand))
  
  # Combine all the data ----
  timeseries <-cbind(
    #from_ninja,
    transport,
    from_chimera,
    from_dcs,
    from_Other,
    space_heating,
    heat_savings
    # from_entsoe
    )
  
return(timeseries)
}
