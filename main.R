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


library(dplyr)
library(tidyr)
library(openxlsx)
source("write.R")
source("fetch.R")
source("categorise.R")
source("specify.R")
source("transform.R")

# File locations ---- 
mopath <- "../../"
subres <- paste(mopath, "SubRES_TMPL/", sep="")
supxls <- paste(mopath, "SuppXLS/", sep="")
subannual <- paste(checkModelPath(supxls), "Scen_SYS_SubAnnual_Data.xlsx", sep="")
syssettings <- paste(mopath, "SysSettings.xlsx", sep="")

# User choices ----
year <- 2018

# Retrieve all the timeseries
ts_data <- fetch_timeseries()

# Categorise all the hours in a year
ts_cats <- categorise_ts(year,syssettings=syssettings)

# Map hours to DayNite, Weekly, and Season time slices
ts_map <- as.data.frame(map_ts(ts_cats))

# Add categories to the timeseries
ts_data <- cbind(ts_cats,ts_data)

### Main dictionary ----
rules <- create_main_dict(mopath)

### Transform and write ----

# Create a workbook
wb <- createWorkbook()

# Add a sheet with timeslice duration
write_TFM(ts_yrfr_data(ts_map[,"DayNite"],unique(rules$Region)), wb, sheet="TimeSlices",
          type="INS", fresh_sheet = TRUE)

# Populate the workbook with timeslice data
for (aTarget_Sheet in unique(rules$Target_Sheet))
  {
  if (!is.na(aTarget_Sheet))
    {
    print(paste("Processing", aTarget_Sheet, sep = " "))
    for (aTS_Level in unique(rules[rules$Target_Sheet==aTarget_Sheet,"TS_Level"]))
      {
      if (!is.na(aTS_Level))
        {
        for (aTransformation in unique(rules[which(rules$Target_Sheet==aTarget_Sheet &
                                                   rules$TS_Level==aTS_Level),"Transformation"]))
          {
          if (!is.na(aTransformation))
            {
            for (aSerie in unique(rules[which(rules$Target_Sheet==aTarget_Sheet &
                                              rules$TS_Level==aTS_Level &
                                              rules$Transformation==aTransformation &
                                              !is.na(rules$Serie)),"Serie"]))
              {
              current_data <- select(rules[which(rules$Target_Sheet==aTarget_Sheet &
                                                    rules$TS_Level==aTS_Level &
                                                    rules$Transformation==aTransformation &
                                                    rules$Serie==aSerie),],
                                      -Target_Sheet,-TS_Level,-Transformation,-Serie)
              transformed_data <- transform_data(current_data,aTransformation,ts_data[aSerie],ts_map[aTS_Level])
              if (!is.null(get0("aDataSet")))
                {
                aDataSet <- rbind(aDataSet,transformed_data)
                }
              else
                {
                aDataSet <- transformed_data
                }
              }
            }
          }
        }
      }
    # Check if aDataSet is populated
    if (!is.null(get0("aDataSet")))
    {
      # Do rounding
      aDataSet$TS_Value <- round(aDataSet$TS_Value,5)
      # Move Regions to columns for unique rows (duplicates are dicarded)
      pDataSet <- pivot_wider(unique(aDataSet),names_from=Region,values_from=TS_Value)
      # Write data to a sheet
      write_TFM(pDataSet,wb,aTarget_Sheet,fresh_sheet=TRUE)
      print(paste("Created", aTarget_Sheet, sep = " "))
      # Empty aDataSet
      aDataSet <- NULL
    }
    }
  }

# Save the workbook

if (saveWorkbook(wb, subannual, overwrite = TRUE, returnValue = TRUE)){
  print(paste("Successfully created:",subannual))
} else {
  print(paste("Failed creating:",subannual))
}

