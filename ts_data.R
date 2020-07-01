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

  # Other: industry profiles, solar heating and resedential heating availability
  Other_data <- "input/timeseries/other.csv"
  
  # One:

# Fetch Ramses data ----
# Load some of the Ramses data  
from_One <- readWorkbook(One_data,
                            sheet = "TVAR",
                            startRow = 11, 
                            colNames = FALSE,
                            rowNames = FALSE,
                            cols = c(8,10,11,13,14,15))

# Give names to the columns with loaded Ramses data
colnames(from_One) <- c("Heat_demand",
                           "WindOnshore_DKE",
                           "WindOnshore_DKW",
                           "WindOffshore_DKE",
                           "WindOffshore_DKW",
                           "PV")
  

# Combine all the data ----
timeseries <-cbind(from_One,
                   from_Other
                   )

# Substitute NAs with zeros
#timeseries[is.na(timeseries)] <- 0

return(timeseries)
}
