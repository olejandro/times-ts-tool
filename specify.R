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
library(dplyr)
library(tidyr)
library(stringr)
source("read.R")

# Collect all the processes that need subannual ts data specified in a data frame

create_main_dict <- function(mopath){

  #TODO: Read regions from SystemSettings
  # Settings ----
  
  defaultYear <- 2005
  
  com_series_names <- c("CCSE","CHLE","CHSE","CLIG","COEL","CPLI","CREF","CWLE",
                        "CWSE","IOI","RCDR","RCME","RCOK","RCRE","RCUE","RCWA",
                        "RDWA","RHME","RHRE","RHUE","RLIG","ROEL","RREF","RWME",
                        "RWRE","RWUE","TCL_TCS","TIB_TMO_TTP_TUB_TUM_TUR_TUT","TFR")
  
  prc_series_names <- c("EUPVSOL_","EUWINON_","EUWINOFF101","EUWINOFF102",
                        "EUWINOFF103")
                    
  
  regions <- c("UA")
  
  # All ----
  
  com_based <- expand.grid(
    Attribute="COM_FR",Serie=com_series_names,Region=regions,Year=defaultYear,
    Transformation="none_shr",TS_Level="DayNite",Target_Sheet="COM_FR",
    Pset_PN=NA, Value = NA
    ) %>%
    mutate(
      Cset_CN=str_split(Serie,"_")
      )
  
  prc_based <- expand.grid(
    Attribute="NCAP_AF",Serie=prc_series_names,Region=regions,Year=defaultYear,
    Transformation="none_afa",TS_Level="DayNite",Target_Sheet="NCAP_AF",
    Cset_CN=NA, Value = NA
  ) %>%
    mutate(Pset_PN=gsub("_","*",Serie))

  
  
# Create main dictionary ----
  main_dict <- rbind(
    prc_based,
    com_based
    )
  
  return(main_dict)
  }
