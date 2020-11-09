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
library(tidyr)
source("read.R")

# Collect all the processes that need subannual ts data specified in a data frame

create_main_dict <- function(mopath){

  subres <- paste(mopath, "SubRES_TMPL", sep="/")
  
  # ELC processes ----
  
  vt_elc_techs <- read_V_FT(mopath,"ELC","TechsR",2,fill_columns = 1:2) %>%
    select(TechName,TechDesc,Region,`Comm-IN`) %>%
    distinct() %>%
    filter(grepl("ELCWIN|ELCWAV|ELCSOL",`Comm-IN`)) %>%
    select(TechName,TechDesc,Region) %>%
    merge(read.csv("input/afa_data_vt.csv",colClasses=c(rep("character",3),"numeric","character"))) %>%
    select(-type) %>%
    rename(Value=AFA)

  sr_elc_techs <- rbind(read_V_FT(subres,"ELC_Techs.x","ELC_TechsR_ELC",2,fill_columns = 1:2),
                        read_V_FT(subres,"ELC_Techs.x","ELC_TechsR_DHC",2,fill_columns = 1:2),
                        read_V_FT(subres,"ELC_Techs.x","ELC_TechsR_DHD",2,fill_columns = 1:2)) %>%
    select(TechName,`*TechDesc`,YEAR,`*availability.pct`) %>%
    rename(TechDesc="*TechDesc",Value="*availability.pct") %>%
    filter(grepl("Wind|Solar|Wave", TechDesc)) %>%
    merge(data.frame(c("DKE","DKW"))) %>%
    rename(Region=5)

  elc_techs_af <- rbind(vt_elc_techs,sr_elc_techs) %>%
    mutate(Commodity=NA,Attribute="NCAP_AF",LimType="UP",Transformation="scale_afa",CURR=NA,
           Target_Sheet=ifelse(grepl("Heat",TechDesc,ignore.case=TRUE),"ELC_SolarHeating",NA),
           Target_Sheet=ifelse(grepl("Onshore|Domestic",TechDesc,ignore.case=TRUE),"ELC_Onshore",Target_Sheet),
           Target_Sheet=ifelse(grepl("Offshore|Near",TechDesc,ignore.case=TRUE),"ELC_Offshore",Target_Sheet),
           Target_Sheet=ifelse(grepl("Photovoltaics",TechDesc,ignore.case=TRUE),"ELC_PV",Target_Sheet),
           TS_Level=ifelse(grepl("Heat",TechDesc,ignore.case=TRUE),"Season","DayNite"),
           Serie=ifelse(grepl("Heat",TechDesc,ignore.case=TRUE),"PV",NA),
           Serie=ifelse((grepl("Onshore|Domestic",TechDesc,ignore.case=TRUE)
                         & grepl("DKE",Region)),"WindOnshore_DKE",Serie),
           Serie=ifelse((grepl("Onshore|Domestic",TechDesc,ignore.case=TRUE)
                         & grepl("DKW",Region)),"WindOnshore_DKW",Serie),
           Serie=ifelse((grepl("Offshore|Near",TechDesc,ignore.case=TRUE)
                         & grepl("DKE",Region)),"WindOffshore_DKE",Serie),
           Serie=ifelse((grepl("Offshore|Near",TechDesc,ignore.case=TRUE)
                         & grepl("DKW",Region)),"WindOffshore_DKW",Serie),
           Serie=ifelse(grepl("Photovoltaics",TechDesc,ignore.case=TRUE),"PV",Serie)) %>%
    rename(Process=TechName,Description=TechDesc)

  
  hou_dems <- rbind(ht_hou_dems,ap_hou_dems) %>%  
    merge(data.frame(c("DKE","DKW"))) %>%
    rename(Region=3) %>%
    mutate(Process=NA,YEAR=2010,Attribute="COM_FR",LimType=NA,Value=NA,CURR=NA,
           Transformation="none_shr",Target_Sheet="HOU_Demands",TS_Level="DayNite",
           Serie=ifelse(grepl("RAD|RAM",CommName,ignore.case=TRUE),"Appliances",NA),
           Serie=ifelse(grepl("RHAREA",CommName,ignore.case=TRUE),"Heat_demand",Serie)) %>%
    rename(Commodity=CommName,Description=CommDesc)
  
# Create main dictionary ----
  main_dict <- rbind(
    elc_techs_af,
    hou_dems
    )
  
  return(main_dict)
  }
