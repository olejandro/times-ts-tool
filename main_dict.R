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
source("read_funcs.R")

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


# ELC trade ----
  elc_price_coefs <- read.xlsx("input/ep_coefs_2011.xlsx") %>%
    pivot_longer(-Year, names_to = "link", values_to = "Value") %>%
    na.omit() %>%
    separate(link, into = c("country_code","Region"), sep = "_")
  
  elc_trade <- read_V_FT(subres,"ELC_ImportExport.x","ELC_IMP-EXP",3) %>%
    select(TechName,"*TechDesc") %>%
    slice(2:(min(which(is.na(.)))-1))  %>% #keep rows [2;first NA)
    mutate(country_code = substr(TechName, nchar(TechName)-2+1, nchar(TechName))) %>%
    merge(elc_price_coefs) %>%
    select(-country_code) %>%
    mutate(Commodity=NA,Attribute="COST",LimType=NA,Transformation="mult_avg",
           Value=ifelse(grepl("EXPELC",TechName,ignore.case=TRUE),Value*0.99,Value), # Export is 0.99 * price
           Serie=ifelse(grepl("DE",TechName,ignore.case=TRUE),"Price_Germany",NA),
           Serie=ifelse(grepl("NO",TechName,ignore.case=TRUE) & 
                          grepl("DKW",Region,ignore.case=TRUE),"Price_Norway",Serie),
           Serie=ifelse(grepl("SE",TechName,ignore.case=TRUE) & 
                          grepl("DKW",Region,ignore.case=TRUE),"Price_Sweden_3",Serie),
           Serie=ifelse(grepl("SE",TechName,ignore.case=TRUE) & 
                          grepl("DKE",Region,ignore.case=TRUE),"Price_Sweden_4",Serie),
           Serie=ifelse(grepl("NL",TechName,ignore.case=TRUE) & 
                          grepl("DKW",Region,ignore.case=TRUE),"Price_Germany",Serie),
           #Serie=ifelse(grepl("UK",TechName,ignore.case=TRUE) & grepl("DKW",Region,ignore.case=TRUE),"Price_Norway",Serie),
           TS_Level="DayNite",Target_Sheet="ELC_EXP_IMP_Prices_TS",CURR="MKr11") %>%
    rename(Process=TechName,Description=`*TechDesc`,YEAR=Year)
  
  
# HOU processes ----
  
  # HOU heating technology data in VT
  vt_hou_htechs <- read_V_FT(mopath,"HOU","BoilersR",2) %>% 
    rename(TechDesc="*TechDesc",COP="Share-I~FX~ResELCH") %>%
    mutate(
      COP=1/as.numeric(COP)
   )
  
  # HOU heating technology data in SubRES
  sr_hou_htechs <- read_V_FT(subres,"HOU_Techs.x","HOU_BoilersR",2) %>%
    rename(TechDesc="*Technology",COP="*OldEFF.-.moved.to.Trans") %>%
    filter(!grepl("\\*RHtech", TechName))
  
  # HOU heat savings - availability factors
  hou_hsav_af <- read_V_FT(subres,"HOU_HeatSav.x","HOU_Topologies",2) %>%
    select(TechName,`*TechDesc`) %>%
    merge(data.frame(c("DKE","DKW"))) %>%
    mutate(Commodity=NA,YEAR=2010,Attribute="NCAP_AF",LimType="FX",Transformation="none_sbd",CURR=NA,
           Value=NA,Serie="Heat_Savings",TS_Level="DayNite",Target_Sheet="AF_HOU_Savings") %>%
    rename(Process=TechName,Description=`*TechDesc`,Region=3)
  
  # HOU heating technologies - availability factors
  hou_htech_af <- rbind(select(vt_hou_htechs,TechName,TechDesc),
                     select(sr_hou_htechs,TechName,TechDesc)) %>%
    distinct() %>%
    merge(data.frame(c("DKE","DKW"))) %>%
    mutate(Commodity=NA,YEAR=2015,Attribute="NCAP_AF",LimType="UP",Transformation="none_afa",
           Value=NA,TS_Level="DayNite",CURR=NA,
           Serie=ifelse(grepl("SOL",TechDesc,ignore.case=TRUE),"PV","Heat_demand"),
           Target_Sheet="HOU_AF_RHtech") %>%
    rename(Process=TechName,Description=`TechDesc`,Region=3)
  
  # HOU heat pumps - COPs  
  hou_hp_share <- rbind(select(vt_hou_htechs,TechName,TechDesc,'Comm-IN',YEAR,COP),
                        select(sr_hou_htechs,TechName,TechDesc,'Comm-IN',YEAR,COP))  %>%
    merge(data.frame(c("DKE","DKW"))) %>%
    rename(Process=TechName,Description=TechDesc,Commodity="Comm-IN",Region=6,
           Value=COP) %>%
    filter(grepl("RESAMB",Commodity,ignore.case=TRUE),YEAR != 0) %>%
    separate(Commodity,c("Commodity","Commodity2"),", ") %>%
    select(-Commodity2) %>%
    mutate(
      Attribute="Share-I",LimType="FX",Value=1/Value,Transformation="mult_avg",CURR=NA,
      Target_Sheet="HOU_COP",TS_Level="Season",
      Serie=ifelse((grepl("ELCE2|ELCE3|ELCN2|ELCN3",Process,ignore.case=TRUE) 
                    & grepl("DKE",Region)),"COP_ASHP_DKE",NA),
      Serie=ifelse((grepl("ELCE2|ELCE3|ELCN2|ELCN3",Process,ignore.case=TRUE) 
                    & grepl("DKW",Region)),"COP_ASHP_DKW",Serie),
      Serie=ifelse((grepl("ELCE4|ELCN4",Process,ignore.case=TRUE) 
                    & grepl("DKE",Region)),"COP_GSHP_DKE",Serie),
      Serie=ifelse((grepl("ELCE4|ELCN4",Process,ignore.case=TRUE) 
                    & grepl("DKW",Region)),"COP_GSHP_DKW",Serie))

# IND processes ----

  #IND techs
  ind_techs <- read_V_FT(subres,"IND_Techs_dist.x","IND_processes",2) %>%
    select(TechName,TechDesc)
  
  # IND techs availability factors (PV)
  ind_techs_af <- ind_techs %>%
    filter(grepl("SOLN",TechName,ignore.case=TRUE)) %>%
    merge(data.frame(c("DKE","DKW"))) %>%
    mutate(Commodity=NA,YEAR=2010,Attribute="NCAP_AF",LimType="UP",Transformation="none_afa",CURR=NA,
           Value=NA,Serie="PV",TS_Level="DayNite",Target_Sheet="IND_AF") %>%
    rename(Process=TechName,Description=TechDesc,Region=3)
  
  # Get the COP data for heat pumps by HP name mask
  ind_cops_dict <- read_V_FT(subres,"IND_Techs_dist_Trans.x","HP_COP",3) %>%
    # Remove unnecessary columns
    select(-TimeSlice,-DKE,-DKW,-X10,-Attribute)  %>%
    # Rename some of the columns
    rename(Description=Name,Type=4,COP=6,Mask=Pset_PN,Input=Cset_CN) %>%
    # Discard rows without COP and repeating rows
    filter(!is.na(COP)) %>% distinct() %>%
    # Remove " COP" substring from Type
    mutate(Type=sub(" COP","",Type))
  
  # Make an empty data frame 
  ind_hp_cops <- data.frame(matrix(ncol=ncol(ind_techs)+ncol(ind_cops_dict)))
  colnames(ind_hp_cops) <- c(colnames(ind_techs),colnames(ind_cops_dict))
  
  # Assign COP values to HPs based on wildcards
  for (hp_mask in unique(ind_cops_dict$Mask)) {
    ind_hp_cops <- 
      rbind(
        merge(
          ind_techs[grepl(glob2rx(hp_mask),ind_techs$TechName,ignore.case = TRUE),],
          ind_cops_dict[ind_cops_dict$Mask==hp_mask,]
          ),
        ind_hp_cops
      )
    }
  
  # IND heat pumps - COPs  
  ind_hp_share <- ind_hp_cops %>%
    # Add a column with regions and name it Region
    merge(data.frame(c("DKE","DKW"))) %>% rename(Region=9)  %>%
    mutate(Attribute="Share-I",LimType="FX",COP=1/COP,Transformation="mult_avg",CURR=NA,
           Target_Sheet="IND_HPCOP",TS_Level="Season",
           Serie=ifelse((grepl("ASHP",Type,ignore.case=TRUE) 
                         & grepl("DKE",Region)),"COP_ASHP_DKE",NA),
           Serie=ifelse((grepl("ASHP",Type,ignore.case=TRUE) 
                         & grepl("DKW",Region)),"COP_ASHP_DKW",Serie),
           Serie=ifelse((grepl("GSHP",Type,ignore.case=TRUE) 
                         & grepl("DKE",Region)),"COP_GSHP_DKE",Serie),
           Serie=ifelse((grepl("GSHP",Type,ignore.case=TRUE) 
                         & grepl("DKW",Region)),"COP_GSHP_DKW",Serie)) %>%
    select(-Description,-Type,-Mask) %>% filter(!is.na(TechName)) %>%
    rename(YEAR=Year,Commodity=Input,Process=TechName,Description=TechDesc,
           Value=COP)

# IND demands ----
  ind_dems <- read_V_FT(mopath,"IND","Commodities",2) %>%
    filter(grepl("DEM",`CSet`)) %>%
    select(CommName,CommDesc) %>%
    merge(data.frame(c("DKE","DKW"))) %>%
    rename(Region=3) %>%
    mutate(Process=NA,YEAR=2010,Attribute="COM_FR",LimType=NA,Value=NA,CURR=NA,
           Transformation="none_shr",Target_Sheet="IND_Demands",TS_Level="DayNite",
           Serie=ifelse(grepl("INDCON",CommName,ignore.case=TRUE),"Construction",NA),
           Serie=ifelse(grepl("INDAGR",CommName,ignore.case=TRUE),"Agriculture",Serie),
           Serie=ifelse(grepl("INDFOD",CommName,ignore.case=TRUE),"Food",Serie),
           Serie=ifelse(grepl("INDCHE",CommName,ignore.case=TRUE),"Chemical",Serie),
           Serie=ifelse(grepl("INDCEM",CommName,ignore.case=TRUE),"Cement",Serie),
           Serie=ifelse(grepl("INDMET",CommName,ignore.case=TRUE),"Metal",Serie),
           Serie=ifelse(grepl("INDCOM",CommName,ignore.case=TRUE),"Other_Commodity",Serie),
           Serie=ifelse(grepl("INDRET",CommName,ignore.case=TRUE),"Wholesale_Retail",Serie),
           Serie=ifelse(grepl("INDPRS",CommName,ignore.case=TRUE),"Private_Service",Serie),
           Serie=ifelse(grepl("INDPUB",CommName,ignore.case=TRUE),"Public_Service",Serie)) %>%
    rename(Commodity=CommName,Description=CommDesc)
  
# HOU demands ----
  ht_hou_dems <- read_V_FT(mopath,"HOU","CommoditiesR",2)%>%
    filter(grepl("DEM",Cset)) %>%
    select(CommName,CommDesc)
  
  ap_hou_dems <- read_V_FT(mopath,"APP","CommoditiesR",2) %>%
    filter(grepl("DEM",`CSet`) & grepl ("Residential",CommDesc)) %>%
    select(CommName,CommDesc)
  
  hou_dems <- rbind(ht_hou_dems,ap_hou_dems) %>%  
    merge(data.frame(c("DKE","DKW"))) %>%
    rename(Region=3) %>%
    mutate(Process=NA,YEAR=2010,Attribute="COM_FR",LimType=NA,Value=NA,CURR=NA,
           Transformation="none_shr",Target_Sheet="HOU_Demands",TS_Level="DayNite",
           Serie=ifelse(grepl("RAD|RAM",CommName,ignore.case=TRUE),"Appliances",NA),
           Serie=ifelse(grepl("RHAREA",CommName,ignore.case=TRUE),"Heat_demand",Serie)) %>%
    rename(Commodity=CommName,Description=CommDesc)
  
# Create main dictionary ----
  main_dict <- rbind(elc_techs_af,
                     elc_trade,
                     ind_techs_af,
                     ind_hp_share,
                     hou_htech_af,
                     hou_hsav_af,
                     hou_hp_share,
                     ind_dems,
                     hou_dems)
  return(main_dict)
  }
