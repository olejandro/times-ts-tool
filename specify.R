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
library(dplyr)
library(tidyr)
source("read.R")

# Collect all the processes that need subannual ts data specified in a data frame

create_main_dict <- function(mopath){

  #TODO: Read regions from SystemSettings 
  
  defaultYear <- 2018
  
  windOnshore <- c("P-RNW-WIN-ON*")
  newWindOffshore <- c("P-RNW-WIN-OF*")
  solar <- c("P-RNW-SOL-*")
  tidal  <- c("P-RNW*TID*")
  wave <- c("P-RNW*WAV*")
  
  single_region <- "IE"
  county_polyfill <- "National"
  county_regions <- c("IE-CW","IE-D","IE-KE","IE-KK","IE-LS",
                      "IE-LD","IE-LH","IE-MH","IE-OY","IE-WH","IE-WX","IE-WW",
                      "IE-CE","IE-CO","IE-KY","IE-LK","IE-TA","IE-WD","IE-G",
                      "IE-LM","IE-MO","IE-RN","IE-SO","IE-CN","IE-DL","IE-MN")
  
  # Services ----
  ## Data Centres ----
  ### Data Centres demand variation ----
  srv_dcs_dem <- expand.grid(Pset_PN=NA, Cset_CN="SRVDCE-CS",
                             Region=c(single_region,county_polyfill),
                             Year=defaultYear, Attribute="COM_FR",
                             Transformation="none_shr", Value = NA, 
                             TS_Level="DayNite", Other_Indexes=NA,
                             Cset_set=NA, Cset_CD=NA, Target_Sheet="DCs",
                             Serie="temp_2018_dublin_demand")
  
  ### Data Centres excess heat profile ----
  srv_dcs_eh  <- expand.grid(Pset_PN="S-DCE-CS", Other_Indexes="SRVHET-DC-LT",
                             Region=c(single_region,county_polyfill),
                             Attribute="VDA_FLOP", Value=NA,
                             Transformation="none_avg", TS_Level="DayNite",
                             Target_Sheet="DCs", Cset_set=NA, Cset_CD=NA,
                             Year=defaultYear, Cset_CN=NA,
                             Serie="temp_2018_dublin_excess_heat")
  
  ### Data centres electricity for cooling profile ----
  srv_dcs_ec <- expand.grid(Pset_PN="S-DCE-CS", Other_Indexes="SRVELC-DC-C",
                            Region=c(single_region,county_polyfill),
                            Attribute="VDA_FLOP", Value=NA,
                            Transformation="none_avg", TS_Level="DayNite",
                            Target_Sheet="DCs", Cset_set=NA, Cset_CD=NA,
                            Year=defaultYear, Cset_CN=NA,
                            Serie = "temp_2018_dublin_cooling")
  
  # Residential ----
  ## Solar profile ----
  rsd_sol_com_fr <- data.frame(c(single_region,county_polyfill)) %>%
    rename(Region=1) %>%
    mutate(Serie="solar",
           Attribute="COM_FR",Year=defaultYear, Other_Indexes=NA,
           TS_Level="DayNite",Target_Sheet="RSDSOL",Transformation="none_shr",
           Cset_set=NA,Cset_CD=NA,Cset_CN="RSDSOL",Pset_PN=NA,Value=NA)
  
  ## Space heating demand profile ----
  rsd_sh_com_fr <- data.frame(c(single_region,county_polyfill)) %>%
    rename(Region=1) %>%
    mutate(Serie="Space_Heating",
           Attribute="COM_FR",Year=defaultYear, Other_Indexes=NA,
           TS_Level="DayNite",Target_Sheet="RSD_SH",Transformation="none_shr",
           Cset_set=NA,Cset_CD=NA,Cset_CN="RSDSH*",Pset_PN=NA,Value=NA)
  
  ## Retrofit energy savings profile ----
  rsd_rtft_af <- data.frame(c(single_region,county_polyfill)) %>%
    rename(Region=1) %>%
    mutate(Serie="Heat_Savings",
           Attribute="NCAP_AF",Year=defaultYear, Other_Indexes=NA,
           TS_Level="DayNite",Target_Sheet="RSD_RTFT",Transformation="none_sbd",
           Cset_set=NA,Cset_CD=NA,Cset_CN=NA,Pset_PN="R-RTFT*",Value=NA)
  
  # Power sector ----
  ## Wind, solar, wave and tidal profiles ----
  pwr_techs_af <- data.frame(c(windOnshore,newWindOffshore,
                               solar,wave,tidal)) %>%
    rename(Pset_PN=1) %>%
    merge(data.frame(c(single_region,county_polyfill))) %>%
    rename(Region=2) %>%
    mutate(
      Value=1,
      Serie=ifelse(Pset_PN %in% c(windOnshore),"onshore",NA),
      Serie=ifelse(Pset_PN %in% newWindOffshore,"offshore",Serie),
      Serie=ifelse(Pset_PN %in% c(solar),"solar",Serie),
      Serie=ifelse(Pset_PN %in% c(wave),"wave",Serie),
      Serie=ifelse(Pset_PN %in% c(tidal),"tidal",Serie),
      Attribute="NCAP_AF",Year=defaultYear, Other_Indexes=NA,
      TS_Level="DayNite",Target_Sheet="PWR_AF",Transformation="mult_avg",
      Cset_set=NA,Cset_CD=NA,Cset_CN=NA)
  
  # Transport ----
  ## Transport demand profile ----
  tra_dem <- data.frame(c(single_region,county_regions)) %>%
    rename(Region=1) %>%
    mutate(
      Serie=ifelse(Region %in% c("IE"),"transport_national",NA),
      Serie=ifelse(Region %in% c("IE-D"),"transport_dublin",Serie),
      Serie=ifelse(Region %in% c("IE-KE","IE-MH","IE-WW"),"transport_greater_dublin",Serie),
      Serie=ifelse(Region %in% c("IE-CW","IE-KK","IE-LS","IE-LD","IE-LH","IE-OY",
                                 "IE-WH","IE-WX","IE-CE","IE-KY","IE-TA","IE-LM",
                                 "IE-MO","IE-RN","IE-SO","IE-CN","IE-DL","IE-MN"),
                   "transport_average1",Serie),
      Serie=ifelse(Region %in% c("IE-CO","IE-LK","IE-WD","IE-G"),
                   "transport_average2",Serie),
      Attribute="COM_FR",Year=defaultYear, Other_Indexes=NA,
      TS_Level="DayNite",Target_Sheet="TRA_DEM",Transformation="none_shr",
      Cset_set="DEM",Cset_CD="Transport Demand*",Pset_PN=NA,Value=NA,Cset_CN=NA)
  
# Create main dictionary ----
  main_dict <- rbind(
    rsd_sol_com_fr,
    rsd_sh_com_fr,
    rsd_rtft_af,
    pwr_techs_af,
    tra_dem,
    srv_dcs_dem,
    srv_dcs_eh,
    srv_dcs_ec
    )
  
  return(main_dict)
  }
