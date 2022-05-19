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
  
  industry_demands <- c("ICAF","ICON","IEOE","IFAB","IMAE","IMAP","INEM","IOMA",
                        "IONM","IPX4","IRAP","ITAP","ITEM","IWAP")
  
  # Services ----
  ## Commercial Services ----
  srv_cs_dem <- expand.grid(Pset_PN=NA, Cset_CN="SRV-CS",
                             Region=c(single_region,county_polyfill),
                             Year=defaultYear, Attribute="COM_FR",
                             Transformation="none_shr", Value = NA, 
                             TS_Level="DayNite", Other_Indexes=NA, Pset_CI=NA,
                             Cset_set=NA, Cset_CD=NA, Target_Sheet="SRV_CS_DEM",
                             Serie="Private_Service")
  
  ## Public Services ----
  srv_pu_dem <- expand.grid(Pset_PN=NA, Cset_CN="SRV-PU",
                             Region=c(single_region,county_polyfill),
                             Year=defaultYear, Attribute="COM_FR",
                             Transformation="none_shr", Value = NA, 
                             TS_Level="DayNite", Other_Indexes=NA, Pset_CI=NA,
                             Cset_set=NA, Cset_CD=NA, Target_Sheet="SRV_PU_DEM",
                             Serie="Public_Service")
  ## Data Centres ----
  ### Data Centres demand variation ----
  srv_dcs_dem <- expand.grid(Pset_PN=NA, Cset_CN="SRVDCE-CS",
                             Region=c(single_region,county_polyfill),
                             Year=defaultYear, Attribute="COM_FR",
                             Transformation="none_shr", Value = NA, 
                             TS_Level="DayNite", Other_Indexes=NA, Pset_CI=NA,
                             Cset_set=NA, Cset_CD=NA, Target_Sheet="DCs",
                             Serie="temp_2018_dublin_demand")
  
  ### Data Centres excess heat profile ----
  srv_dcs_eh  <- expand.grid(Pset_PN="S-DCE-CS", Other_Indexes="SRVHET-DC-LT",
                             Region=c(single_region,county_polyfill),
                             Attribute="VDA_FLOP", Value=NA,
                             Transformation="none_avg", TS_Level="DayNite",
                             Target_Sheet="DCs", Cset_set=NA, Cset_CD=NA,
                             Year=defaultYear, Cset_CN=NA, Pset_CI=NA,
                             Serie="temp_2018_dublin_excess_heat")
  
  ### Data centres electricity for cooling profile ----
  srv_dcs_ec <- expand.grid(Pset_PN="S-DCE-CS", Other_Indexes="SRVELC-DC-C",
                            Region=c(single_region,county_polyfill),
                            Attribute="VDA_FLOP", Value=NA,
                            Transformation="none_avg", TS_Level="DayNite",
                            Target_Sheet="DCs", Cset_set=NA, Cset_CD=NA,
                            Year=defaultYear, Cset_CN=NA, Pset_CI=NA,
                            Serie = "temp_2018_dublin_cooling")
  
  ## Solar profile ----
  srv_sol_afcs <- expand.grid(Region=c(single_region,county_polyfill),
                              Serie="solar", Attribute="NCAP_AFCS",
                              Year=defaultYear, Other_Indexes="SRVSOL",
                              TS_Level="DayNite", Target_Sheet="SRVSOL",
                              Transformation="none_avg", Cset_set=NA,
                              Cset_CD=NA, Cset_CN=NA, Pset_PN="S*",
                              Pset_CI="SRVSOL", Value=NA)
  
  # Residential ----
  ## Solar profile ----
  rsd_sol_afcs <- expand.grid(Region=c(single_region,county_polyfill), 
                              Serie="solar", Attribute="NCAP_AFCS",
                              Year=defaultYear, Other_Indexes="RSDSOL",
                              TS_Level="DayNite", Target_Sheet="RSDSOL",
                              Transformation="none_avg", Cset_set=NA,
                              Cset_CD=NA, Cset_CN=NA, Pset_PN="R*",
                              Pset_CI="RSDSOL", Value=NA)
  
  ## Space heating demand profile ----
  rsd_sh_com_fr <- data.frame(c(single_region,county_polyfill)) %>%
    rename(Region=1) %>%
    mutate(Serie="Space_Heating",
           Attribute="COM_FR",Year=defaultYear, Other_Indexes=NA, Pset_CI=NA,
           TS_Level="DayNite",Target_Sheet="RSD_SH",Transformation="none_shr",
           Cset_set=NA,Cset_CD=NA,Cset_CN="RSDSH*",Pset_PN=NA,Value=NA)
  
  ## Retrofit energy savings profile ----
  rsd_rtft_af <- data.frame(c(single_region,county_polyfill)) %>%
    rename(Region=1) %>%
    mutate(Serie="Heat_Savings", Pset_CI=NA,
           Attribute="NCAP_AF",Year=defaultYear, Other_Indexes=NA,
           TS_Level="DayNite",Target_Sheet="RSD_RTFT",Transformation="none_sbd",
           Cset_set=NA,Cset_CD=NA,Cset_CN=NA,Pset_PN="R-RTFT*",Value=NA)
  
  ## Appliances demand profile ----
  rsd_oe_dem <- expand.grid(Pset_PN=NA, Cset_CN="RSDOE", Pset_CI=NA,
                             Region=c(single_region,county_polyfill),
                             Year=defaultYear, Attribute="COM_FR",
                             Transformation="none_shr", Value = NA, 
                             TS_Level="DayNite", Other_Indexes=NA,
                             Cset_set=NA, Cset_CD=NA, Target_Sheet="RSD_OE_DEM",
                             Serie="Appliances")
  
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
      Attribute="NCAP_AF",Year=defaultYear, Other_Indexes=NA, Pset_CI=NA,
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
      Attribute="COM_FR",Year=defaultYear, Other_Indexes=NA, Pset_CI=NA,
      TS_Level="DayNite",Target_Sheet="TRA_DEM",Transformation="none_shr",
      Cset_set="DEM",Cset_CD="Transport Demand*",Pset_PN=NA,Value=NA,Cset_CN=NA)
  
  # Industry ----
  ## Industry demand profile ----
  ind_dem <- expand.grid(Pset_PN=NA, Other_Indexes=NA, Attribute="COM_FR",
                         Region=c(single_region,county_polyfill), Value=NA,
                         Transformation="none_shr", TS_Level="DayNite",
                         Target_Sheet="IND_DEM", Cset_set=NA, Cset_CD=NA,
                         Year=defaultYear, Pset_CI=NA, Cset_CN=industry_demands) %>%
    mutate(Serie=ifelse(Cset_set == "ICAF", "Chemical", NA),
           Serie=ifelse(Cset_set == "ICON", "Construction", Serie),
           Serie=ifelse(Cset_set == "IFAB", "Food", Serie),
           Serie=ifelse(Cset_set == "IMAP", "Metal", Serie),
           Serie=ifelse(Cset_set == "IONM", "Cement", Serie),
           Serie=ifelse(Cset_set %in% c("IEOE","IMAE","IOMA","INEM","IPX4","IRAP",
                                      "ITAP","ITEM","IWAP"), "Other_Commodity", 
                        Serie))
  
  
# Create main dictionary ----
  main_dict <- rbind(
    rsd_sol_afcs,
    rsd_sh_com_fr,
    rsd_rtft_af,
    rsd_oe_dem,
    pwr_techs_af,
    tra_dem,
    srv_cs_dem,
    srv_pu_dem,
    srv_dcs_dem,
    srv_dcs_eh,
    srv_dcs_ec,
    srv_sol_afcs,
    ind_dem
    )
  
  return(main_dict)
  }
