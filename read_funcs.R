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
library(tidyr)
library(dplyr)

filepath <- function(path, file_name_pattern){
  exclude_pattern <- "~$"
  n = nchar(path)
  if (substr(path, n, n) == "/"){
    sep <- ""
  } else {
    sep <- "/"
  }
  matching_files <- list.files(path=path,pattern=file_name_pattern)
  file_name <- matching_files[!grepl(exclude_pattern,matching_files,fixed=TRUE)]
  return(paste(path, file_name, sep = sep))
}

read_DMI <- function(path, file_name_pattern, column=NULL){
  data <- read.csv(filepath(path,file_name_pattern), sep = ";")
  if (is.null(column)) {
    return(data[,])
  } else {
    return(as.data.frame(data[,column]))
  }
  
}

read_V_FT <- function(path, file_name_pattern, sheet, from_row, colNames = TRUE, fill_columns) {
  xlsxFile <- filepath(path,file_name_pattern)
  df <- read.xlsx(xlsxFile, sheet = sheet, startRow = from_row, colNames = colNames)
  if (missing(fill_columns)){
    return(df)
  } else {
    return(fill(df,all_of(fill_columns)))
  }
}