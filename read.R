# Copyright 2019 Energy Modelling Lab ApS
# Copyright 2020 Olexandr Balyk
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

checkModelPath <- function(modelPath){
  defaultOutputDir <- "output"
  if (dir.exists(filepath(modelPath,"SysSettings"))){
    return(modelPath)
  } else {
    if (!dir.exists(defaultOutputDir)){
      dir.create(defaultOutputDir)
    }
    return(paste0(defaultOutputDir,"/"))
  }
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

readXlRange <- function(xlFilePath, sheet=NULL, colNames=TRUE, rowNames=TRUE,
                        rows, cols, detectDates = TRUE) {
  
  # If the sheet name is not NULL read that sheet
  if (!is.null(sheet)) {
    df <- readWorkbook(xlFilePath,
                       sheet = sheet,
                       colNames = colNames,
                       rowNames = rowNames,
                       rows = rows,
                       cols = cols,
                       detectDates = detectDates)
    # Assign column name sheet name if there is only one column
    if (length(colnames(df)) == 1) {
      colnames(df) <- sheet
    }
    return(df)
  } else {
    sheets <- getSheetNames(xlFilePath)
    for (aSheetName in sheets)
    {
      temp_df <- readXlRange(xlFilePath = xlFilePath,
                             sheet = aSheetName,
                             rows = rows,
                             cols = cols)
      # Check the position of the sheet in the sheets list
      if (match(aSheetName,sheets) == 1) {
        df <- temp_df
      } else {
        df <- cbind(df, temp_df)
      }
    }
    return(df)
  }
}