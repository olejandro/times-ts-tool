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
library(dplyr)

write_TFM <- function(aTable,wb,sheet,type='INS',position=c(2, 1),fresh_sheet=FALSE,
                      no_empty_cols=TRUE, pretty_table=TRUE) {
  # Table tag
  if(type %in% c("INS","DINS","UPD","MIG")){
    tag <- paste("~TFM",type,sep="_")
  }else{
    tag <- paste("~",type,sep="")
  }
  # Table tag position
  tagposition <- position
  # Position of the table => right below the tag
  belowtag <- tagposition + c(0, 1)
  # Table style
  tablestyle <- "TableStyleMedium4"
  # Adjust the table
  if (pretty_table){
    aTable <- prettify_table(aTable,table_type=type)
  }
  # Remove empty columns from the table
  if (no_empty_cols){
    aTable <- aTable[ ,apply(aTable, 2, function(x) !all(is.na(x)))]
  }
  # Check whether the original sheet is to be preserved
  if (fresh_sheet & (sheet %in% names(wb)))
  {
    removeWorksheet(wb, sheet = sheet)
  }
  if (!(sheet %in% names(wb)))
  {
    addWorksheet(wb, sheetName = sheet)
  }
  # Write tag
  writeData(wb, sheet = sheet, c(tag), xy = tagposition)
  # Write table
  writeDataTable(wb, sheet = sheet , aTable, xy = belowtag, tableStyle = tablestyle, tableName = sheet)
}

prettify_table <- function(aTable,table_type){
  col_names <- colnames(aTable)
  col_order <- c("TimeSlice",
                 "LimType",
                 "Attribute",
                 "YEAR",
                 "CURR",
                 "PSet_PN",
                 "CSet_CN",
                 "*Description")
  region_cols <- col_names[!(col_names %in% col_order)]
  col_order <- append(col_order,region_cols,after=4)
  # Ensure that all of the column names are valid
  col_names <- col_order[col_order %in% col_names]
  
  #Apply inter/extrapolation rule
  if ("Share-I" %in% unique(aTable$Attribute))
    {
    ie_rules <- aTable[which(aTable$Attribute=="Share-I"),] %>%
      mutate(YEAR=0) %>%
      mutate_at(region_cols,function(x) 5) %>%
      unique()
    
    aTable <- rbind(aTable,ie_rules)
    }
  return(aTable[col_names])
}

update_ts_defs <- function(file_path,ts_cats,season_map,weekly_map,daynite_map,
                           start_fresh=TRUE,ts_def_sheet='TS_Definition'){

  ts_def_cols <- c(5,6,7)
  tables_row <- 4
  
  # Read the existing ts definition table in the file
  if (file.exists(file_path) & !start_fresh){
    wb <- loadWorkbook(file_path)
    if (ts_def_sheet %in% names(wb))
    {
      xl_ts_def <- read.xlsx(file_path, sheet = ts_def_sheet, startRow = tables_row,
                             colNames = TRUE, cols = ts_def_cols, skipEmptyRows = FALSE)
    } else {
      xl_ts_def <- NULL
    }
   
  } else {
    xl_ts_def <- NULL
  }
  
  
  # Determine the maximum number of unique ts levels in ts_cats
  max_unique_rows = 0
  
  for (i in 1:ncol(ts_cats)) {
    n_unique_rows <- length(unique(ts_cats[,i]))
    if (max_unique_rows < n_unique_rows)
    {
      max_unique_rows <- n_unique_rows
    }
  }
  
  # Determine the size of the table to be printed in the file
  
  if (!is.null(xl_ts_def)){
    original_nrows <- nrow(xl_ts_def)
  } else {
    original_nrows <- 0
  }
  
  if (max_unique_rows < original_nrows) {
    ts_def2xl_size <- nrow(xl_ts_def)
  } else {
    ts_def2xl_size <- max_unique_rows
  }
  
  # Load the workbook
  if (file.exists(file_path) & !start_fresh){
    wb <- loadWorkbook(file_path)
    existing_tables <- getTables(wb, sheet = ts_def_sheet)
  } else {
    existing_tables <- NULL
    wb <- createWorkbook()
  }
  
  if (!(ts_def_sheet %in% names(wb))){
    ts_levels <- c("Season", "Weekly", "DayNite")
    Season <- unique(ts_cats[,"Season"])
    Weekly <- unique(ts_cats[,"Day"])
    DayNite <- unique(ts_cats[,"Hour"])
    nrows <- max(c(length(Season),length(Weekly),length(DayNite)))
    ts_defs <- data.frame(matrix(nrow=nrows,ncol=length(ts_levels)))
    colnames(ts_defs) <- ts_levels
    ts_defs[1:length(Season),"Season"] <- Season
    ts_defs[1:length(Weekly),"Weekly"] <- Weekly
    ts_defs[1:length(DayNite),"DayNite"] <- DayNite
    
    write_TFM(ts_defs,wb,ts_def_sheet,type="TimeSlices",
              position=c(ts_def_cols[1],tables_row-1),no_empty_cols=FALSE)
  } else {
  
  # Print ts levels
    for (i in 1:length(ts_def_cols)) {
      v <- character(ts_def2xl_size)
      v[1:ts_def2xl_size] <- NA
      ts_levels <- unique(ts_cats[,i])
      v[1:length(ts_levels)] <- ts_levels
      writeData(wb, ts_def_sheet, v, 
                startCol = ts_def_cols[i], 
                startRow = tables_row + 1
      )
    }
  }
  
  #Print information about timeslices
  info_tables <- c("season_info", "weekly_info", "daynite_info")
  tables2delete <- existing_tables #[existing_tables %in% info_tables]
  
  if (length(tables2delete)>0) {
    for (i in 1:length(tables2delete)) {
      removeTable(wb,sheet = ts_def_sheet, table = tables2delete[i])
    }
  }
  
  writeDataTable(wb,ts_def_sheet,season_map,startCol = 11,startRow = tables_row,
                 tableStyle = "TableStyleLight9",tableName = "season_info",
                 withFilter = FALSE)
  writeDataTable(wb,ts_def_sheet,weekly_map,startCol = 14,startRow = tables_row,
                 tableStyle = "TableStyleLight9",tableName = "weekly_info",
                 withFilter = FALSE)
  writeDataTable(wb,ts_def_sheet,daynite_map,startCol = 17,startRow = tables_row,
                 tableStyle = "TableStyleLight9",tableName = "daynite_info",
                 withFilter = FALSE)
  
  # Save modified workbook
  saveWorkbook(wb, file_path, overwrite = TRUE)
  
}