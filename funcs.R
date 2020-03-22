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

write_TFM <- function(aTable,wb,sheet,type='DINS',position=c(2, 1),fresh_sheet=FALSE,
                      no_empty_cols=TRUE, pretty_table=TRUE) {
  # Table tag
  tag <- paste("~TFM",type,sep="_")
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