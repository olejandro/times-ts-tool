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


HP_COP <- function(t, outlet_t, const1, const2) {
  HP_COP <- const1*(outlet_t-t)+const2
  return(HP_COP)
}

days2hours <-function(df) {
  new_df <- as.data.frame(matrix(nrow=8760))
  chunk=24
  for (i in 1:nrow(df)) {
    start_row <- (i - 1) * chunk + 1
    end_row <- i * chunk
    new_df[start_row:end_row,] <- df[i,]
  }
  return(new_df)
}