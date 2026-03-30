#
# Copyright (C) 2019 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#


LSSpeciesclassification <- function(jaspResults, dataset, options, state = NULL){
  if (is.null(jaspResults[["readyTable"]])){
    # 这里的名字建议换一个，不要叫 "ready"，换成更有意义的 "readyTable"
    newTable <- createJaspTable(title = "This is a table")

    # 必须至少定义一列，否则有些版本的 JASP 会因为表格是“空的”而报错
    newTable$addColumnInfo(name = "status", title = "Status", type = "string")
    newTable$addRows(list(status = "Successfully Loaded"))

    jaspResults[["readyTable"]] <- newTable
  }
}

