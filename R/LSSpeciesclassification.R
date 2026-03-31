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
  .scIntro(jaspResults, options)
}


.scIntro <- function(jaspResults, options) {
  if(isFALSE(options[["introductoryText"]])) return()
  if(!is.null(jaspResults[["introductoryText"]])) return()

  text <- gettextf('You need an explanation here...')

  jaspResults[["introductoryText"]] <- createJaspHtml(title        = gettext("Welcome to Species Classification with JASP!"),
                                                      text         = text,
                                                      dependencies = "introductoryText",
                                                      position     = 1)
}
