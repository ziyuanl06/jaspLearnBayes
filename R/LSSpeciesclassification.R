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

  data_mode <- options[["inputType"]]

  if (data_mode != "randsamp"){
    jaspResults[["islandState"]] <- NULL
  }

  if (data_mode == "randsamp"){
    if (is.null(jaspResults[["islandState"]]) || options[["redrawTrigger"]] == 0) {
      spe_island <- .scCreateIsland(jaspResults, options)
    }

  }
}


.scIntro <- function(jaspResults, options) {
  if(isFALSE(options[["introductoryText"]])) return()
  if(!is.null(jaspResults[["introductoryText"]])) return()

  text <- gettextf('You need an explanation here...') #TODO

  jaspResults[["introductoryText"]] <- createJaspHtml(title        = gettext("Welcome to Species Classification with JASP!"),
                                                      text         = text,
                                                      dependencies = "introductoryText",
                                                      position     = 1)
}


.scCreateIsland <- function(jaspResults, options){
  if (options[["redrawTrigger"]] != 0) return()

  com_spe   <- c("Pigeon", "Duck", "Cat", "Dog", "Fox", "Sparrow", "Honeybee", "Squirrel")
  uncom_spe <- c("Panda", "Kingfisher", "Sloth", "Capybara", "Lizard", "Eagle", "Koala", "Wombat")
  rare_spe  <- c("Unicorn", "Phoenix", "Dragon", "Griffin", "Sphinx", "Pegasus", "Chimera", "Godzilla")

  nspecies <- sample(1:5, 3, replace = TRUE)
  ncom <- nspecies[1]; nuncom <- nspecies[2]; nrare <- nspecies[3]

  total_spe <- ncom + nuncom + nrare

  com_spe_sel   <- sample(com_spe, ncom, replace = FALSE)
  uncom_spe_sel <- sample(uncom_spe, nuncom, replace = FALSE)
  rare_spe_sel  <- sample(rare_spe, nrare, replace = FALSE)

  spe_island <- c(rep(com_spe_sel, 12), rep(uncom_spe_sel, 7), rep(rare_spe_sel, 1))

  islandState <- createJaspState(spe_island)
  islandState$dependOn(options = c("redrawTrigger"))
  jaspResults[["islandState"]] <- islandState

  return(spe_island)
}
