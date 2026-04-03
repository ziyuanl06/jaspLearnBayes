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


LSSpeciesclassification <- function(jaspResults, dataset, options, state = NULL) {
  .scIntro(jaspResults, options)

  data_mode <- options[["inputType"]]

  if (data_mode != "randsamp") {
    jaspResults[["islandContainer"]] <- NULL
  }

  if (data_mode == "randsamp") {
    .scCreateIsland(jaspResults, options)

    if (options[["redrawTrigger"]] > 0) {
      .scUpdateSample(jaspResults, options)
    }

    if (is.null(jaspResults[["dataContainer"]])) {
      dataContainer <- createJaspContainer(title = gettext("Data"))
      dataContainer$dependOn(c("inputType", "randsamp", "speandnum", "sampseq",
                               "redrawTrigger", "resetSample"))
      jaspResults[["dataContainer"]] <- dataContainer
    }

    if (!is.null(jaspResults[["dataContainer"]])) {
      .scDisplaySample(jaspResults, options, jaspResults[["dataContainer"]])
      .scDisplayAll(jaspResults, options, jaspResults[["dataContainer"]])
    }

  }

  if (!isFALSE(options[["barBatch"]]))
    .scDrawBatchBar(jaspResults, options, jaspResults[["dataContainer"]])

  if (!isFALSE(options[["barAllSample"]]))
    .scDrawAllBar(jaspResults, options, jaspResults[["dataContainer"]])
}


.scIntro <- function(jaspResults, options) {
  if (isFALSE(options[["introductoryText"]]))
    return()
  if (!is.null(jaspResults[["introductoryText"]]))
    return()

  text <- gettextf('You need an explanation here...') #TODO

  jaspResults[["introductoryText"]] <- createJaspHtml(
    title        = gettext("Welcome to Species Classification with JASP!"),
    text         = text,
    dependencies = "introductoryText",
    position     = 1
  )
}


.scCreateIsland <- function(jaspResults, options) {
  if (!is.null(jaspResults[["islandContainer"]]))
    return()

  if (!isFALSE(options[["selectisland"]]))
    set.seed(as.numeric(options[["islandid"]]))

  container <- createJaspContainer(title = gettext("Island"))
  container$dependOn(c("inputType","randsamp", "speandnum", "sampseq","resetSample", "selectisland"))
  jaspResults[["islandContainer"]] <- container

  com_spe   <- c("Pigeon",
                 "Duck",
                 "Cat",
                 "Dog",
                 "Fox",
                 "Sparrow",
                 "Honeybee",
                 "Squirrel")
  uncom_spe <- c("Panda",
                 "Kingfisher",
                 "Sloth",
                 "Capybara",
                 "Lizard",
                 "Eagle",
                 "Koala",
                 "Wombat")
  rare_spe  <- c(
    "Unicorn",
    "Phoenix",
    "Dragon",
    "Griffin",
    "Sphinx",
    "Pegasus",
    "Chimera",
    "Godzilla"
  )

  ncom <- sample(1:8, 1, replace = TRUE)
  nuncom <- sample(1:4, 1, replace = TRUE)
  nrare <- sample(1:2, 1, replace = TRUE)

  total_spe <- ncom + nuncom + nrare

  com_spe_sel   <- sample(com_spe, ncom, replace = FALSE)
  uncom_spe_sel <- sample(uncom_spe, nuncom, replace = FALSE)
  rare_spe_sel  <- sample(rare_spe, nrare, replace = FALSE)

  spe_island <- c(rep(com_spe_sel, 12),
                  rep(uncom_spe_sel, 7),
                  rep(rare_spe_sel, 1))
  all_sample <- c()

  container[["lastTriggerState"]] <- createJaspState(options[["redrawTrigger"]])

  container[["islandState"]] <- createJaspState(spe_island)
  container[["sampleState"]] <- createJaspState(all_sample)
}


.scUpdateSample <- function(jaspResults, options) {
  if (is.null(jaspResults[["sampleContainer"]])) {
    samplecontainer <- createJaspContainer(title = gettext("Batch Sample"))
    samplecontainer$dependOn(c("inputType", "randsamp", "speandnum", "sampseq","redrawTrigger"))
    jaspResults[["sampleContainer"]] <- samplecontainer
  }

  if (is.null(jaspResults[["islandContainer"]]))
    return()

  if (is.null(jaspResults[["islandContainer"]][["islandState"]]))
    return()

  if (is.null(jaspResults[["islandContainer"]][["sampleState"]]))
    return()

  if (options[["redrawTrigger"]] == 0)
    return()

  currentTrigger <- options[["redrawTrigger"]]

  lastTriggerState <- jaspResults[["islandContainer"]][["lastTriggerState"]]

  lastTriggerValue <- if (!is.null(lastTriggerState))
    lastTriggerState$object
  else
    0

  if (currentTrigger > lastTriggerValue) {
    spe_island <- jaspResults[["islandContainer"]][["islandState"]]$object
    batch_sample <- sample(spe_island, as.numeric(options[["nsample"]]), replace = TRUE)

    all_sample <- jaspResults[["islandContainer"]][["sampleState"]]$object
    all_sample <- c(all_sample, batch_sample)

    jaspResults[["islandContainer"]][["sampleState"]] <- createJaspState(all_sample)

    jaspResults[["islandContainer"]][["lastTriggerState"]] <- createJaspState(currentTrigger)

    jaspResults[["sampleContainer"]][["sampleState"]] <- createJaspState(batch_sample)
  }
}

.scDisplaySample <- function(jaspResults, options, dataContainer) {
  if (!is.null(dataContainer[["batchTable"]]))
    return()

  if (is.null(jaspResults[["sampleContainer"]]) ||
      is.null(jaspResults[["sampleContainer"]][["sampleState"]])
  )
    return()

  batch_sample <- jaspResults[["sampleContainer"]][["sampleState"]]$object

  if (length(batch_sample) == 0)
    return()


  batch_counts <- as.data.frame(table(batch_sample),  stringsAsFactors = FALSE)
  batch_counts <- batch_counts[order(-as.numeric(batch_counts$Freq)), ]

  batchTable <- createJaspTable(title = gettext("The Current Batch"))
  batchTable$dependOn(c("inputType","randsamp", "speandnum", "sampseq","redrawTrigger", "resetSample"))

  batchTable$addColumnInfo(name = "species",
                           title = gettext("Species"),
                           type = "string")
  batchTable$addColumnInfo(name = "counts",
                           title = gettext("Count"),
                           type = "integer")

  for (i in seq_len(nrow(batch_counts))) {
    batchTable$addRows(list(species = as.character(batch_counts[i, "batch_sample"]),
                            counts = batch_counts[i, "Freq"]))
  }

  num_spe_batch <- nrow(batch_counts)

  batchTable$addFootnote(message = gettextf("No. different species in the batch: %d", num_spe_batch))

  dataContainer[["batchTable"]] <- batchTable

}

.scDisplayAll <- function(jaspResults, options, dataContainer){
  if (!is.null(dataContainer[["allTable"]]))
    return()

  if (is.null(jaspResults[["islandContainer"]]) ||
      is.null(jaspResults[["islandContainer"]][["sampleState"]]))
    return()

  all_sample <- jaspResults[["islandContainer"]][["sampleState"]]$object

  if (length(all_sample) == 0)
    return()

  all_counts <- as.data.frame(table(all_sample), stringsAsFactors = FALSE)
  all_counts <- all_counts[order(-as.numeric(all_counts$Freq)), ]

  allTable <- createJaspTable(title = gettext("The Whole Sample"))
  allTable$dependOn(c("inputType","randsamp", "speandnum", "sampseq","redrawTrigger", "resetSample"))

  allTable$addColumnInfo(name = "species",
                             title = gettext("Species"),
                             type = "string")
  allTable$addColumnInfo(name = "counts",
                           title = gettext("Count"),
                           type = "integer")
  for (i in seq_len(nrow(all_counts))){
    allTable$addRows(list(species = as.character(all_counts[i, "all_sample"]),
                          counts = all_counts[i, "Freq"]))
  }

  num_spe_all <- nrow(all_counts)
  num_samp_all <- sum(all_counts$Freq)

  allTable$addFootnote(message = gettextf("No. different species in the overall sample: %d", num_spe_all))
  allTable$addFootnote(message = gettextf("No. individuals in the overall sample: %d", num_samp_all))

  dataContainer[["allTable"]] <- allTable


}

.scDrawBatchBar <- function(jaspResults, options, dataContainer){
  if (!is.null(dataContainer[["batchBar"]]))
    return()

  if (is.null(jaspResults[["sampleContainer"]]) ||
      is.null(jaspResults[["sampleContainer"]][["sampleState"]]))
    return()

  batch_sample <- jaspResults[["sampleContainer"]][["sampleState"]]$object

  if (length(batch_sample) == 0)
    return()

  batchPlot <- createJaspPlot(title = gettext("Species Frequencies (Current Batch)"),
                              width = 600, height = 320)

  batchPlot$dependOn(c("inputType","redrawTrigger", "resetSample", "barBatch"))

  batchPlot$plotObject <- .scFillDataBarPlot(batch_sample)

  dataContainer[["batchBar"]] <- batchPlot
}

.scDrawAllBar <- function(jaspResults, options, dataContainer){
  if (!is.null(dataContainer[["allBar"]]))
    return()

  if (is.null(jaspResults[["islandContainer"]]) ||
      is.null(jaspResults[["islandContainer"]][["islandState"]]))
    return()

  all_sample <- jaspResults[["islandContainer"]][["islandState"]]$object
  all_sample <- jaspResults[["islandContainer"]][["sampleState"]]$object

  if (length(all_sample) == 0)
    return()

  allPlot <- createJaspPlot(title = gettext("Species Frequencies (Overall)"),
                            width = 600, height = 320)
  allPlot$dependOn(c("inputType","redrawTrigger", "resetSample", "barAllSample"))

  allPlot$plotObject <- .scFillDataBarPlot(all_sample)

  dataContainer[["allBar"]] <- allPlot
}


.scFillDataBarPlot <- function(sample_vec){
  df <- as.data.frame(table(Species = sample_vec))
  colnames(df) <- c("Species", "Count")

  df$Species <- reorder(df$Species, df$Count)


  p <- ggplot2::ggplot(df, ggplot2::aes(x = Species, y = Count)) +
    ggplot2::geom_bar(stat = "identity", fill = "steelblue", color = "black", width = 0.7) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = gettext("Species"), y = gettext("Count"))

  p <- p +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  return(p)
}
