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

  if (is.null(jaspResults[["dataContainer"]])) {
    dataContainer <- createJaspContainer(title = gettext("Data"))
    dataContainer$dependOn(c("inputType", "randsamp", "speandnum", "sampseq",
                             "redrawTrigger", "resetSample"))
    jaspResults[["dataContainer"]] <- dataContainer
  }



  if (data_mode != "randsamp") {
    jaspResults[["islandContainer"]] <- NULL
  }

  if (data_mode == "randsamp") {
    .scCreateIsland(jaspResults, options)

    if (options[["redrawTrigger"]] > 0) {
      .scUpdateSample(jaspResults, options)
    }


  }

  if (!is.null(jaspResults[["dataContainer"]])) {
    .scDisplaySample(jaspResults, options, jaspResults[["dataContainer"]])
    .scDisplayAll(jaspResults, options, jaspResults[["dataContainer"]])
  }

  if (!isFALSE(options[["barBatch"]]))
    .scDrawBatchBar(jaspResults, options, jaspResults[["dataContainer"]])

  if (!isFALSE(options[["barAllSample"]]))
    .scDrawAllBar(jaspResults, options, jaspResults[["dataContainer"]])

  #######################Models#############################
  modelContainer <- jaspResults[["modelContainer"]]

  if (is.null(modelContainer)){
    modelContainer <- createJaspContainer(title = gettext("Model"))
    modelContainer$dependOn(c("inputType", "randsamp", "speandnum", "sampseq",
                              "models"))
    jaspResults[["modelContainer"]] <- modelContainer
  }

  if (!is.null(modelContainer)){
    .scDisplayModel(jaspResults, options, modelContainer)
    .scSummaryModel(jaspResults, options, modelContainer)
    .scPlotModel(jaspResults, options, modelContainer)
  }

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


#################Models#######################################
.scDisplayModel <- function(jaspResults, options, modelContainer){
  if (is.null(options[["models"]]))
    return()
  if (!is.null(modelContainer[["modelTable"]]))
    return()

  modelTable <- createJaspTable(title = gettext("Models"))
  modelTable$dependOn(c("inputType", "randsamp", "speandnum", "sampseq",
                        "models"))
  ###Code###
  modelTable$addColumnInfo(name = "name",          title = gettext("Name"),         type = "string")
  modelTable$addColumnInfo(name = "distribution",  title = gettext("Distribution"), type = "string")
  modelTable$addColumnInfo(name = "para",          title = gettext("Parameter"), type = "string")
  modelTable$addColumnInfo(name = "min",           title = gettext("Min"),       type = "integer")
  modelTable$addColumnInfo(name = "max",           title = gettext("Max"),       type = "integer")

  modelContainer[["modelTable"]] <- modelTable


  explainModel <- sapply(options[["models"]], function(m) m[["showExplain"]])

  for (m in options[["models"]]) {
    name <- m[["name"]]
    distribution <- m[["type"]]
    if (m[["type"]] == "point"){
      para <- paste0("N = ", m[["pointPriorN"]])
      min <- ""
      max <- ""
      desc <- ifelse(m[["showExplain"]], .scDescText(m), "")
    }else if (m[["type"]] == "uniform"){
      para <- ""
      min <- m[["minimum"]]
      max <- m[["maximum"]]
      desc <- ifelse(m[["showExplain"]], .scDescText(m), "")
    }else if (m[["type"]] == "poisson"){
      para <- paste0("\u03BB = ", m[["poissonlambda"]])
      min <- m[["minimum"]]
      max <- m[["maximum"]]
      desc <- ifelse(m[["showExplain"]], .scDescText(m), "")
    }else if (m[["type"]] == "negbino"){
      para <- paste0("\u03BC = ", m[["nbMu"]], ", \u03D5 = ", m[["nbPhi"]])
      min <- m[["minimum"]]
      max <- m[["maximum"]]
      desc <- ifelse(m[["showExplain"]], .scDescText(m), "")
    }

    rowList <- list(
      name = name,
      distribution = distribution,
      para = para,
      min = as.character(min),
      max = as.character(max)
    )


    modelTable$addRows(rowList)

    if (m[["showExplain"]]) {
      htmlID <- paste0("html_desc_", m[["name"]])

      htmlContent <- gettextf("<b>Explanation for %1$s:</b><br>%2$s",
                              m[["name"]], .scDescText(m))

      modelContainer[[htmlID]] <- createJaspHtml(htmlContent)

      modelContainer[[htmlID]]$maxWidth <- "600px"
    }
  }
}

.scSummaryModel <- function(jaspResults, options, modelContainer){
  if (is.null(options[["models"]]))
    return()
  if (!is.null(modelContainer[["priorSumTable"]]))
    return()

  priorSumTable <- createJaspTable(title = gettext("Prior Summary Statistics"))
  priorSumTable$dependOn(c("models", "priorMean", "priorMedian", "priorMode",
                           "priorSD", "variableBetween", "priorUserMin", "priorUserMax"))

  priorSumTable$addColumnInfo(name = "name", title = gettext("Model"), type = "string")

  stAllOptions <- c("priorMean", "priorMedian", "priorMode",
                    "priorSD", "variableBetween")

  idx_checked <- which(as.logical(options[stAllOptions]))

  if (length(idx_checked) == 0) return()

  stAllTitles <- c(gettext("priorMean"),
                   gettext("priorMedian"),
                   gettext("priorMode"),
                   gettext("priorSD"),
                   gettextf("P(%1$s \u2264 S \u2264 %2$s)", options[["priorUserMin"]], options[["priorUserMax"]])
  )

  for (i in idx_checked) {
    priorSumTable$addColumnInfo(name = stAllOptions[i], title = stAllTitles[i], type = "number")
  }

  modelContainer[["priorSumTable"]] <- priorSumTable

  for (m in options[["models"]]) {
    mPMF <- .scGetModelPMF(m)
    name <- m[["name"]]
    m_sum_list <- list(name = name)

    stat_vec <- list(priorMean     = .scCalculateMean(mPMF),
                     priorMedian   = .scCalculateMedian(mPMF),
                     priorMode     = .scCalculateMode(mPMF),
                     priorSD       = .scCalculateSD(mPMF),
                     variableBetween  = .scCalculateProbability(mPMF, as.numeric(options[["priorUserMin"]]),
                                                             as.numeric(options[["priorUserMax"]])))

    for (i in idx_checked) {
      opt_name <- stAllOptions[i]
      m_sum_list[[opt_name]] <- stat_vec[[opt_name]]
    }

    priorSumTable$addRows(m_sum_list)
  }

}

.scPlotModel <- function(jaspResults, options, modelContainer){
  for (m in options[["models"]]){
    if (!isFALSE(m[["showPlot"]])){
      mPMF <- .scGetModelPMF(m)
      plotName <- paste0("Prior_plot_", m[["name"]])
      pPlot <- createJaspPlot(title = plotName, width = 450, height = 300)
      pPlot$dependOn(c("models", "priorMean", "priorMedian", "priorMode",
                       "priorSD", "variableBetween", "priorUserMin", "priorUserMax"))

      dfPMF <- data.frame(s = mPMF[["s"]], p = mPMF[["p"]])

      plotObj <- ggplot2::ggplot(dfPMF, ggplot2::aes(x = s, y = p)) +
        ggplot2::geom_bar(stat = "identity", color = "black", width = 0.7) +
        ggplot2::scale_fill_manual(values = c("no" = "grey80", "yes" = "#3e92cc"), guide = "none") +
        ggplot2::labs(x = gettext("Number of Species (S)"), y = gettext("Prior Probability"))
      plotObj <- plotObj +
        ggplot2::xlim(0, max(dfPMF$s) + 2) +
        jaspGraphs::geom_rangeframe() +
        jaspGraphs::themeJaspRaw()

      pPlot$plotObject <- plotObj

      modelContainer[[plotName]] <- pPlot
    }
  }
}


#################Helper Functions#############################
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


.scDescText <- function(model){
  if (model[["type"]] == "point"){
    desc <- gettextf("You are absolutely certain there are exactly %s species, all in, no more, no less",
                     model[["pointPriorN"]])

  }else if (model[["type"]] == "uniform"){
    desc <- gettextf("You know that the number of different species lies between %1$s and %2$s, but anything in between is equally likely, no additional information.",
                     model[["minimum"]], model[["maximum"]])

  }else if (model[["type"]] == "poisson"){
    desc <- gettextf("You know the number of different species lies between %1$s and %2$s, and you expect the number to be around %3$s on average.",
                     model[["minimum"]], model[["maximum"]], model[["poissonlambda"]])

  }else if (model[["type"]] == "negbino"){
    desc <- gettextf("You know the number of different species lies between %1$s and %2$s. You expect about %3$s species, but you've added some uncertainty (\u03D5 = %4$s)",
                     model[["minimum"]], model[["maximum"]], model[["nbMu"]], model[["nbPhi"]])
  }

  return(desc)
}

.scGetModelPMF <- function(model){
  if (model[["type"]] == "point"){
    s_range <- model[["pointPriorN"]]
  }else{
    s_min <- model[["minimum"]]
    s_max <- model[["maximum"]]
    s_range <- s_min:s_max
  }

  raw_probs <- switch(model[["type"]],
                      "point"        = 1,
                      "uniform"       = rep(1, length(s_range)),
                      "poisson"       = dpois(s_range, lambda = model[["poissonlambda"]]),
                      "negbino"       = dnbinom(s_range, size = model[["nbPhi"]], mu = model[["nbMu"]]))

  if (sum(raw_probs) == 0)
    return(NULL)

  probs <- raw_probs / sum(raw_probs)

  PMFlist <- list(s = s_range, p = probs)

  return(PMFlist)
}


.scCalculateMean <- function(modelPMF){
  s_vec <- modelPMF[["s"]]
  p_vec <- modelPMF[["p"]]

  meanMod <- 0

  for (i in 1:length(s_vec)){
    meanMod = meanMod + s_vec[i] * p_vec[i]
  }


  return(meanMod)
}

.scCalculateMode <- function(modelPMF){
  s_vec <- modelPMF[["s"]]
  p_vec <- modelPMF[["p"]]

  mod_s <- s_vec[which.max(p_vec)]
  return(mod_s[1])
}

.scCalculateMedian <- function(modelPMF){
  s_vec <- modelPMF[["s"]]
  p_vec <- modelPMF[["p"]]

  cum_p <- cumsum(p_vec)
  median_ind <- which(cum_p >= 0.5)[1]
  return(s_vec[median_ind])
}

.scCalculateSD <- function(modelPMF){
  s <- modelPMF[["s"]]
  p <- modelPMF[["p"]]

  mu <- sum(s * p)
  variance <- sum((s - mu)^2 * p)
  return(sqrt(variance))
}

.scCalculateProbability <- function(modelPMF, mincut, maxcut){
  s_vec <- modelPMF[["s"]]
  p_vec <- modelPMF[["p"]]

  if (mincut > max(s_vec) || maxcut < min(s_vec)){
    return(0)
  }else{
    cum_p <- sum(p_vec[s_vec >= mincut & s_vec <= maxcut])
  }
  return(cum_p)
}






