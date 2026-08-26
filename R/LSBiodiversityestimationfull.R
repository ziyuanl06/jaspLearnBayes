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


LSBiodiversityestimationfull <- function(jaspResults, dataset, options, state = NULL) {
  .befIntro(jaspResults, options)

##################Trigger##########################
  curr_draw  <- options[["redrawTrigger"]]
  curr_reset <- options[["resetSample"]]

  if (is.null(jaspResults[["triggerState"]])) {
    prev_draw  <- curr_draw
    prev_reset <- curr_reset
  } else {
    prev       <- jaspResults[["triggerState"]]$object
    prev_draw  <- prev[1]
    prev_reset <- prev[2]
  }
  should_sample <- (curr_draw > prev_draw) && (curr_reset == prev_reset)

#######################Models#############################
  if (is.null(jaspResults[["modelContainer"]]))
    .befCreateModelContainer(jaspResults, options)

  .befDisplayModel(jaspResults, options)
  .befSummaryModel(jaspResults, options)
  .befPlotModel(jaspResults, options)

#######################Data#############################
  if (is.null(jaspResults[["dataContainer"]]))
    .befCreateDataContainer(jaspResults, options)

  .befCreateIsland(jaspResults, options)
  if (should_sample)
    .befUpdateSample(jaspResults, options)

  newTriggerState <- createJaspState(c(curr_draw, curr_reset))
  newTriggerState$dependOn(c("resetSample"))
  jaspResults[["triggerState"]] <- newTriggerState

  .befDisplaySample(jaspResults, options)
  .befDisplayAll(jaspResults, options)

  if (!isFALSE(options[["barBatch"]]))
    .befDrawBatchBar(jaspResults, options)

  if (!isFALSE(options[["barAllSample"]]))
    .befDrawAllBar(jaspResults, options)

#######################Abundance#############################
  if (is.null(jaspResults[["abundanceContainer"]]))
    .befCreateAbundanceContainer(jaspResults, options)

  .befUpdateAbundanceState(jaspResults, options, should_sample)

  if (!isFALSE(options[["despAbundanceTable"]]))
    .befDisplayAbundanceTable(jaspResults, options)

  if (!isFALSE(options[["despAbundancePlot"]]))
    .befDisplayAbundancePlots(jaspResults, options)

#######################Bio Likelihood########################
  if (is.null(jaspResults[["bioLikeContainer"]]))
    .befCreateBioLikeContainer(jaspResults, options)

  if (is.null(jaspResults[["bioLikeState"]]))
    .befCreateBioLikeState(jaspResults, options)


  .befUpdateBioLike(jaspResults, options, should_sample)

  if (!isFALSE(options[["bioLikeTable"]]))
    .befDisplayBioLikeTable(jaspResults, options)

  if (!isFALSE(options[["showBioBF"]]))
    .befDisplayBioLikePz(jaspResults, options)

#######################Bio Posterior########################
  if (is.null(jaspResults[["bioPostContainer"]]))
    .befCreateBioPostContainer(jaspResults, options)

  if (!isFALSE(options[["beliefUpdateTable"]]))
    .befDisplayBeliefUpdateTable(jaspResults, options)

  if (!isFALSE(options[["beliefUpdatePlot"]]))
    .befDisplayBeliefUpdatePlot(jaspResults, options)

  if (!isFALSE(options[["evidAccPlot"]]))
    .befDisplayEvidAccPlot(jaspResults, options)

  if (!isFALSE(options[["compPostTable"]]) || !isFALSE(options[["compPostPlot"]]))
    .befDisplayCompPost(jaspResults, options)

  if (!isFALSE(options[["margAbundanceTable"]]))
    .befDisplayMargAbundanceTable(jaspResults, options)

  if (!isFALSE(options[["margAbundancePlot"]]))
    .befDisplayMargAbundancePlot(jaspResults, options)

  if (!isFALSE(options[["predTable"]]))
    .befDisplayPredTable(jaspResults, options)

  if (!isFALSE(options[["predPlot"]]))
    .befDisplayPredPlot(jaspResults, options)

}

#######################Intro##############################
  .befIntro <- function(jaspResults, options) {
    if (isFALSE(options[["introductoryText"]]))
      return()
    if (!is.null(jaspResults[["introductoryText"]]))
      return()

    text <- gettextf('<p>Do you still remember your career as a <b>biologist</b> and the adventure you had on the island in the Static module? If so, great! Because another adventure awaits! This time, you are heading to a <b>larger island</b> with potentially more animals, and even less idea of what you will encounter.</p>
                    <p>Just for a quick recap: your main task is still to estimate how many different species live on the island, by updating what you believed before based on what the data tell you.</p>
                    <p>But there is a twist. Some "big figures" up there told you that it would be great if you could also estimate the <b>abundance</b> of each species you encounter on the island, so that they would know which species most need protecting. Guess what? They also offered a huge reward for it!</p>
                    <p>So, ready for your new adventure? Dive into the module and give it a spin!</p>') #TODO

    jaspResults[["introductoryText"]] <- createJaspHtml(
      title        = gettext("Welcome!"),
      text         = text,
      dependencies = "introductoryText",
      position     = 1
    )
  }

#######################Data#############################
  .befCreateDataContainer <- function(jaspResults, options) {
    dataContainer <- createJaspContainer(title = gettext("Data"))
    dataContainer$dependOn(c("resetSample"))
    jaspResults[["dataContainer"]] <- dataContainer
  }

  .befCreateIsland <- function(jaspResults, options) {
    if (!is.null(jaspResults[["islandContainer"]]))
      return()

    if (!isFALSE(options[["selectisland"]]))
      set.seed(as.numeric(options[["islandid"]]))

    container <- createJaspContainer(title = gettext("Island"))
    container$dependOn(c("resetSample", "selectisland", "islandid"))
    jaspResults[["islandContainer"]] <- container

    com_spe   <- c("Pigeon", "Duck", "Cat", "Dog", "Fox", "Sparrow", "Honeybee", "Squirrel")
    uncom_spe <- c("Panda", "Kingfisher", "Sloth", "Capybara", "Lizard", "Eagle", "Koala", "Wombat")
    rare_spe  <- c("Unicorn", "Phoenix", "Dragon", "Griffin", "Sphinx", "Pegasus", "Chimera", "Godzilla")

    ncom   <- sample(3:6, 1)
    nuncom <- sample(1:3, 1)
    nrare  <- sample(0:1, 1)
    while (ncom + nuncom + nrare > 10) {
      ncom   <- sample(3:6, 1)
      nuncom <- sample(1:3, 1)
      nrare  <- sample(0:1, 1)
    }

    com_spe_sel   <- sample(com_spe,   ncom,   replace = FALSE)
    uncom_spe_sel <- sample(uncom_spe, nuncom, replace = FALSE)
    rare_spe_sel  <- sample(rare_spe,  nrare,  replace = FALSE)

    spe_island_prob <- c(rep(com_spe_sel, 25),
                        rep(uncom_spe_sel, 10),
                        rep(rare_spe_sel, 1))

    island_df <- data.frame(
      species = sample(spe_island_prob, 1000, replace = TRUE),
      id      = 1:1000
    )

    islandDeps <- c("resetSample", "selectisland", "islandid")

    islandState_j <- createJaspState(island_df)
    islandState_j$dependOn(islandDeps)
    container[["islandState"]] <- islandState_j

    seenIdsState_j <- createJaspState(c())
    seenIdsState_j$dependOn(islandDeps)
    container[["seenIdsState"]] <- seenIdsState_j

    sampleState_j <- createJaspState(list())
    sampleState_j$dependOn(islandDeps)
    container[["sampleState"]] <- sampleState_j

    abundanceHistoryState_j <- createJaspState(list())
    abundanceHistoryState_j$dependOn(islandDeps)
    container[["abundanceHistoryState"]] <- abundanceHistoryState_j
  }

  .befUpdateSample <- function(jaspResults, options) {
    if (is.null(jaspResults[["islandContainer"]]) ||
        is.null(jaspResults[["islandContainer"]][["islandState"]]) ||
        is.null(jaspResults[["islandContainer"]][["sampleState"]]))
      return()

    island_df    <- jaspResults[["islandContainer"]][["islandState"]]$object
    all_seen_ids <- jaspResults[["islandContainer"]][["seenIdsState"]]$object

    n_to_sample     <- as.numeric(options[["nsample"]])
    sampled_indices <- sample(1:1000, size = n_to_sample, replace = FALSE)
    batch_data      <- island_df[sampled_indices, ]

    status_vec <- character(n_to_sample)
    for (i in 1:nrow(batch_data)) {
      current_id <- batch_data$id[i]
      if (current_id %in% all_seen_ids) {
        status_vec[i] <- "Old"
      } else {
        status_vec[i] <- "New"
        all_seen_ids  <- c(all_seen_ids, current_id)
      }
    }

    batch_data$status <- status_vec

    all_sample_history <- jaspResults[["islandContainer"]][["sampleState"]]$object
    all_sample_history[[length(all_sample_history) + 1]] <- batch_data

    jaspResults[["islandContainer"]][["sampleState"]]$object  <- all_sample_history
    jaspResults[["islandContainer"]][["seenIdsState"]]$object <- all_seen_ids
  }

  .befGetLastBatch <- function(jaspResults) {
    ic <- jaspResults[["islandContainer"]]
    if (is.null(ic) || is.null(ic[["sampleState"]])) return(NULL)
    batches <- ic[["sampleState"]]$object
    if (length(batches) == 0) return(NULL)
    batches[[length(batches)]]
  }

  .befDisplaySample <- function(jaspResults, options) {
    dataContainer <- jaspResults[["dataContainer"]]
    if (is.null(dataContainer))
      return()
    if (!is.null(dataContainer[["batchTable"]]))
      return()

    batch_df <- .befGetLastBatch(jaspResults)
    if (is.null(batch_df) || nrow(batch_df) == 0)
      return()

    counts_matrix <- table(batch_df$species, batch_df$status)
    counts_df     <- as.data.frame.matrix(counts_matrix)

    if (!"New" %in% colnames(counts_df)) counts_df$New <- 0
    if (!"Old" %in% colnames(counts_df)) counts_df$Old <- 0

    new_vec   <- as.numeric(counts_df$New)
    old_vec   <- as.numeric(counts_df$Old)
    total_vec <- new_vec + old_vec

    clean_df <- data.frame(
      species = as.character(rownames(counts_df)),
      old     = old_vec,
      new     = new_vec,
      total   = total_vec,
      stringsAsFactors = FALSE
    )
    clean_df <- clean_df[order(clean_df$total, decreasing = TRUE), ]

    batchTable <- createJaspTable(title = gettext("The current batch (identity-aware)"))
    batchTable$dependOn(c("redrawTrigger", "resetSample"))

    batchTable$addColumnInfo(name = "species", title = gettext("Species"),           type = "string")
    batchTable$addColumnInfo(name = "old",     title = gettext("Old (re-captured)"), type = "integer")
    batchTable$addColumnInfo(name = "new",     title = gettext("New (first-time)"),  type = "integer")
    batchTable$addColumnInfo(name = "total",   title = gettext("Total"),             type = "integer")

    for (i in seq_len(nrow(clean_df))) {
      batchTable$addRows(list(
        species = clean_df$species[i],
        old     = clean_df$old[i],
        new     = clean_df$new[i],
        total   = clean_df$total[i]
      ))
    }
    batchTable$addRows(list(
      species = paste0("<b>", gettext("Total"), "</b>"),
      old     = sum(clean_df$old),
      new     = sum(clean_df$new),
      total   = sum(clean_df$total)
    ))

    dataContainer[["batchTable"]] <- batchTable
  }

  .befDisplayAll <- function(jaspResults, options) {
    dataContainer <- jaspResults[["dataContainer"]]
    if (is.null(dataContainer))
      return()
    if (!is.null(dataContainer[["allTable"]]))
      return()

    if (is.null(jaspResults[["islandContainer"]]) ||
        is.null(jaspResults[["islandContainer"]][["sampleState"]]))
      return()

    all_sample_list <- jaspResults[["islandContainer"]][["sampleState"]]$object

    if (length(all_sample_list) == 0)
      return()

    full_df        <- do.call(rbind, all_sample_list)
    new_animals_df <- full_df[full_df$status == "New", ]

    if (nrow(new_animals_df) == 0)
      return()

    counts_table <- table(new_animals_df$species)
    all_counts <- data.frame(
      species = as.character(names(counts_table)),
      Freq    = as.numeric(counts_table),
      stringsAsFactors = FALSE
    )
    all_counts <- all_counts[order(all_counts$Freq, decreasing = TRUE), ]

    allTable <- createJaspTable(title = gettext("The whole sample (unique individuals)"))
    allTable$dependOn(c("redrawTrigger", "resetSample"))

    allTable$addColumnInfo(name = "species", title = gettext("Species"),               type = "string")
    allTable$addColumnInfo(name = "counts",  title = gettext("Number of individuals"), type = "integer")

    for (i in seq_len(nrow(all_counts))) {
      allTable$addRows(list(
        species = all_counts$species[i],
        counts  = all_counts$Freq[i]
      ))
    }
    allTable$addRows(list(
      species = paste0("<b>", gettext("Total"), "</b>"),
      counts  = sum(all_counts$Freq)
    ))
    allTable$addFootnote(message = gettextf("Total unique species discovered: %d", nrow(all_counts)))

    dataContainer[["allTable"]] <- allTable
  }

  .befDrawBatchBar <- function(jaspResults, options) {
    dataContainer <- jaspResults[["dataContainer"]]
    if (is.null(dataContainer))
      return()
    if (!is.null(dataContainer[["batchBar"]]))
      return()

    batch_df <- .befGetLastBatch(jaspResults)
    if (is.null(batch_df) || nrow(batch_df) == 0)
      return()

    counts_matrix <- table(batch_df$species, batch_df$status)
    counts_df     <- as.data.frame.matrix(counts_matrix)

    if (!"New" %in% colnames(counts_df)) counts_df$New <- 0
    if (!"Old" %in% colnames(counts_df)) counts_df$Old <- 0

    clean_df <- data.frame(
      species = as.character(rownames(counts_df)),
      old     = as.numeric(counts_df$Old),
      new     = as.numeric(counts_df$New),
      total   = as.numeric(counts_df$Old) + as.numeric(counts_df$New),
      stringsAsFactors = FALSE
    )
    clean_df <- clean_df[order(clean_df$total, decreasing = TRUE), ]

    plot_data <- data.frame(
      Species = rep(clean_df$species, 2),
      Count   = c(clean_df$old, clean_df$new),
      Type    = rep(c("Old (Re-captured)", "New (First-time)"), each = nrow(clean_df))
    )
    plot_data$Species <- factor(plot_data$Species, levels = rev(clean_df$species))

    yMax <- max(clean_df$total, na.rm = TRUE)
    if (!is.finite(yMax) || yMax <= 0) yMax <- 1
    yBreaks <- scales::breaks_pretty()(c(0, yMax))
    yBreaks <- yBreaks[yBreaks == floor(yBreaks)]
    yUpper  <- max(yBreaks)

    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Species, y = Count, fill = Type)) +
      ggplot2::geom_bar(stat = "identity", width = 0.7) +
      ggplot2::scale_fill_manual(values = c("New (First-time)" = "#3498db",
                                            "Old (Re-captured)" = "#95a5a6")) +
      ggplot2::scale_y_continuous(
        limits = c(0, yUpper),
        breaks = yBreaks,
        expand = ggplot2::expansion(mult = c(0, 0.08))
      ) +
      ggplot2::coord_flip() +
      ggplot2::labs(x = "Species Name", y = "Number of Individuals", fill = "Status") +
      ggplot2::theme(legend.position = "bottom",
                     plot.margin = ggplot2::margin(t = 5, r = 15, b = 15, l = 5)) +
      jaspGraphs::geom_rangeframe() +
      jaspGraphs::themeJaspRaw()

    batchPlot <- createJaspPlot(title = gettext("Species frequencies (current batch)"),
                                width = 600, height = max(320, 60 + 26 * nrow(clean_df)))
    batchPlot$dependOn(c("barBatch", "redrawTrigger", "resetSample"))
    batchPlot$plotObject <- p

    dataContainer[["batchBar"]] <- batchPlot
  }

  .befDrawAllBar <- function(jaspResults, options) {
    dataContainer <- jaspResults[["dataContainer"]]
    if (is.null(dataContainer))
      return()
    if (!is.null(dataContainer[["allBar"]]))
      return()

    if (is.null(jaspResults[["islandContainer"]]) ||
        is.null(jaspResults[["islandContainer"]][["sampleState"]]))
      return()

    all_sample_list <- jaspResults[["islandContainer"]][["sampleState"]]$object

    if (length(all_sample_list) == 0)
      return()

    full_df               <- do.call(rbind, all_sample_list)
    unique_individuals_df <- full_df[!duplicated(full_df$id), ]
    n_species             <- length(unique(unique_individuals_df$species))

    allPlot <- createJaspPlot(title = gettext("Species frequencies (overall)"),
                              width = 600, height = max(320, 60 + 26 * n_species))
    allPlot$dependOn(c("barAllSample", "redrawTrigger", "resetSample"))
    allPlot$plotObject <- .befFillDataBarPlot(as.character(unique_individuals_df$species))

    dataContainer[["allBar"]] <- allPlot
  }


#######################Models#############################
  .befGetModel <- function(options) {
    list(
      type          = options[["modelType"]],
      pointPriorN   = options[["modelPointPriorN"]],
      poissonlambda = options[["modelPoissonLambda"]],
      nbMu          = options[["modelNbMu"]],
      nbPhi         = options[["modelNbPhi"]],
      minimum       = options[["modelMin"]],
      maximum       = options[["modelMax"]],
      showExplain   = options[["modelShowExplain"]],
      showPlot      = options[["modelShowPlot"]]
    )
  }
  

  .befModelDepends <- c("modelType",
                        "modelPointPriorN", "modelPoissonLambda",
                        "modelNbMu", "modelNbPhi",
                        "modelMin", "modelMax",
                        "modelShowExplain", "modelShowPlot")

  .befCreateModelContainer <- function(jaspResults, options) {
    modelContainer <- createJaspContainer(title = gettext("Model"))
    modelContainer$dependOn(.befModelDepends)
    jaspResults[["modelContainer"]] <- modelContainer
  }

  .befDisplayModel <- function(jaspResults, options) {
    modelContainer <- jaspResults[["modelContainer"]]
    if (is.null(modelContainer))
      return()
    if (!is.null(modelContainer[["modelTable"]]))
      return()

    m <- .befGetModel(options)

    modelTable <- createJaspTable(title = gettext("Model"))
    modelTable$addColumnInfo(name = "distribution", title = gettext("Distribution"), type = "string")
    modelTable$addColumnInfo(name = "para",         title = gettext("Parameter"),    type = "string")
    modelTable$addColumnInfo(name = "min",          title = gettext("Min"),          type = "integer")
    modelTable$addColumnInfo(name = "max",          title = gettext("Max"),          type = "integer")

    if (m[["type"]] == "point") {
      para <- paste0("N = ", m[["pointPriorN"]])
      min  <- ""
      max  <- ""
    } else if (m[["type"]] == "uniform") {
      para <- ""
      min  <- m[["minimum"]]
      max  <- m[["maximum"]]
    } else if (m[["type"]] == "poisson") {
      para <- paste0("λ = ", m[["poissonlambda"]])
      min  <- m[["minimum"]]
      max  <- m[["maximum"]]
    } else if (m[["type"]] == "negbino") {
      para <- paste0("μ = ", m[["nbMu"]], ", ϕ = ", m[["nbPhi"]])
      min  <- m[["minimum"]]
      max  <- m[["maximum"]]
    }

    modelTable$addRows(list(
      distribution = m[["type"]],
      para         = para,
      min          = as.character(min),
      max          = as.character(max)
    ))
    modelContainer[["modelTable"]] <- modelTable

    if (isTRUE(m[["showExplain"]])) {
      htmlContent <- paste0("<b>", gettext("Explanation:"), "</b><br>", .befDescText(m))
      modelContainer[["html_desc"]] <- createJaspHtml(htmlContent)
      modelContainer[["html_desc"]]$maxWidth <- "600px"
    }
  }

  .befSummaryModel <- function(jaspResults, options) {
    modelContainer <- jaspResults[["modelContainer"]]
    if (is.null(modelContainer))
      return()
    if (!is.null(modelContainer[["priorSumTable"]]))
      return()

    priorSumTable <- createJaspTable(title = gettext("Prior summary statistics"))
    priorSumTable$dependOn(c("priorMean", "priorMedian", "priorMode",
                            "priorSD", "variableBetween", "priorUserMin", "priorUserMax"))


    stAllOptions <- c("priorMean", "priorMedian", "priorMode", "priorSD", "variableBetween")
    idx_checked  <- which(as.logical(options[stAllOptions]))

    if (length(idx_checked) == 0) return()

    stAllTitles <- c(
      gettext("Mean"),
      gettext("Median"),
      gettext("Mode"),
      gettext("SD"),
      gettextf("P(%1$s ≤ S ≤ %2$s)", options[["priorUserMin"]], options[["priorUserMax"]])
    )

    for (i in idx_checked)
      priorSumTable$addColumnInfo(name = stAllOptions[i], title = stAllTitles[i], type = "number")

    modelContainer[["priorSumTable"]] <- priorSumTable

    m    <- .befGetModel(options)
    mPMF <- .befGetModelPMF(m)

    stat_vec <- list(
      priorMean       = .befCalculateMean(mPMF),
      priorMedian     = .befCalculateMedian(mPMF),
      priorMode       = .befCalculateMode(mPMF),
      priorSD         = .befCalculateSD(mPMF),
      variableBetween = .befCalculateProbability(mPMF,
                                                 as.numeric(options[["priorUserMin"]]),
                                                 as.numeric(options[["priorUserMax"]]))
    )

    m_sum_list <- list()
    for (i in idx_checked)
      m_sum_list[[stAllOptions[i]]] <- stat_vec[[stAllOptions[i]]]

    priorSumTable$addRows(m_sum_list)
  }

  .befPlotModel <- function(jaspResults, options) {
    modelContainer <- jaspResults[["modelContainer"]]
    if (is.null(modelContainer))
      return()
    if (!is.null(modelContainer[["priorPlot"]]))
      return()

    m <- .befGetModel(options)
    if (!isTRUE(m[["showPlot"]]))
      return()

    mPMF  <- .befGetModelPMF(m)
    pPlot <- createJaspPlot(title = gettext("Prior distribution"),
                            width = 450, height = 300)
    pPlot$dependOn(.befModelDepends)

    dfPMF   <- data.frame(s = mPMF[["s"]], p = mPMF[["p"]])
    s_min   <- min(dfPMF[["s"]])
    s_max   <- max(dfPMF[["s"]])

    yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, max(dfPMF[["p"]], na.rm = TRUE)))
    yMax    <- min(1, max(yBreaks))

    plotObj <- ggplot2::ggplot(dfPMF, ggplot2::aes(x = s, y = p)) +
      ggplot2::geom_bar(stat = "identity", fill = "#4DA3FF", width = 0.7) +
      ggplot2::labs(x = gettext("Species Count (S)"), y = gettext("Prior Probability")) +
      ggplot2::scale_x_continuous(limits = c(s_min - 0.5, s_max + 0.5), breaks = s_min:s_max,
                                  expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
      ggplot2::scale_y_continuous(limits = c(0, yMax),
                                  breaks = yBreaks[yBreaks <= yMax],
                                  expand = ggplot2::expansion(mult = c(0, 0.08))) +
      jaspGraphs::geom_rangeframe() +
      jaspGraphs::themeJaspRaw() +
      ggplot2::theme(plot.margin = ggplot2::margin(t = 5, r = 15, b = 15, l = 5))

    pPlot$plotObject <- plotObj
    modelContainer[["priorPlot"]] <- pPlot
  }

#######################Abundance#############################
  .befCreateAbundanceContainer <- function(jaspResults, options) {
    abundanceContainer <- createJaspContainer(title = gettext("Abundance"))
    abundanceContainer$dependOn(c("resetSample"))
    jaspResults[["abundanceContainer"]] <- abundanceContainer
  }

  .befUpdateAbundanceState <- function(jaspResults, options, should_sample) {
    if (is.null(jaspResults))
      return()

    abundanceS    <- 1:10
    abundanceSMax <- 10

    abundanceState_j <- jaspResults[["abundanceState"]]

    if (is.null(abundanceState_j)) {
      alpha_matrix <- matrix(NA, nrow = abundanceSMax, ncol = length(abundanceS))

      for (col_idx in seq_along(abundanceS))
        alpha_matrix[1:abundanceS[col_idx], col_idx] <- 1

      initial_names <- paste0("unknown_", 1:abundanceSMax)
      rownames(alpha_matrix) <- initial_names
      colnames(alpha_matrix) <- paste0("S = ", abundanceS)

      abundanceState <- list(
        alphaMatrix  = alpha_matrix,
        speciesNames = initial_names,
        s_values     = abundanceS,
        currentK     = 0
      )

      abundanceState_j <- createJaspState(abundanceState)
      abundanceState_j$dependOn(options = c("resetSample"))
      jaspResults[["abundanceState"]] <- abundanceState_j

      betaParamsDf <- .befCalculateBeta(abundanceState)
      jaspResults[["betaParamState"]] <- createJaspState(betaParamsDf)
      jaspResults[["betaParamState"]]$dependOn(options = c("resetSample"))
    }

    if (is.null(jaspResults[["betaParamState"]])) {
      betaParamsDf <- .befCalculateBeta(abundanceState_j$object)
      bState <- createJaspState(betaParamsDf)
      bState$dependOn(options = c("resetSample"))
      jaspResults[["betaParamState"]] <- bState
    }

    if (!should_sample)
      return()

    abundanceState    <- abundanceState_j$object
    abundanceStateOld <- abundanceState

    abundanceState <- .befUpdateAbundance(jaspResults, abundanceState)
    abundanceState_j$object <- abundanceState

    if (is.null(jaspResults[["preUpdateAbundanceState"]])) {
      puas <- createJaspState(abundanceStateOld)
      puas$dependOn(options = c("resetSample"))
      jaspResults[["preUpdateAbundanceState"]] <- puas
    } else {
      jaspResults[["preUpdateAbundanceState"]]$object <- abundanceStateOld
    }


    betaParamsDf    <- .befCalculateBeta(abundanceState)
    oldbetaParamsDf <- .befCalculateBeta(abundanceStateOld)

    jaspResults[["betaParamState"]]$object <- betaParamsDf

    if (is.null(jaspResults[["oldbetaParamState"]])) {
      jaspResults[["oldbetaParamState"]] <- createJaspState(oldbetaParamsDf)
      jaspResults[["oldbetaParamState"]]$dependOn(options = c("resetSample"))
    } else {
      jaspResults[["oldbetaParamState"]]$object <- oldbetaParamsDf
    }

    .befAppendAbundanceHistory(jaspResults, betaParamsDf)
  }

  .befUpdateAbundance <- function(jaspResults, abundanceState) {
    batch_df      <- .befGetLastBatch(jaspResults)
    counts_matrix <- table(batch_df$species, batch_df$status)
    counts_df     <- as.data.frame.matrix(counts_matrix)

    if (is.null(counts_df) || nrow(counts_df) == 0)
      return(abundanceState)

    if (!"New" %in% colnames(counts_df)) counts_df$New <- 0
    if (!"Old" %in% colnames(counts_df)) counts_df$Old <- 0

    new_names_in_batch <- rownames(counts_df)[counts_df$New > 0]
    truly_new_names <- setdiff(
      new_names_in_batch,
      abundanceState$speciesNames[!grepl("^unknown_", abundanceState$speciesNames)]
    )

    if (length(truly_new_names) > 0) {
      for (s_name in truly_new_names) {
        abundanceState$currentK <- abundanceState$currentK + 1
        k_idx <- abundanceState$currentK
        if (k_idx <= length(abundanceState$speciesNames))
          abundanceState$speciesNames[k_idx] <- s_name
      }
      rownames(abundanceState$alphaMatrix) <- abundanceState$speciesNames

      keep_indices <- which(abundanceState$s_values >= abundanceState$currentK)
      abundanceState$alphaMatrix <- abundanceState$alphaMatrix[, keep_indices, drop = FALSE]
      abundanceState$s_values    <- abundanceState$s_values[keep_indices]
    }

    if (sum(counts_df$New) == 0)
      return(abundanceState)

    for (r in rownames(counts_df)) {
      nnew <- counts_df[r, "New"]
      if (nnew > 0)
        abundanceState$alphaMatrix[r, ] <- abundanceState$alphaMatrix[r, ] + nnew
    }

    row_total <- rowSums(abundanceState$alphaMatrix, na.rm = TRUE)
    sort_idx  <- order(row_total, decreasing = TRUE)
    abundanceState$alphaMatrix  <- abundanceState$alphaMatrix[sort_idx, , drop = FALSE]
    abundanceState$speciesNames <- rownames(abundanceState$alphaMatrix)

    return(abundanceState)
  }

  .befAppendAbundanceHistory <- function(jaspResults, betaParamsDf) {
    ic <- jaspResults[["islandContainer"]]
    if (is.null(ic) || is.null(ic[["abundanceHistoryState"]]))
      return()

    history <- ic[["abundanceHistoryState"]]$object

    entry <- betaParamsDf[!grepl("^unknown_", betaParamsDf$Species),
                          c("Species", "S_Hypothesis", "alpha_a", "beta_b")]
    entry$mean  <- entry$alpha_a / (entry$alpha_a + entry$beta_b)
    entry$batch <- length(history) + 1

    history[[length(history) + 1]] <- entry
    ic[["abundanceHistoryState"]]$object <- history
  }

  .befDisplayAbundanceTable <- function(jaspResults, options) {
    betaParamState <- jaspResults[["betaParamState"]]
    if (is.null(betaParamState))
      return()

    abundanceContainer <- jaspResults[["abundanceContainer"]]
    if (is.null(abundanceContainer))
      return()

    betaParamsDf    <- betaParamState$object
    all_species     <- unique(as.character(betaParamsDf$Species))
    known_species   <- all_species[!grepl("^unknown_", all_species)]
    unknown_species <- all_species[grepl("^unknown_", all_species)]

    if (!is.null(abundanceContainer[["abundanceTable"]]))
      return()

    abundanceTable <- createJaspTable(title = "Estimated species abundance")
    abundanceTable$dependOn(c("redrawTrigger", "resetSample", "despAbundanceTable", "abundanceStat"))
    abundanceTable$addColumnInfo(name = "s_label", title = "Hypothetical S", type = "string")

    for (spec in known_species)
      abundanceTable$addColumnInfo(name = spec, title = spec, type = "number", format = "dp:3")

    abundanceTable$addColumnInfo(name = "unknown_combined", title = "Unknown",
                                type = "number", format = "dp:3")

    abundanceContainer[["abundanceTable"]] <- abundanceTable

    a <- betaParamsDf$alpha_a
    b <- betaParamsDf$beta_b

    selectedStat <- options[["abundanceStat"]]
    if (selectedStat == "median") {
      betaParamsDf$displayVal <- qbeta(0.5, a, b)
    } else if (selectedStat == "mode") {
      betaParamsDf$displayVal <- ifelse(a > 1 & b > 1, (a - 1) / (a + b - 2), NA)
    } else if (selectedStat == "mean") {
      betaParamsDf$displayVal <- a / (a + b)
    }

    s_values <- sort(unique(betaParamsDf$S_Hypothesis))

    for (s_val in s_values) {
      current_subset <- betaParamsDf[betaParamsDf$S_Hypothesis == s_val, ]
      row_list <- list(s_label = paste0("S = ", s_val))

      for (spec in known_species) {
        spec_row <- current_subset[current_subset$Species == spec, ]
        if (nrow(spec_row) > 0 && !is.na(spec_row$displayVal[1]))
          row_list[[spec]] <- spec_row$displayVal[1]
      }

      unknown_rows <- current_subset[grepl("^unknown_", current_subset$Species), ]
      row_list[["unknown_combined"]] <- if (nrow(unknown_rows) > 0)
        sum(unknown_rows$displayVal, na.rm = TRUE)
      else
        0

      abundanceTable$addRows(row_list)
    }
  }

  .befDisplayAbundancePlots <- function(jaspResults, options) {
    abundanceContainer <- jaspResults[["abundanceContainer"]]
    if (is.null(abundanceContainer))
      return()

    betaParamState    <- jaspResults[["betaParamState"]]
    oldbetaParamState <- jaspResults[["oldbetaParamState"]]
    abundanceState_j  <- jaspResults[["abundanceState"]]

    if (is.null(betaParamState) || is.null(oldbetaParamState) || is.null(abundanceState_j))
      return()

    betaParamsDf    <- betaParamState$object
    oldbetaParamsDf <- oldbetaParamState$object
    abundanceState  <- abundanceState_j$object

    s_values      <- abundanceState[["s_values"]]
    all_species   <- unique(as.character(betaParamsDf$Species))
    known_species <- all_species[!grepl("^unknown_", all_species)]

    if (is.null(abundanceContainer[["abundancePlotsContainer"]])) {
      abundancePlotsContainer <- createJaspContainer(title = "Estimated Species Abundance Plots")
      abundancePlotsContainer$dependOn(c("resetSample", "redrawTrigger",
                                        "despAbundancePlot", "despAbundancePlotExp",
                                        "despAbundancePlotEst", "abundanceStat",
                                        "despAbundancePlotSameScale"))
      abundanceContainer[["abundancePlotsContainer"]] <- abundancePlotsContainer
    } else {
      abundancePlotsContainer <- abundanceContainer[["abundancePlotsContainer"]]
    }

    densityCache <- list()
    for (s_val in s_values) {
      plotName <- paste0("S = ", s_val)
      if (!is.null(abundancePlotsContainer[[plotName]]))
        next

      for (spe in known_species) {
        d <- .befComputeAbundanceDensity(s_val, spe, betaParamsDf, oldbetaParamsDf)
        if (!is.null(d))
          densityCache[[paste0(s_val, "_", spe)]] <- d
      }
    }

    globalYMax <- NULL
    if (isTRUE(options[["despAbundancePlotSameScale"]]) && length(densityCache) > 0)
      globalYMax <- max(vapply(densityCache, function(d) d$maxDensity, numeric(1)))

    for (s_val in s_values) {
      plotName <- paste0("S = ", s_val)

      if (!is.null(abundancePlotsContainer[[plotName]]))
        next

      plot_list <- list()
      for (spe in known_species) {
        d <- densityCache[[paste0(s_val, "_", spe)]]
        if (!is.null(d))
          plot_list[[paste0(s_val, "_", spe)]] <- .befPlotAbundance(spe, d, options, globalYMax)
      }

      combined <- cowplot::plot_grid(plotlist = plot_list, nrow = 1)

      sPlot <- createJaspPlot(
        title  = paste0("S = ", s_val),
        width  = 400 * length(known_species),
        height = 300
      )
      sPlot$dependOn(c("despAbundancePlot", "despAbundancePlotExp",
                       "despAbundancePlotEst", "abundanceStat",
                       "despAbundancePlotSameScale"))
      sPlot$plotObject <- combined

      abundancePlotsContainer[[plotName]] <- sPlot
    }
  }

  .befDisplayAbundanceTrajectory <- function(jaspResults, options) {
    ic <- jaspResults[["islandContainer"]]
    if (is.null(ic) || is.null(ic[["abundanceHistoryState"]]))
      return()

    history <- ic[["abundanceHistoryState"]]$object

    if (length(history) < 2)
      return()

    if (is.null(jaspResults[["abundanceTrajContainer"]])) {
      trajContainer <- createJaspContainer(title = gettext("Species Abundance Trajectory"))
      trajContainer$dependOn(c("redrawTrigger", "resetSample", "despAbundanceTraj"))
      jaspResults[["abundanceTrajContainer"]] <- trajContainer
    } else {
      trajContainer <- jaspResults[["abundanceTrajContainer"]]
    }

    history_df <- do.call(rbind, history)
    s_values   <- sort(unique(history_df$S_Hypothesis))

    for (s_val in s_values) {
      plotName <- paste0("Traj_S = ", s_val)

      if (!is.null(trajContainer[[plotName]]))
        next

      subset_df <- history_df[history_df$S_Hypothesis == s_val, ]

      if (nrow(subset_df) == 0)
        next

      yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, max(subset_df$mean, na.rm = TRUE)))
      yMax    <- min(1, max(yBreaks))

      p <- ggplot2::ggplot(subset_df,
                          ggplot2::aes(x = batch, y = mean, color = Species, group = Species)) +
        ggplot2::geom_line(linewidth = 0.8) +
        ggplot2::geom_point(size = 2) +
        ggplot2::scale_x_continuous(breaks = function(x) {
          b <- scales::pretty_breaks(n = 6)(x)
          b[b == floor(b) & b >= 1]
        }) +
        ggplot2::scale_y_continuous(limits = c(0, yMax),
                                    breaks = yBreaks[yBreaks <= yMax],
                                    expand = ggplot2::expansion(mult = c(0, 0.1))) +
        ggplot2::labs(
          x     = gettext("Batch"),
          y     = gettext("Mean Abundance Estimate"),
          color = gettext("Species")
        ) +
        jaspGraphs::geom_rangeframe() +
        jaspGraphs::themeJaspRaw() +
        ggplot2::theme(plot.margin = ggplot2::margin(t = 5, r = 15, b = 15, l = 5))

      trajPlot <- createJaspPlot(
        title  = gettextf("Abundance trajectory (S = %d)", s_val),
        width  = 500,
        height = 350
      )
      trajPlot$dependOn("despAbundanceTraj")
      trajPlot$plotObject <- p

      trajContainer[[plotName]] <- trajPlot
    }
  }


###################Bio Likelihood#############################
  .befCreateBioLikeContainer <- function(jaspResults, options) {
    bioLikeContainer <- createJaspContainer(title = gettext("Likelihood"))
    bioLikeContainer$dependOn(c("resetSample"))
    jaspResults[["bioLikeContainer"]] <- bioLikeContainer
  }

  .befCreateBioLikeState <- function(jaspResults, options) {
    if (is.null(jaspResults) || is.null(jaspResults[["islandContainer"]]))
      return()

    S_values       <- 1:10
    like_df        <- data.frame(S = S_values, stringsAsFactors = FALSE)
    like_df[["cumulative"]] <- 0

    bioLikeState <- list(
      likeDF      = like_df,
      batch_count = 0,
      seen_species = c()
    )

    bioLikeState_j <- createJaspState(bioLikeState)
    bioLikeState_j$dependOn(options = c("resetSample"))
    jaspResults[["bioLikeState"]] <- bioLikeState_j
  }

  .befUpdateBioLike <- function(jaspResults, options, should_sample) {
    if (is.null(jaspResults) || is.null(jaspResults[["islandContainer"]]))
      return()

    bioLikeState_j <- jaspResults[["bioLikeState"]]

    if (!should_sample)
      return()

    batch_data <- .befGetLastBatch(jaspResults)
    if (is.null(batch_data) || nrow(batch_data) == 0)
      return()

    preUpdateAbundanceState_j <- jaspResults[["preUpdateAbundanceState"]]
    if (is.null(preUpdateAbundanceState_j))
      return()
    abundanceState <- preUpdateAbundanceState_j$object

    bioLikeState <- bioLikeState_j$object
    batch_count  <- bioLikeState$batch_count + 1
    like_df      <- bioLikeState$likeDF
    seen_species <- bioLikeState$seen_species


    new_animals <- batch_data$species[batch_data$status == "New"]
    if (length(new_animals) == 0) {
      col_name                  <- paste0("batch_", batch_count)
      like_df[[col_name]]       <- rep(0, nrow(like_df))
      batch_cols                <- grep("^batch_", colnames(like_df), value = TRUE)
      like_df[["cumulative"]]   <- rowSums(like_df[, batch_cols, drop = FALSE], na.rm = FALSE)
      bioLikeState$likeDF       <- like_df
      bioLikeState$batch_count  <- batch_count
      bioLikeState_j$object     <- bioLikeState
      return()
    }

    new_animals_counts <- table(new_animals)
    new_animals_names <- names(new_animals_counts)
    unseen_species <- setdiff(new_animals_names, seen_species)

    abundanceState_local <- abundanceState

    result               <- .befCalculateLikelihoodNew(abundanceState_local, bioLikeState, unseen_species)
    lik_new              <- result$likelihood
    abundanceState_local <- result$abundanceState

    for (name in unseen_species) {
      new_animals_counts[name] <- new_animals_counts[name] - 1
    }

    if (sum(new_animals_counts) == 0) {
      col_name                <- paste0("batch_", batch_count)
      like_df[[col_name]]     <- lik_new
      batch_cols              <- grep("^batch_", colnames(like_df), value = TRUE)
      like_df[["cumulative"]] <- rowSums(like_df[, batch_cols, drop = FALSE], na.rm = FALSE)
      bioLikeState$likeDF       <- like_df
      bioLikeState$batch_count  <- batch_count
      bioLikeState$seen_species <- union(seen_species, unseen_species)
      bioLikeState_j$object     <- bioLikeState
      return()
    }

    result               <- .befCalculateLikelihoodOld(abundanceState_local, new_animals_counts)
    lik_old              <- result$likelihood

    lik_batch <- lik_new + lik_old

    col_name                <- paste0("batch_", batch_count)
    like_df[[col_name]]     <- lik_batch
    batch_cols              <- grep("^batch_", colnames(like_df), value = TRUE)
    like_df[["cumulative"]] <- rowSums(like_df[, batch_cols, drop = FALSE], na.rm = FALSE)
    bioLikeState$likeDF       <- like_df
    bioLikeState$batch_count  <- batch_count
    bioLikeState$seen_species <- union(seen_species, unseen_species)
    bioLikeState_j$object     <- bioLikeState
  }

  .befDisplayBioLikeTable <- function(jaspResults, options) {
    bioLikeState_j <- jaspResults[["bioLikeState"]]
    if (is.null(bioLikeState_j))
      return()

    bioLikeContainer <- jaspResults[["bioLikeContainer"]]
    if (is.null(bioLikeContainer))
      return()

    if (!is.null(bioLikeContainer[["bioLikeTable"]]))
      return()

    like_df <- bioLikeState_j$object$likeDF
    batch_count <- bioLikeState_j$object$batch_count

    likeTable <- createJaspTable(title = gettext("Biodiversity likelihood"))
    likeTable$dependOn(c("redrawTrigger", "resetSample",
                          "bioLikeTable", "bioLikeTableDisp", "bioLikeTableHide"))

    likeTable$addColumnInfo(name = "S",             title = gettext("Hypothetical S"),    type = "integer")
    likeTable$addColumnInfo(name = "batch_biolike", title = gettext("Sample likelihood"), type = "number")
    likeTable$addColumnInfo(name = "all_biolike",   title = gettext("Overall likelihood"), type = "number")

    n_rows <- nrow(like_df)

    if (batch_count == 0) {
      likeTable$setError(gettext("No data, sample some animals to see likelihood."))
      bioLikeContainer[["bioLikeTable"]] <- likeTable
      return()
    }

    last_batch_col <- paste0("batch_", batch_count)

    if (!isFALSE(options[["bioLikeTableHide"]]))
      like_df <- like_df[is.finite(like_df$cumulative), ]

    batch_biolike_all <- like_df[[last_batch_col]]
    all_biolike_all   <- like_df$cumulative

    if (is.null(batch_biolike_all) || !is.numeric(batch_biolike_all))
      batch_biolike_all <- rep(NA_real_, nrow(like_df))

    if (options[["bioLikeTableDisp"]] == "raw") {
      batch_biolike_all <- exp(batch_biolike_all)
      all_biolike_all   <- exp(as.numeric(all_biolike_all))
    }

    for (i in seq_len(nrow(like_df))) {
      row_list <- list(S = like_df$S[i], batch_biolike = batch_biolike_all[i], all_biolike = all_biolike_all[i])
      likeTable$addRows(row_list)
    }

    bioLikeContainer[["bioLikeTable"]] <- likeTable
  }

  .befDisplayBioLikePz <- function(jaspResults, options) {
    bioLikeContainer <- jaspResults[["bioLikeContainer"]]
    if (is.null(bioLikeContainer))
      return()

    bioLikeState_j <- jaspResults[["bioLikeState"]]
    if (is.null(bioLikeState_j))
      return()

    bioLikeState <- bioLikeState_j$object
    if (bioLikeState$batch_count == 0)
      return()

    if (is.null(bioLikeContainer[["bioBFPizzaContainer"]])) {
      bioBFPizzaContainer <- createJaspContainer(title = gettext("Bayes Factor"))
      bioBFPizzaContainer$dependOn(c("resetSample", "redrawTrigger",
                                      "showBioBF", "bioBFPairsSpe", "bioLikeBFData"))
      bioLikeContainer[["bioBFPizzaContainer"]] <- bioBFPizzaContainer
    }

    bioBFPizzaContainer <- bioLikeContainer[["bioBFPizzaContainer"]]

    like_df     <- bioLikeState$likeDF
    batch_count <- bioLikeState$batch_count
    keys        <- paste0("h", 1:10)
    disp_labels <- c("H₁", "H₂", "H₃", "H₄", "H₅",
                    "H₆", "H₇", "H₈", "H₉", "H₁₀")

    ll_vec <- if (isTRUE(options[["bioLikeBFData"]] == "batch"))
      like_df[[paste0("batch_", batch_count)]]
    else
      like_df$cumulative

    lookup       <- stats::setNames(ll_vec, keys)
    key_to_label <- stats::setNames(disp_labels, keys)

    pairs <- options[["bioBFPairsSpe"]]
    if (length(pairs) == 0)
      return()

    comp_i <- 0
    for (pair in pairs) {
      if (length(pair) < 2) next
      keyA <- pair[[1]]
      keyB <- pair[[2]]
      if (keyA == "" || keyB == "") next
      comp_i <- comp_i + 1

      plotName <- paste0("Comparison_", comp_i)
      if (!is.null(bioBFPizzaContainer[[plotName]])) next

      llA <- lookup[keyA]
      llB <- lookup[keyB]
      if (is.na(llA) || is.na(llB)) next

      bf_ab <- exp(llA - llB)
      bf_ba <- exp(llB - llA)

      if (!is.finite(llA) && !is.finite(llB)) {
        prop_a <- 0.5; prop_b <- 0.5
      } else {
        prop_a <- 1 / (1 + exp(-(llA - llB)))
        prop_b <- 1 - prop_a
      }

      dispA <- key_to_label[[keyA]]
      dispB <- key_to_label[[keyB]]

      note <- if (!is.finite(llA) && !is.finite(llB))
        gettext("Both hypotheses are ruled out by the data.")
      else if (!is.finite(llA))
        gettextf("%s is ruled out by the data; it overwhelmingly favors %s.", dispA, dispB)
      else if (!is.finite(llB))
        gettextf("%s is ruled out by the data; it overwhelmingly favors %s.", dispB, dispA)
      else if (bf_ab > 1)
        gettextf("The data are %.2f times more likely under %s than under %s.", bf_ab, dispA, dispB)
      else if (bf_ab == 1)
        gettext("The data are equally likely under both hypotheses.")
      else
        gettextf("The data are %.2f times more likely under %s than under %s.", bf_ba, dispB, dispA)

      note <- paste(strwrap(note, width = 30), collapse = "\n")

      bfPlot           <- createJaspPlot(title = gettextf("Comparison %d: %s vs. %s", comp_i, dispA, dispB),
                                        width = 338, height = 300)
      bfPlot$position  <- comp_i

      p <- jaspGraphs::drawBFpizza(
        c(prop_a, prop_b),
        labels = c(paste0("data | ", dispB), paste0("data | ", dispA))
      ) +
        ggplot2::labs(caption = note) +
        ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0.5, size = 11))

      bfPlot$plotObject <- cowplot::plot_grid(p)
      bioBFPizzaContainer[[plotName]] <- bfPlot
    }
  }

#######################Bio Posterior#############################
  .befCreateBioPostContainer <- function(jaspResults, options) {
    bioPostContainer <- createJaspContainer(title = gettext("Posterior belief"))
    bioPostContainer$dependOn(c("resetSample"))
    jaspResults[["bioPostContainer"]] <- bioPostContainer
  }
  
  .befDisplayBeliefUpdateTable <- function(jaspResults, options) {
    bioPostContainer <- jaspResults[["bioPostContainer"]]
    if (is.null(bioPostContainer))
      return()

    stat     <- options[["beliefUpdateTableStat"]]
    hide_imp <- !isFALSE(options[["beliefUpdateTableHide"]])

    beliefTable <- createJaspTable(title = gettext("Belief update"))
    beliefTable$dependOn(c("resetSample",
                            "beliefUpdateTable", "beliefUpdateTableStat",
                            "beliefUpdateTableHide"))
    beliefTable$addColumnInfo(name = "S",             title = gettext("Species count"), type = "integer")
    beliefTable$addColumnInfo(name = "prior",         title = gettext("Prior"),              type = "number")
    beliefTable$addColumnInfo(name = "likelihood",    title = gettext("Likelihood"),         type = "number")
    beliefTable$addColumnInfo(name = "raw_posterior", title = gettext("Prior × likelihood"), type = "number")
    beliefTable$addColumnInfo(name = "posterior",     title = gettext("Posterior"),          type = "number")

    bioPostContainer[["beliefUpdateTable"]] <- beliefTable

    # Always get cumulative posterior for the "Posterior" column
    cumPostDf <- .befComputePosterior(jaspResults, options, "all")
    if (is.null(cumPostDf)) {
      beliefTable$setError(gettext("No data yet. Generate a sample to see the posterior."))
      return()
    }

    # Compute prior based on stat
    if (!is.null(stat) && stat == "batch") {
      bioLikeState_j <- jaspResults[["bioLikeState"]]
      if (!is.null(bioLikeState_j)) {
        bls         <- bioLikeState_j$object
        batch_count <- bls$batch_count
        if (batch_count <= 1) {
          prior_vec <- cumPostDf$prior
          lik_vec   <- cumPostDf$likelihood
        } else {
          like_df   <- bls$likeDF
          prev_cols <- paste0("batch_", seq_len(batch_count - 1))
          log_lik_p <- rowSums(like_df[, prev_cols, drop = FALSE], na.rm = FALSE)

          m    <- .befGetModel(options)
          mPMF <- .befGetModelPMF(m)
          p0   <- numeric(nrow(like_df))
          for (i in seq_len(nrow(like_df))) {
            idx   <- which(mPMF$s == like_df$S[i])
            p0[i] <- if (length(idx) > 0) mPMF$p[idx] else 0
          }

          lp <- log(p0) + log_lik_p
          fin <- lp[is.finite(lp)]
          if (length(fin) == 0) {
            prior_vec <- cumPostDf$prior
          } else {
            raw       <- ifelse(is.finite(lp), exp(lp - max(fin)), 0)
            prior_vec <- raw / sum(raw)
          }

          # Current batch likelihood (normalized for display)
          batch_ll <- like_df[[paste0("batch_", batch_count)]]
          fin_lik  <- batch_ll[is.finite(batch_ll)]
          if (length(fin_lik) > 0) {
            lik_vec <- ifelse(is.finite(batch_ll), exp(batch_ll - max(fin_lik)), 0)
          } else {
            lik_vec <- rep(0, nrow(like_df))
          }
        }
      } else {
        prior_vec <- cumPostDf$prior
        lik_vec   <- cumPostDf$likelihood
      }
    } else {
      prior_vec <- cumPostDf$prior
      lik_vec   <- cumPostDf$likelihood
    }

    raw_post  <- prior_vec * lik_vec
    post_vec  <- cumPostDf$posterior
    impossible <- cumPostDf$impossible
    all_ruled_out <- cumPostDf$all_ruled_out

    if (hide_imp)
      keep <- !impossible
    else
      keep <- rep(TRUE, length(impossible))

    for (i in seq_len(nrow(cumPostDf))) {
      if (!keep[i]) next
      beliefTable$addRows(list(
        S             = cumPostDf$S[i],
        prior         = prior_vec[i],
        likelihood    = lik_vec[i],
        raw_posterior = raw_post[i],
        posterior     = post_vec[i]
      ))
    }

    if (isTRUE(all_ruled_out[1]))
      beliefTable$addFootnote(gettext(
        "All hypotheses are ruled out by the data under the current model. Consider expanding the prior range."
      ))
  }

  .befDisplayBeliefUpdatePlot <- function(jaspResults, options) {
    bioPostContainer <- jaspResults[["bioPostContainer"]]
    if (is.null(bioPostContainer))
      return()

    stat     <- options[["beliefUpdatePlotStat"]]
    hide_imp <- !isFALSE(options[["beliefUpdatePlotHide"]])

    postPlot <- createJaspPlot(title = gettext("Prior and posterior plot"),
                               width = 500, height = 400)
    postPlot$dependOn(c("resetSample",
                         "beliefUpdatePlot", "beliefUpdatePlotStat",
                         "beliefUpdatePlotHide",
                         .befModelDepends))
    bioPostContainer[["beliefUpdatePlot"]] <- postPlot

    # Current (cumulative) posterior
    postDf <- .befComputePosterior(jaspResults, options, "all")
    if (is.null(postDf)) {
      postPlot$setError(gettext("No data yet. Generate a sample to see the posterior."))
      return()
    }

    # Prior to show: original model prior ("initial") or posterior from previous batches ("batch")
    if (is.null(stat) || stat == "initial") {
      prior_probs <- postDf$prior
    } else {
      bioLikeState <- jaspResults[["bioLikeState"]]$object
      batch_count  <- bioLikeState$batch_count
      if (batch_count <= 1) {
        prior_probs <- postDf$prior
      } else {
        like_df   <- bioLikeState$likeDF
        prev_cols <- paste0("batch_", seq_len(batch_count - 1))
        log_lik_p <- rowSums(like_df[, prev_cols, drop = FALSE], na.rm = FALSE)

        m    <- .befGetModel(options)
        mPMF <- .befGetModelPMF(m)
        p0   <- numeric(nrow(like_df))
        for (i in seq_len(nrow(like_df))) {
          idx   <- which(mPMF$s == like_df$S[i])
          p0[i] <- if (length(idx) > 0) mPMF$p[idx] else 0
        }

        lp       <- log(p0) + log_lik_p
        fin      <- lp[is.finite(lp)]
        if (length(fin) == 0) {
          prior_probs <- postDf$prior
        } else {
          raw         <- ifelse(is.finite(lp), exp(lp - max(fin)), 0)
          prior_probs <- raw / sum(raw)
        }
      }
    }

    if (hide_imp) {
      keep        <- !postDf$impossible
      postDf      <- postDf[keep, ]
      prior_probs <- prior_probs[keep]
    }

    plot_df <- data.frame(
      S    = rep(postDf$S, 2),
      prob = c(prior_probs, postDf$posterior),
      type = factor(rep(c("Prior", "Posterior"), each = nrow(postDf)),
                    levels = c("Prior", "Posterior")),
      stringsAsFactors = FALSE
    )
    plot_df$S <- factor(plot_df$S, levels = 1:10)

    x_limits <- if (hide_imp) as.character(sort(postDf$S)) else as.character(1:10)
    y_max    <- max(plot_df$prob, na.rm = TRUE)
    y_step   <- if (y_max <= 0.5) 0.05 else 0.1
    y_upper  <- min(ceiling(y_max / y_step) * y_step + y_step, 1.0)

    p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = S, y = prob, fill = type)) +
      ggplot2::geom_bar(stat = "identity",
                        position = ggplot2::position_dodge(width = 0.7),
                        width = 0.6) +
      ggplot2::scale_fill_manual(values = c("Prior" = "#95a5a6", "Posterior" = "#4DA3FF"),
                                 name = NULL) +
      ggplot2::scale_x_discrete(limits = x_limits, labels = x_limits) +
      ggplot2::scale_y_continuous(
        limits = c(0, y_upper),
        expand = ggplot2::expansion(mult = c(0, 0.08))
      ) +
      ggplot2::labs(x = gettext("Species Count"), y = gettext("Probability")) +
      jaspGraphs::geom_rangeframe() +
      jaspGraphs::themeJaspRaw() +
      ggplot2::theme(legend.position = "right",
                     plot.margin = ggplot2::margin(t = 5, r = 15, b = 15, l = 5))

    postPlot$plotObject <- p
  }

  .befDisplayEvidAccPlot <- function(jaspResults, options) {
    bioPostContainer <- jaspResults[["bioPostContainer"]]
    if (is.null(bioPostContainer))
      return()

    selected <- unlist(options[["evidAccSelected"]])
    if (length(selected) == 0)
      return()

    evidPlot <- createJaspPlot(title = gettext("Evidence accumulation"),
                               width = 550, height = 400)
    evidPlot$dependOn(c("resetSample", "evidAccPlot",
                         "evidAccSelected", .befModelDepends))
    bioPostContainer[["evidAccPlot"]] <- evidPlot

    bioLikeState_j <- jaspResults[["bioLikeState"]]
    if (is.null(bioLikeState_j)) {
      evidPlot$setError(gettext("No data yet. Generate a sample to see the evidence accumulation."))
      return()
    }

    bioLikeState <- bioLikeState_j$object
    batch_count  <- bioLikeState$batch_count

    if (batch_count == 0) {
      evidPlot$setError(gettext("No data yet. Generate a sample to see the evidence accumulation."))
      return()
    }

    ic <- jaspResults[["islandContainer"]]
    if (is.null(ic) || is.null(ic[["sampleState"]]))
      return()

    sample_list  <- ic[["sampleState"]]$object
    n_batches    <- min(batch_count, length(sample_list))
    batch_new    <- sapply(sample_list[seq_len(n_batches)],
                           function(df) sum(df$status == "New"))
    cum_n        <- c(0, cumsum(batch_new))

    m    <- .befGetModel(options)
    mPMF <- .befGetModelPMF(m)
    if (is.null(mPMF))
      return()

    like_df <- bioLikeState$likeDF

    prior_p <- numeric(nrow(like_df))
    for (i in seq_len(nrow(like_df))) {
      idx        <- which(mPMF$s == like_df$S[i])
      prior_p[i] <- if (length(idx) > 0) mPMF$p[idx] else 0
    }

    selected_s <- sort(as.integer(sub("h", "", selected)))
    hyp_labels <- as.character(selected_s)

    plot_rows <- list()

    for (k in 0:n_batches) {
      if (k == 0) {
        post_p <- prior_p
      } else {
        cols         <- paste0("batch_", seq_len(k))
        log_lik_k    <- rowSums(like_df[, cols, drop = FALSE], na.rm = FALSE)
        log_post_raw <- log(prior_p) + log_lik_k
        fin          <- log_post_raw[is.finite(log_post_raw)]
        if (length(fin) == 0) {
          post_p <- rep(0, nrow(like_df))
        } else {
          raw    <- ifelse(is.finite(log_post_raw), exp(log_post_raw - max(fin)), 0)
          post_p <- raw / sum(raw)
        }
      }

      for (s_val in selected_s) {
        idx <- which(like_df$S == s_val)
        if (length(idx) == 0) next
        plot_rows[[length(plot_rows) + 1]] <- data.frame(
          cum_n     = cum_n[k + 1],
          S         = as.character(s_val),
          posterior = post_p[idx],
          stringsAsFactors = FALSE
        )
      }
    }

    plot_df   <- do.call(rbind, plot_rows)
    plot_df$S <- factor(plot_df$S, levels = hyp_labels)

    y_max   <- max(plot_df$posterior, na.rm = TRUE)
    y_step  <- if (y_max <= 0.5) 0.05 else 0.1
    y_upper <- min(ceiling(y_max / y_step) * y_step + y_step, 1.0)

    p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = cum_n, y = posterior,
                                                color = S, group = S)) +
      ggplot2::geom_line(linewidth = 0.8) +
      ggplot2::geom_point(size = 2) +
      ggplot2::scale_x_continuous(
        breaks = function(x) {
          b <- scales::pretty_breaks(n = 6)(x)
          b[b == floor(b) & b >= 0]
        },
        expand = ggplot2::expansion(mult = c(0.02, 0.02))
      ) +
      ggplot2::scale_y_continuous(
        limits = c(0, y_upper),
        expand = ggplot2::expansion(mult = c(0, 0.08))
      ) +
      ggplot2::labs(
        x     = gettext("Unique Individuals Observed"),
        y     = gettext("Posterior Probability"),
        color = gettext("Species Count")
      ) +
      jaspGraphs::geom_rangeframe() +
      jaspGraphs::themeJaspRaw() +
      ggplot2::theme(legend.position = "right",
                     plot.margin = ggplot2::margin(t = 5, r = 15, b = 15, l = 5))

    evidPlot$plotObject <- p
  }

  .befDisplayCompPost <- function(jaspResults, options) {
    bioPostContainer <- jaspResults[["bioPostContainer"]]
    if (is.null(bioPostContainer))
      return()

    pairs <- options[["compPostPairs"]]
    if (length(pairs) == 0)
      return()

    stat   <- options[["compPostStat"]]
    postDf <- .befComputePosterior(jaspResults, options, "all")
    if (is.null(postDf))
      return()

    # When "batch" is selected, use previous-batch posterior as prior
    if (!is.null(stat) && stat == "batch") {
      bioLikeState_j <- jaspResults[["bioLikeState"]]
      if (!is.null(bioLikeState_j)) {
        bls         <- bioLikeState_j$object
        batch_count <- bls$batch_count
        if (batch_count <= 1) {
          prior_probs <- postDf$prior
        } else {
          like_df   <- bls$likeDF
          prev_cols <- paste0("batch_", seq_len(batch_count - 1))
          log_lik_p <- rowSums(like_df[, prev_cols, drop = FALSE], na.rm = FALSE)

          m    <- .befGetModel(options)
          mPMF <- .befGetModelPMF(m)
          p0   <- numeric(nrow(like_df))
          for (i in seq_len(nrow(like_df))) {
            idx   <- which(mPMF$s == like_df$S[i])
            p0[i] <- if (length(idx) > 0) mPMF$p[idx] else 0
          }

          lp <- log(p0) + log_lik_p
          fin <- lp[is.finite(lp)]
          if (length(fin) == 0) {
            prior_probs <- postDf$prior
          } else {
            raw         <- ifelse(is.finite(lp), exp(lp - max(fin)), 0)
            prior_probs <- raw / sum(raw)
          }
        }
      } else {
        prior_probs <- postDf$prior
      }
    } else {
      prior_probs <- postDf$prior
    }

    subscripts <- c("₁","₂","₃","₄","₅","₆","₇","₈","₉","₁₀")

    # Likelihood ratio needs per-S log likelihood
    bioLikeState <- jaspResults[["bioLikeState"]]
    if (!is.null(bioLikeState)) {
      bls         <- bioLikeState$object
      like_df     <- bls$likeDF
      log_lik_vec <- if (!is.null(stat) && stat == "batch")
        like_df[[paste0("batch_", bls$batch_count)]]
      else
        like_df$cumulative
    } else {
      log_lik_vec <- NULL
    }

    if (!isFALSE(options[["compPostTable"]])) {
      compTable <- createJaspTable(title = gettext("Posterior comparison table"))
      compTable$dependOn(c("resetSample",
                            "compPostTable", "compPostStat", "compPostPairs",
                            .befModelDepends))
      compTable$addColumnInfo(name = "hyp1",       title = gettext("Hypothesis 1"),        type = "string")
      compTable$addColumnInfo(name = "hyp2",       title = gettext("Hypothesis 2"),        type = "string")
      compTable$addColumnInfo(name = "prior_odds", title = gettext("Prior odds"),           type = "number")
      compTable$addColumnInfo(name = "bf",         title = gettext("Likelihood ratio (BF)"), type = "number")
      compTable$addColumnInfo(name = "post_odds",  title = gettext("Posterior odds"),      type = "number")

      bioPostContainer[["compPostTable"]] <- compTable

      if (is.null(postDf)) {
        compTable$setError(gettext("No data yet. Generate a sample to see the posterior comparison."))
        return()
      }

      comp_i <- 0
      for (pair in pairs) {
        if (length(pair) < 2) next
        keyA <- pair[[1]]; keyB <- pair[[2]]
        if (keyA == "" || keyB == "") next
        comp_i <- comp_i + 1

        sA <- as.integer(sub("h", "", keyA))
        sB <- as.integer(sub("h", "", keyB))

        idxA <- which(postDf$S == sA)
        idxB <- which(postDf$S == sB)
        if (length(idxA) == 0 || length(idxB) == 0) next

        prior_A    <- prior_probs[idxA]
        prior_B    <- prior_probs[idxB]
        post_A     <- postDf$posterior[idxA]
        post_B     <- postDf$posterior[idxB]
        prior_odds <- if (prior_B == 0) Inf else prior_A / prior_B
        post_odds  <- if (post_B  == 0) Inf else post_A  / post_B

        if (!is.null(log_lik_vec)) {
          llA <- log_lik_vec[idxA]
          llB <- log_lik_vec[idxB]
          bf  <- if ( is.finite(llA) &&  is.finite(llB)) exp(llA - llB)
                 else if (!is.finite(llA) && !is.finite(llB)) NaN
                 else if (!is.finite(llA)) 0
                 else Inf
        } else {
          bf <- if (prior_B == 0) NaN else post_odds / prior_odds
        }

        compTable$addRows(list(
          hyp1       = paste0("H", subscripts[sA]),
          hyp2       = paste0("H", subscripts[sB]),
          prior_odds = prior_odds,
          bf         = bf,
          post_odds  = post_odds
        ))
      }
    }
  }

  .befDisplayMargAbundanceTable <- function(jaspResults, options) {
    bioPostContainer <- jaspResults[["bioPostContainer"]]
    if (is.null(bioPostContainer))
      return()

    stat <- options[["margAbundanceStat"]]
    if (is.null(stat)) stat <- "mean"

    margTable <- createJaspTable(title = gettext("Species abundance table"))
    margTable$dependOn(c("resetSample",
                          "margAbundanceTable", "margAbundanceStat",
                          .befModelDepends))
    margTable$addColumnInfo(name = "species",  title = gettext("Species"),            type = "string")
    margTable$addColumnInfo(name = "estimate", title = gettext("Marginal abundance"), type = "number")

    bioPostContainer[["margAbundanceTable"]] <- margTable

    # Posterior weights (overall cumulative)
    postDf <- .befComputePosterior(jaspResults, options, "all")
    if (is.null(postDf)) {
      margTable$setError(gettext("No data yet. Generate a sample to see the species abundance."))
      return()
    }

    if (isTRUE(postDf$all_ruled_out[1])) {
      margTable$setError(gettext("The model is impossible given the data."))
      return()
    }

    # Beta parameters per species per S
    betaParamState <- jaspResults[["betaParamState"]]
    if (is.null(betaParamState)) {
      margTable$setError(gettext("No abundance data available."))
      return()
    }

    betaDf       <- betaParamState$object
    known_df     <- betaDf[!grepl("^unknown_", betaDf$Species), ]
    if (nrow(known_df) == 0) {
      margTable$setError(gettext("No species have been discovered yet."))
      return()
    }

    known_species <- unique(known_df$Species)
    unknown_df    <- betaDf[grepl("^unknown_", betaDf$Species), ]
    post_lookup   <- setNames(postDf$posterior, as.character(postDf$S))

    .betaStat <- function(a, b, stat) {
      switch(stat,
        "mean"   = a / (a + b),
        "median" = qbeta(0.5, a, b),
        "mode"   = if (a > 1 && b > 1) (a - 1) / (a + b - 2) else NA_real_,
        a / (a + b)
      )
    }

    for (spe in known_species) {
      spe_rows <- known_df[known_df$Species == spe, ]
      weights  <- numeric(nrow(spe_rows))
      ests     <- numeric(nrow(spe_rows))

      for (i in seq_len(nrow(spe_rows))) {
        s_key      <- as.character(spe_rows$S_Hypothesis[i])
        w          <- post_lookup[s_key]
        if (is.na(w) || is.null(w)) w <- 0
        est        <- .betaStat(spe_rows$alpha_a[i], spe_rows$beta_b[i], stat)
        weights[i] <- w
        ests[i]    <- if (!is.na(est)) est else 0
      }

      total_w  <- sum(weights)
      marginal <- if (total_w > 0) sum(weights * ests) / total_w else NA_real_
      margTable$addRows(list(species = spe, estimate = marginal))
    }

    # Unknown row: for each S, sum all unknown species' estimates, weight by posterior
    unk_marginal <- 0
    if (nrow(unknown_df) > 0) {
      s_vals <- sort(unique(unknown_df$S_Hypothesis))
      for (j in seq_along(s_vals)) {
        s_key    <- as.character(s_vals[j])
        w        <- post_lookup[s_key]
        if (is.null(w) || is.na(w)) w <- 0
        if (w == 0) next
        unk_rows <- unknown_df[unknown_df$S_Hypothesis == s_vals[j], ]
        s_sum    <- sum(sapply(seq_len(nrow(unk_rows)), function(i) {
          est <- .betaStat(unk_rows$alpha_a[i], unk_rows$beta_b[i], stat)
          if (!is.na(est)) est else 0
        }))
        unk_marginal <- unk_marginal + w * s_sum
      }
    }
    margTable$addRows(list(species = "Unknown", estimate = unk_marginal))
  }

  .befDisplayMargAbundancePlot <- function(jaspResults, options) {
    bioPostContainer <- jaspResults[["bioPostContainer"]]
    if (is.null(bioPostContainer))
      return()

    margPlot <- createJaspPlot(title = gettext("Species abundance plot"),
                               width = 550, height = 400)
    margPlot$dependOn(c("resetSample",
                         "margAbundancePlot", "margAbundanceStat",
                         .befModelDepends))
    bioPostContainer[["margAbundancePlot"]] <- margPlot

    postDf <- .befComputePosterior(jaspResults, options, "all")
    if (is.null(postDf)) {
      margPlot$setError(gettext("No data yet. Generate a sample to see the species abundance."))
      return()
    }

    if (isTRUE(postDf$all_ruled_out[1])) {
      margPlot$setError(gettext("The model is impossible given the data."))
      return()
    }

    betaParamState <- jaspResults[["betaParamState"]]
    if (is.null(betaParamState)) {
      margPlot$setError(gettext("No abundance data available."))
      return()
    }

    betaDf        <- betaParamState$object
    known_df      <- betaDf[!grepl("^unknown_", betaDf$Species), ]
    if (nrow(known_df) == 0) {
      margPlot$setError(gettext("No species have been discovered yet."))
      return()
    }

    post_lookup   <- setNames(postDf$posterior, as.character(postDf$S))
    known_species <- unique(known_df$Species)

    # Collect all alpha/beta pairs to determine dynamic x-axis range
    all_ab <- list()
    for (spe in known_species) {
      spe_rows <- known_df[known_df$Species == spe, ]
      for (i in seq_len(nrow(spe_rows))) {
        a <- spe_rows$alpha_a[i]; b <- spe_rows$beta_b[i]
        if (is.finite(a) && is.finite(b) && a > 0 && b > 0)
          all_ab[[length(all_ab) + 1]] <- c(a, b)
      }
    }
    for (s_val in postDf$S) {
      unk_rows <- betaDf[betaDf$S_Hypothesis == s_val &
                           grepl("^unknown_", betaDf$Species), ]
      for (i in seq_len(nrow(unk_rows))) {
        a <- unk_rows$alpha_a[i]; b <- unk_rows$beta_b[i]
        if (is.finite(a) && is.finite(b) && a > 0 && b > 0)
          all_ab[[length(all_ab) + 1]] <- c(a, b)
      }
    }

    if (length(all_ab) > 0) {
      q_lower <- min(sapply(all_ab, function(ab) stats::qbeta(0.001, ab[1], ab[2])))
      q_upper <- max(sapply(all_ab, function(ab) stats::qbeta(0.999, ab[1], ab[2])))
      x_pad   <- (q_upper - q_lower) * 0.15
      x_lim_l <- max(0, q_lower - x_pad)
      x_lim_r <- min(1, q_upper + x_pad)
    } else {
      x_lim_l <- 0
      x_lim_r <- 1
    }

    x_seq <- seq(x_lim_l, x_lim_r, length.out = 300)

    plot_rows <- list()
    for (spe in known_species) {
      spe_rows <- known_df[known_df$Species == spe, ]
      dens     <- numeric(length(x_seq))

      for (i in seq_len(nrow(spe_rows))) {
        s_key <- as.character(spe_rows$S_Hypothesis[i])
        w     <- post_lookup[s_key]
        if (is.null(w) || is.na(w) || w == 0) next
        a <- spe_rows$alpha_a[i]
        b <- spe_rows$beta_b[i]
        if (!is.finite(a) || !is.finite(b) || a <= 0 || b <= 0) next
        dens <- dens + w * dbeta(x_seq, a, b)
      }

      plot_rows[[length(plot_rows) + 1]] <- data.frame(
        x       = x_seq,
        density = dens,
        species = spe,
        stringsAsFactors = FALSE
      )
    }

    if (length(plot_rows) == 0) return()

    # Unknown line: iterate over all S hypotheses directly from betaDf
    unk_dens <- numeric(length(x_seq))
    for (s_val in postDf$S) {
      w <- post_lookup[as.character(s_val)]
      if (is.null(w) || is.na(w) || w == 0) next
      unk_rows <- betaDf[betaDf$S_Hypothesis == s_val &
                           grepl("^unknown_", betaDf$Species), ]
      if (nrow(unk_rows) == 0) next
      for (i in seq_len(nrow(unk_rows))) {
        a <- unk_rows$alpha_a[i]; b <- unk_rows$beta_b[i]
        if (!is.finite(a) || !is.finite(b) || a <= 0 || b <= 0) next
        unk_dens <- unk_dens + w * dbeta(x_seq, a, b)
      }
    }

    plot_rows[[length(plot_rows) + 1]] <- data.frame(
      x       = x_seq,
      density = unk_dens,
      species = "Unknown",
      stringsAsFactors = FALSE
    )

    plot_df <- do.call(rbind, plot_rows)

    y_max   <- max(plot_df$density, na.rm = TRUE)
    if (!is.finite(y_max) || y_max <= 0) y_max <- 1
    y_step  <- if (y_max <= 5) 1 else if (y_max <= 20) 5 else 10
    y_upper <- min(ceiling(y_max / y_step) * y_step + y_step, y_max * 1.5)

    all_species <- unique(plot_df$species)
    known_in_df <- all_species[all_species != "Unknown"]
    color_vals  <- c(
      setNames(scales::hue_pal()(length(known_in_df)), known_in_df),
      c("Unknown" = "#95a5a6")
    )

    p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = x, y = density, color = species)) +
      ggplot2::geom_line(linewidth = 0.8) +
      ggplot2::scale_color_manual(values = color_vals) +
      ggplot2::scale_x_continuous(
        expand = ggplot2::expansion(mult = c(0, 0.05))
      ) +
      ggplot2::scale_y_continuous(
        limits = c(0, y_upper),
        expand = ggplot2::expansion(mult = c(0, 0.08))
      ) +
      ggplot2::labs(
        x     = gettext("Abundance"),
        y     = gettext("Density"),
        color = gettext("Species")
      ) +
      jaspGraphs::geom_rangeframe() +
      jaspGraphs::themeJaspRaw() +
      ggplot2::theme(legend.position = "right",
                     plot.margin = ggplot2::margin(t = 5, r = 15, b = 15, l = 5))

    margPlot$plotObject <- p
  }

  .befDisplayPredTable <- function(jaspResults, options) {
    bioPostContainer <- jaspResults[["bioPostContainer"]]
    if (is.null(bioPostContainer))
      return()

    predType <- options[["predTableTyp"]]

    predTable <- createJaspTable(title = gettext("Posterior prediction"))
    predTable$dependOn(c("resetSample", "predTable", "predTableTyp",
                          .befModelDepends))

    if (is.null(predType) || predType == "speType") {
      predTable$addColumnInfo(name = "species",  title = gettext("Species"),            type = "string")
      predTable$addColumnInfo(name = "estimate", title = gettext("Predictive probability"), type = "number")
    } else {
      predTable$addColumnInfo(name = "category", title = gettext("Category"),              type = "string")
      predTable$addColumnInfo(name = "estimate", title = gettext("Predictive probability"), type = "number")
    }

    bioPostContainer[["predTable"]] <- predTable

    postDf <- .befComputePosterior(jaspResults, options, "all")
    if (is.null(postDf)) {
      predTable$setError(gettext("No data yet. Generate a sample to see predictions."))
      return()
    }

    if (isTRUE(postDf$all_ruled_out[1])) {
      predTable$setError(gettext("The model is impossible given the data."))
      return()
    }

    betaParamState <- jaspResults[["betaParamState"]]
    if (is.null(betaParamState)) {
      predTable$setError(gettext("No abundance data available."))
      return()
    }

    betaDf       <- betaParamState$object
    known_df     <- betaDf[!grepl("^unknown_", betaDf$Species), ]
    unknown_df   <- betaDf[grepl("^unknown_", betaDf$Species), ]
    post_lookup  <- setNames(postDf$posterior, as.character(postDf$S))

    .betaStat <- function(a, b) {
      a / (a + b)
    }

    if (is.null(predType) || predType == "speType") {
      # --- By individual species ---
      known_species <- unique(known_df$Species)

      for (spe in known_species) {
        spe_rows <- known_df[known_df$Species == spe, ]
        weights  <- numeric(nrow(spe_rows))
        ests     <- numeric(nrow(spe_rows))

        for (i in seq_len(nrow(spe_rows))) {
          s_key      <- as.character(spe_rows$S_Hypothesis[i])
          w          <- post_lookup[s_key]
          if (is.na(w) || is.null(w)) w <- 0
          est        <- .betaStat(spe_rows$alpha_a[i], spe_rows$beta_b[i])
          weights[i] <- w
          ests[i]    <- if (!is.na(est)) est else 0
        }

        total_w  <- sum(weights)
        marginal <- if (total_w > 0) sum(weights * ests) / total_w else NA_real_
        predTable$addRows(list(species = spe, estimate = marginal))
      }

      # Unknown row
      unk_marginal <- 0
      if (nrow(unknown_df) > 0) {
        s_vals <- sort(unique(unknown_df$S_Hypothesis))
        for (j in seq_along(s_vals)) {
          s_key <- as.character(s_vals[j])
          w <- post_lookup[s_key]
          if (is.null(w) || is.na(w)) w <- 0
          if (w == 0) next
          unk_rows <- unknown_df[unknown_df$S_Hypothesis == s_vals[j], ]
          s_sum <- sum(sapply(seq_len(nrow(unk_rows)), function(i) {
            est <- .betaStat(unk_rows$alpha_a[i], unk_rows$beta_b[i])
            if (!is.na(est)) est else 0
          }))
          unk_marginal <- unk_marginal + w * s_sum
        }
      }
      predTable$addRows(list(species = gettext("Unknown"), estimate = unk_marginal))

    } else {
      # --- Seen vs. unseen ---
      seen_total   <- 0
      unseen_total <- 0

      for (s_val in postDf$S) {
        s_key <- as.character(s_val)
        w <- post_lookup[s_key]
        if (is.null(w) || is.na(w) || w == 0) next

        known_for_s <- known_df[known_df$S_Hypothesis == s_val, ]
        if (nrow(known_for_s) > 0) {
          seen_sum <- sum(sapply(seq_len(nrow(known_for_s)), function(i) {
            est <- .betaStat(known_for_s$alpha_a[i], known_for_s$beta_b[i])
            if (!is.na(est)) est else 0
          }))
          seen_total <- seen_total + w * seen_sum
        }

        unk_for_s <- unknown_df[unknown_df$S_Hypothesis == s_val, ]
        if (nrow(unk_for_s) > 0) {
          unk_sum <- sum(sapply(seq_len(nrow(unk_for_s)), function(i) {
            est <- .betaStat(unk_for_s$alpha_a[i], unk_for_s$beta_b[i])
            if (!is.na(est)) est else 0
          }))
          unseen_total <- unseen_total + w * unk_sum
        }
      }

      predTable$addRows(list(category = gettext("Seen species"),  estimate = seen_total))
      predTable$addRows(list(category = gettext("Unseen species"), estimate = unseen_total))
    }
  }

  .befDisplayPredPlot <- function(jaspResults, options) {
    bioPostContainer <- jaspResults[["bioPostContainer"]]
    if (is.null(bioPostContainer))
      return()

    predType <- options[["predPlotTyp"]]

    predPlot <- createJaspPlot(title = gettext("Posterior prediction"),
                               width = 550, height = 400)
    predPlot$dependOn(c("resetSample", "predPlot", "predPlotTyp",
                          .befModelDepends))

    bioPostContainer[["predPlot"]] <- predPlot

    postDf <- .befComputePosterior(jaspResults, options, "all")
    if (is.null(postDf)) {
      predPlot$setError(gettext("No data yet. Generate a sample to see predictions."))
      return()
    }

    if (isTRUE(postDf$all_ruled_out[1])) {
      predPlot$setError(gettext("The model is impossible given the data."))
      return()
    }

    betaParamState <- jaspResults[["betaParamState"]]
    if (is.null(betaParamState)) {
      predPlot$setError(gettext("No abundance data available."))
      return()
    }

    betaDf       <- betaParamState$object
    known_df     <- betaDf[!grepl("^unknown_", betaDf$Species), ]
    unknown_df   <- betaDf[grepl("^unknown_", betaDf$Species), ]
    post_lookup  <- setNames(postDf$posterior, as.character(postDf$S))

    .betaStat <- function(a, b) {
      a / (a + b)
    }

    if (is.null(predType) || predType == "speType") {
      # --- By individual species: bar chart of predictive means ---
      known_species <- unique(known_df$Species)

      plot_rows <- list()
      for (spe in known_species) {
        spe_rows <- known_df[known_df$Species == spe, ]
        weights  <- numeric(nrow(spe_rows))
        ests     <- numeric(nrow(spe_rows))

        for (i in seq_len(nrow(spe_rows))) {
          s_key      <- as.character(spe_rows$S_Hypothesis[i])
          w          <- post_lookup[s_key]
          if (is.na(w) || is.null(w)) w <- 0
          est        <- .betaStat(spe_rows$alpha_a[i], spe_rows$beta_b[i])
          weights[i] <- w
          ests[i]    <- if (!is.na(est)) est else 0
        }

        total_w  <- sum(weights)
        marginal <- if (total_w > 0) sum(weights * ests) / total_w else NA_real_
        plot_rows[[length(plot_rows) + 1]] <- data.frame(
          species     = spe,
          probability = marginal,
          stringsAsFactors = FALSE
        )
      }

      # Unknown
      unk_marginal <- 0
      if (nrow(unknown_df) > 0) {
        s_vals <- sort(unique(unknown_df$S_Hypothesis))
        for (j in seq_along(s_vals)) {
          s_key <- as.character(s_vals[j])
          w <- post_lookup[s_key]
          if (is.null(w) || is.na(w)) w <- 0
          if (w == 0) next
          unk_rows <- unknown_df[unknown_df$S_Hypothesis == s_vals[j], ]
          s_sum <- sum(sapply(seq_len(nrow(unk_rows)), function(i) {
            est <- .betaStat(unk_rows$alpha_a[i], unk_rows$beta_b[i])
            if (!is.na(est)) est else 0
          }))
          unk_marginal <- unk_marginal + w * s_sum
        }
      }
      plot_rows[[length(plot_rows) + 1]] <- data.frame(
        species     = gettext("Unknown"),
        probability = unk_marginal,
        stringsAsFactors = FALSE
      )

      plot_df <- do.call(rbind, plot_rows)

      if (all(is.na(plot_df$probability))) {
        predPlot$setError(gettext("No species have been discovered yet."))
        return()
      }

      all_species <- unique(plot_df$species)
      known_in_df <- all_species[all_species != gettext("Unknown")]

      plot_df$species <- factor(plot_df$species,
                                 levels = c(known_in_df, gettext("Unknown")))
      plot_df$is_unknown <- plot_df$species == gettext("Unknown")

      predPlot$height <- max(400, 60 + 26 * nlevels(plot_df$species))

      yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, max(plot_df$probability, na.rm = TRUE)))
      yMax    <- min(1, max(yBreaks))

      p <- ggplot2::ggplot(plot_df,
                           ggplot2::aes(x = species, y = probability, fill = is_unknown)) +
        ggplot2::geom_bar(stat = "identity", width = 0.6) +
        ggplot2::scale_fill_manual(values = c("FALSE" = "#4DA3FF", "TRUE" = "#95a5a6")) +
        ggplot2::scale_y_continuous(
          limits = c(0, yMax),
          breaks = yBreaks[yBreaks <= yMax],
          expand = ggplot2::expansion(mult = c(0, 0.08))
        ) +
        ggplot2::coord_flip() +
        ggplot2::labs(
          x = gettext("Species"),
          y = gettext("Predictive probability")
        ) +
        jaspGraphs::geom_rangeframe() +
        jaspGraphs::themeJaspRaw() +
        ggplot2::theme(legend.position = "none",
                       plot.margin = ggplot2::margin(t = 5, r = 15, b = 15, l = 5))

    } else {
      # --- Seen vs. unseen: bar chart ---
      seen_total   <- 0
      unseen_total <- 0

      for (s_val in postDf$S) {
        s_key <- as.character(s_val)
        w <- post_lookup[s_key]
        if (is.null(w) || is.na(w) || w == 0) next

        known_for_s <- known_df[known_df$S_Hypothesis == s_val, ]
        if (nrow(known_for_s) > 0) {
          seen_sum <- sum(sapply(seq_len(nrow(known_for_s)), function(i) {
            est <- .betaStat(known_for_s$alpha_a[i], known_for_s$beta_b[i])
            if (!is.na(est)) est else 0
          }))
          seen_total <- seen_total + w * seen_sum
        }

        unk_for_s <- unknown_df[unknown_df$S_Hypothesis == s_val, ]
        if (nrow(unk_for_s) > 0) {
          unk_sum <- sum(sapply(seq_len(nrow(unk_for_s)), function(i) {
            est <- .betaStat(unk_for_s$alpha_a[i], unk_for_s$beta_b[i])
            if (!is.na(est)) est else 0
          }))
          unseen_total <- unseen_total + w * unk_sum
        }
      }

      plot_df <- data.frame(
        category = c(gettext("Seen species"), gettext("Unseen species")),
        value    = c(seen_total, unseen_total),
        stringsAsFactors = FALSE
      )
      plot_df$category <- factor(plot_df$category,
                                  levels = c(gettext("Seen species"), gettext("Unseen species")))

      yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, max(plot_df$value, na.rm = TRUE)))
      yMax    <- min(1, max(yBreaks))

      p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = category, y = value, fill = category)) +
        ggplot2::geom_bar(stat = "identity", width = 0.5) +
        ggplot2::scale_fill_manual(values = setNames(
          c("#4DA3FF", "#95a5a6"),
          levels(plot_df$category)
        )) +
        ggplot2::scale_y_continuous(
          limits = c(0, yMax),
          breaks = yBreaks[yBreaks <= yMax],
          expand = ggplot2::expansion(mult = c(0, 0.08))
        ) +
        ggplot2::labs(x = "", y = gettext("Predictive probability")) +
        jaspGraphs::geom_rangeframe() +
        jaspGraphs::themeJaspRaw() +
        ggplot2::theme(legend.position = "none",
                       plot.margin = ggplot2::margin(t = 5, r = 15, b = 15, l = 5))
    }

    predPlot$plotObject <- p
  }


#################Helper Functions#############################
  #################General######################################
    .befFillDataBarPlot <- function(sample_vec) {
      df <- as.data.frame(table(Species = sample_vec))
      colnames(df) <- c("Species", "Count")
      df$Species <- reorder(df$Species, df$Count)

      yMax <- max(df$Count, na.rm = TRUE)
      if (!is.finite(yMax) || yMax <= 0) yMax <- 1
      yBreaks <- scales::breaks_pretty()(c(0, yMax))
      yBreaks <- yBreaks[yBreaks == floor(yBreaks)]
      yUpper  <- max(yBreaks)

      p <- ggplot2::ggplot(df, ggplot2::aes(x = Species, y = Count)) +
        ggplot2::geom_bar(stat = "identity", fill = "#4DA3FF", color = "#4DA3FF", width = 0.7) +
        ggplot2::scale_y_continuous(
          limits = c(0, yUpper),
          breaks = yBreaks,
          expand = ggplot2::expansion(mult = c(0, 0.08))
        ) +
        ggplot2::coord_flip() +
        ggplot2::labs(x = gettext("Species"), y = gettext("Count")) +
        jaspGraphs::geom_rangeframe() +
        jaspGraphs::themeJaspRaw() +
        ggplot2::theme(plot.margin = ggplot2::margin(t = 5, r = 15, b = 15, l = 5))

      return(p)
    }


    .befDescText <- function(model) {
      if (model[["type"]] == "point") {
        desc <- gettextf("You are absolutely certain there are exactly %s species, all in, no more, no less",
                        model[["pointPriorN"]])
      } else if (model[["type"]] == "uniform") {
        desc <- gettextf("You know that the number of different species lies between %1$s and %2$s, but anything in between is equally likely, no additional information.",
                        model[["minimum"]], model[["maximum"]])
      } else if (model[["type"]] == "poisson") {
        desc <- gettextf("You know the number of different species lies between %1$s and %2$s, and you expect an average of %3$s species.",
                        model[["minimum"]], model[["maximum"]], model[["poissonlambda"]])
      } else if (model[["type"]] == "negbino") {
        desc <- gettextf("You know the number of different species lies between %1$s and %2$s. The average number of species you expect is %3$s (μ = mean). The parameter ϕ = %4$s controls the spread: smaller ϕ means more uncertainty around that average.",
                        model[["minimum"]], model[["maximum"]], model[["nbMu"]], model[["nbPhi"]])
      }
      return(desc)
    }

    .befGetModelPMF <- function(model) {
      if (model[["type"]] == "point") {
        s_range <- model[["pointPriorN"]]
      } else {
        s_range <- model[["minimum"]]:model[["maximum"]]
      }

      raw_probs <- switch(model[["type"]],
        "point"   = 1,
        "uniform" = rep(1, length(s_range)),
        "poisson" = dpois(s_range, lambda = model[["poissonlambda"]]),
        "negbino" = dnbinom(s_range, size = model[["nbPhi"]], mu = model[["nbMu"]])
      )

      if (sum(raw_probs) == 0)
        return(NULL)

      list(s = s_range, p = raw_probs / sum(raw_probs))
    }


    .befCalculateMean <- function(modelPMF) {
      sum(modelPMF[["s"]] * modelPMF[["p"]])
    }

    .befCalculateMode <- function(modelPMF) {
      s_vec <- modelPMF[["s"]]
      s_vec[which.max(modelPMF[["p"]])][1]
    }

    .befCalculateMedian <- function(modelPMF) {
      cum_p <- cumsum(modelPMF[["p"]])
      modelPMF[["s"]][which(cum_p >= 0.5)[1]]
    }

    .befCalculateSD <- function(modelPMF) {
      s  <- modelPMF[["s"]]
      p  <- modelPMF[["p"]]
      mu <- sum(s * p)
      sqrt(sum((s - mu)^2 * p))
    }

    .befCalculateProbability <- function(modelPMF, mincut, maxcut) {
      s_vec <- modelPMF[["s"]]
      p_vec <- modelPMF[["p"]]
      if (mincut > max(s_vec) || maxcut < min(s_vec))
        return(0)
      sum(p_vec[s_vec >= mincut & s_vec <= maxcut])
    }


  ##################Abundance######################
    .befCalculateBeta <- function(abundanceState) {
      alphaMatrix  <- abundanceState[["alphaMatrix"]]
      speciesNames <- abundanceState[["speciesNames"]]
      s_values     <- abundanceState[["s_values"]]

      colTotals <- colSums(alphaMatrix, na.rm = TRUE)
      b_matrix  <- abs(sweep(alphaMatrix, 2, colTotals, "-"))

      resultsList <- vector("list", length(s_values))

      for (j in seq_along(s_values)) {
        a_vals    <- alphaMatrix[, j]
        b_vals    <- b_matrix[, j]
        valid_idx <- !is.na(a_vals)

        resultsList[[j]] <- data.frame(
          Species      = as.character(speciesNames[valid_idx]),
          S_Hypothesis = as.numeric(s_values[j]),
          alpha_a      = a_vals[valid_idx],
          beta_b       = b_vals[valid_idx],
          stringsAsFactors = FALSE
        )
      }

      do.call(rbind, resultsList)
    }


    .befGetAbundanceBetaParams <- function(s_val, spe, betaParamsDf, oldbetaParamsDf) {
      old_row <- oldbetaParamsDf[oldbetaParamsDf$Species == spe &
                                  oldbetaParamsDf$S_Hypothesis == s_val, ]
      new_row <- betaParamsDf[betaParamsDf$Species == spe &
                                betaParamsDf$S_Hypothesis == s_val, ]

      if (nrow(new_row) == 0)
        return(NULL)

      if (nrow(old_row) == 0) {
        oldAlpha <- 1
        oldBetaSelected <- oldbetaParamsDf[oldbetaParamsDf$S_Hypothesis == s_val, ]
        oldBeta  <- oldBetaSelected[nrow(oldBetaSelected), "beta_b"]
      } else {
        oldAlpha <- old_row$alpha_a[1]
        oldBeta  <- old_row$beta_b[1]
      }

      newAlpha <- new_row$alpha_a[1]
      newBeta  <- new_row$beta_b[1]

      list(oldAlpha = oldAlpha, oldBeta = oldBeta, newAlpha = newAlpha, newBeta = newBeta)
    }

    .befAbundanceDensityRange <- function(params) {
      x_lower <- max(0, min(stats::qbeta(0.001, params$oldAlpha, params$oldBeta),
                             stats::qbeta(0.001, params$newAlpha, params$newBeta)))
      x_upper <- min(1, max(stats::qbeta(0.999, params$oldAlpha, params$oldBeta),
                             stats::qbeta(0.999, params$newAlpha, params$newBeta)))
      x_pad   <- (x_upper - x_lower) * 0.15
      x_lim_l <- max(0, x_lower - x_pad)
      x_lim_r <- min(1, x_upper + x_pad)

      seq(x_lim_l, x_lim_r, length.out = 200)
    }

    # Computes the density curve data once per (S, species) pair so it can be
    # reused both for the global y-axis max (Enforce same scale) and the plot itself.
    .befComputeAbundanceDensity <- function(s_val, spe, betaParamsDf, oldbetaParamsDf) {
      params <- .befGetAbundanceBetaParams(s_val, spe, betaParamsDf, oldbetaParamsDf)
      if (is.null(params))
        return(NULL)

      x_seq   <- .befAbundanceDensityRange(params)
      df_plot <- data.frame(
        x    = rep(x_seq, 2),
        y    = c(dbeta(x_seq, params$oldAlpha, params$oldBeta),
                dbeta(x_seq, params$newAlpha, params$newBeta)),
        type = rep(c("Before", "After"), each = length(x_seq))
      )

      list(params = params, df_plot = df_plot, maxDensity = max(df_plot$y, na.rm = TRUE))
    }

    .befPlotAbundance <- function(spe, densityData, options, globalYMax = NULL) {
      params   <- densityData$params
      df_plot  <- densityData$df_plot
      oldAlpha <- params$oldAlpha
      oldBeta  <- params$oldBeta
      newAlpha <- params$newAlpha
      newBeta  <- params$newBeta

      .betaEst <- function(a, b, stat) {
        switch(stat,
          "mean"   = a / (a + b),
          "median" = stats::qbeta(0.5, a, b),
          "mode"   = if (a > 1 && b > 1) (a - 1) / (a + b - 2) else NA_real_,
          a / (a + b)
        )
      }

      stat <- options[["abundanceStat"]]
      if (is.null(stat)) stat <- "mean"

      stat_label <- switch(stat,
        "mean"   = "Mean",
        "median" = "Median",
        "mode"   = "Mode",
        "Mean"
      )

      old_est <- .betaEst(oldAlpha, oldBeta, stat)
      new_est <- .betaEst(newAlpha, newBeta, stat)

      caption_text <- if (!isFALSE(options[["despAbundancePlotExp"]])) {
        paste0(
          "Beta(", round(oldAlpha, 2), ", ", round(oldBeta, 2), ") → ",
          "Beta(", round(newAlpha, 2), ", ", round(newBeta, 2), ")\n",
          stat_label, ": ", round(old_est, 3),
          " → ", round(new_est, 3)
        )
      } else {
        NULL
      }

      show_est <- !isFALSE(options[["despAbundancePlotEst"]])

      maxDensity <- if (!is.null(globalYMax)) globalYMax else densityData$maxDensity
      yBreaks    <- jaspGraphs::getPrettyAxisBreaks(c(0, maxDensity))
      yMax       <- max(yBreaks)

      p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = x, y = y, linetype = type)) +
        ggplot2::geom_line(linewidth = 0.9) +
        ggplot2::scale_linetype_manual(values = c("Before" = "dashed", "After" = "solid")) +
        ggplot2::scale_x_continuous(limits = if (!is.null(globalYMax)) c(0, 1) else NULL,
                                    expand = ggplot2::expansion(mult = c(0, 0.05))) +
        ggplot2::scale_y_continuous(limits = c(0, yMax),
                                    breaks = yBreaks,
                                    expand = ggplot2::expansion(mult = c(0, 0.08))) +
        ggplot2::labs(
          title    = spe,
          caption  = caption_text,
          x        = "Abundance",
          y        = "Density",
          linetype = ""
        ) +
        ggplot2::theme(
          legend.position = "bottom",
          plot.title      = ggplot2::element_text(hjust = 0.5),
          plot.caption    = ggplot2::element_text(hjust = 0.5, size = 10, color = "grey40"),
          plot.margin     = ggplot2::margin(t = 5, r = 15, b = 15, l = 5)
        ) +
        jaspGraphs::geom_rangeframe() +
        jaspGraphs::themeJaspRaw()

      if (show_est) {
        vline_df <- data.frame(
          xintercept = c(old_est, new_est),
          type       = c("Before", "After")
        )
        p <- p +
          ggplot2::geom_vline(data = vline_df,
                              ggplot2::aes(xintercept = xintercept, linetype = type),
                              color = "#E74C3C", linewidth = 0.7, show.legend = FALSE)
      }

      p
    }

  ##################Bio Likelihood######################
  .befCalculateLikelihoodNew <- function(abundanceState, bioLikeState, unseen_species) {
    alpha_mat    <- abundanceState$alphaMatrix
    k            <- length(unseen_species)
    new_currentK <- abundanceState$currentK + k

    for (i in seq_along(unseen_species))
      rownames(alpha_mat)[abundanceState$currentK + i] <- unseen_species[i]

    keep_cols <- which(abundanceState$s_values >= new_currentK)

    if (length(keep_cols) == 0)
      return(list(likelihood = rep(-Inf, 10), abundanceState = abundanceState))

    alpha_mat <- alpha_mat[, keep_cols, drop = FALSE]

    bioLikeVecNew <- rep(-Inf, 10)

    if (k == 0) {
      for (col in seq_along(colnames(alpha_mat))) {
        num_species <- as.numeric(sub("S = ", "", colnames(alpha_mat)[col]))
        bioLikeVecNew[num_species] <- 0
      }
    } else {
      for (col in seq_along(colnames(alpha_mat))) {
        num_species <- as.numeric(sub("S = ", "", colnames(alpha_mat)[col]))
        sum_alpha   <- colSums(alpha_mat, na.rm = TRUE)[col]

        loglik     <- 0
        cumulative <- 0
        for (fac in seq_len(k)) {
          loglik <- loglik + log((k + col - fac) / (sum_alpha + cumulative))
          cumulative <- cumulative + 1
        }
        bioLikeVecNew[num_species] <- loglik
      }
    }

    for (i in seq_along(unseen_species))
      alpha_mat[unseen_species[i], ] <- 2

    abundanceState$alphaMatrix  <- alpha_mat
    abundanceState$speciesNames <- rownames(alpha_mat)
    abundanceState$currentK     <- new_currentK
    abundanceState$s_values     <- abundanceState$s_values[keep_cols]

    return(list(likelihood = bioLikeVecNew, abundanceState = abundanceState))
  }

  .befCalculateLikelihoodOld <- function(abundanceState, new_animals_counts) {
    alpha_mat     <- abundanceState$alphaMatrix
    bioLikeVecOld <- rep(0, 10)

    for (species_name in names(new_animals_counts)) {
      count <- as.integer(new_animals_counts[species_name])
      if (count == 0) next

      for (obs in seq_len(count)) {
        for (col in seq_along(colnames(alpha_mat))) {
          num_species <- as.numeric(sub("S = ", "", colnames(alpha_mat)[col]))
          if (bioLikeVecOld[num_species] == -Inf) next

          alpha_i   <- alpha_mat[species_name, col]
          sum_alpha <- sum(alpha_mat[, col], na.rm = TRUE)

          if (is.na(alpha_i) || sum_alpha <= 0) {
            bioLikeVecOld[num_species] <- -Inf
          } else {
            bioLikeVecOld[num_species] <- bioLikeVecOld[num_species] + log(alpha_i / sum_alpha)
          }
        }
        alpha_mat[species_name, ] <- alpha_mat[species_name, ] + 1
      }
    }

    abundanceState$alphaMatrix <- alpha_mat
    return(list(likelihood = bioLikeVecOld, abundanceState = abundanceState))
  }



  .befComputePosterior <- function(jaspResults, options, stat = "all") {
    bioLikeState_j <- jaspResults[["bioLikeState"]]
    if (is.null(bioLikeState_j))
      return(NULL)

    bioLikeState <- bioLikeState_j$object
    if (bioLikeState$batch_count == 0)
      return(NULL)

    like_df <- bioLikeState$likeDF

    log_lik_vec <- if (stat == "batch")
      like_df[[paste0("batch_", bioLikeState$batch_count)]]
    else
      like_df$cumulative

    m    <- .befGetModel(options)
    mPMF <- .befGetModelPMF(m)
    if (is.null(mPMF))
      return(NULL)

    prior_p <- numeric(nrow(like_df))
    for (i in seq_len(nrow(like_df))) {
      idx         <- which(mPMF$s == like_df$S[i])
      prior_p[i]  <- if (length(idx) > 0) mPMF$p[idx] else 0
    }

    # Likelihood: normalize to max = 1 for display
    finite_lik    <- log_lik_vec[is.finite(log_lik_vec)]
    if (length(finite_lik) > 0) {
      lik_display <- ifelse(is.finite(log_lik_vec),
                            exp(log_lik_vec - max(finite_lik)),
                            0)
    } else {
      lik_display <- rep(0, nrow(like_df))
    }

    raw_posterior <- prior_p * lik_display

    impossible    <- !is.finite(log_lik_vec) | prior_p == 0

    all_ruled_out <- sum(raw_posterior) == 0

    if (all_ruled_out) {
      post_p <- rep(0, nrow(like_df))
    } else {
      post_p <- raw_posterior / sum(raw_posterior)
    }

    data.frame(
      S             = like_df$S,
      prior         = prior_p,
      likelihood    = lik_display,
      raw_posterior = raw_posterior,
      posterior     = post_p,
      impossible    = impossible,
      all_ruled_out = all_ruled_out,
      stringsAsFactors = FALSE
    )
  }


