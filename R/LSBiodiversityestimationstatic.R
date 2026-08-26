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


LSBiodiversityestimationstatic <- function(jaspResults, dataset, options, state = NULL) {
  .besIntro(jaspResults, options)

##################Trigger##########################
  curr_samp  <- options[["sampleTrigger"]]
  curr_reset <- options[["resetTrigger"]]
  if (is.null(jaspResults[["triggerState"]])) {
    prev_samp  <- curr_samp
    prev_reset <- curr_reset
  } else {

    prev <- jaspResults[["triggerState"]]$object
    prev_samp  <- prev[1]
    prev_reset <- prev[2]
  }
  should_sample <- (curr_samp > prev_samp) && (curr_reset == prev_reset)
  
##################Prior############################
  if (is.null(jaspResults[["priorState"]]))
    .besInitializePriorBelief(jaspResults, options)

  if (is.null(jaspResults[["priorBelief"]]))
    .besCreatePriorState(jaspResults, options)

  priorBeliefContainer <- jaspResults[["priorBelief"]]

  if (!isFALSE(options[["priorExpText"]]))
    .besDisplayPriorExp(priorBeliefContainer, options)

  .besDisplayPriorTable(jaspResults, priorBeliefContainer, options)

  if (!isFALSE(options[["priorDispPlot"]]))
    .besDisplayPriorPlot(jaspResults, priorBeliefContainer, options)

##################Data#############################
  
  if (is.null(jaspResults[["dataContainer"]]))
    .besCreateDataContainer(jaspResults, options)

  if (!isFALSE(options[["dataExpText"]]))
    .besDisplayDataExp(jaspResults, options)

  if (is.null(jaspResults[["islandState"]]))
    .besCreateSampleIsland(jaspResults, options)

  if (is.null(jaspResults[["sampleData"]])){
    .besCreateDataState(jaspResults, options)
    sampleDataState <- jaspResults[["sampleData"]]
  }else
    sampleDataState <- jaspResults[["sampleData"]]

  if (should_sample)
    .besUpdateData(jaspResults, options)

  newTrigger <- createJaspState(c(curr_samp, curr_reset))
  newTrigger$dependOn(c("resetTrigger"))
  jaspResults[["triggerState"]] <- newTrigger

  if (!is.null(sampleDataState))
    .besDisplayDataTable(jaspResults, options)

##################Likelihood#####################
  if (is.null(jaspResults[["likelihoodContainer"]]))
    .besCreateLikelihoodContainer(jaspResults, options)

  if (!isFALSE(options[["likelihoodExpText"]]))
    .besDisplayLikelihoodExp(jaspResults, options)

  if (is.null(jaspResults[["likelihoodState"]]))
    .besCreateLikelihoodState(jaspResults, options)

  if (should_sample)
    .besUpdateLikelihood(jaspResults, options)

  if (!is.null(jaspResults[["likelihoodContainer"]])) {
    if (!isFALSE(options[["likelihoodTable"]]))
      .besDisplayLikelihoodTable(jaspResults, options)

    if (!isFALSE(options[["likelihoodPlot"]]))
      .besDisplayLikelihoodPlot(jaspResults, options)

    if (!isFALSE(options[["showBF"]]))
      .besDisplayLikelihoodPz(jaspResults, options)
  }

##################Posterior######################
  if (is.null(jaspResults[["posteriorContainer"]]))
    .besCreatePosteriorContainer(jaspResults, options)

  if (!isFALSE(options[["posteriorExpText"]]))
    .besDisplayPosteriorExp(jaspResults, options)


  if (!isFALSE(options[["beliefUpdateTable"]]))
    .besDisplayPosteriorTable(jaspResults, options)

  if (!isFALSE(options[["beliefUpdatePlot"]]))
    .besDisplayPosteriorPlot(jaspResults, options)

  if (!isFALSE(options[["evidPlot"]]))
    .besDisplayPosteriorAccumu(jaspResults, options)

  if (!isFALSE(options[["compPostTable"]]))
    .besDisplayPosteriorCompTable(jaspResults, options)

  if (!isFALSE(options[["compPostPlot"]]))
    .besDisplayPosteriorCompPlot(jaspResults, options)

################Posterior Prediction###############
  if (is.null(jaspResults[["predContainer"]]))
    .besCreatePredContainer(jaspResults, options)

  if (!isFALSE(options[["predExpText"]]))
    .besDisplayPredExp(jaspResults, options)

  if (!isFALSE(options[["postPredTable"]]))
    .besDisplayPredTable(jaspResults, options)

  if (!isFALSE(options[["postPredPlot"]]))
    .besDisplayPredPlot(jaspResults, options)

  if (!isFALSE(options[["predChangePlot"]]))
    .besDisplayPredChangePlot(jaspResults, options)

}


################Intro#############################
.besIntro <- function(jaspResults, options) {
  if (isFALSE(options[["introductoryText"]]))
    return()
  if (!is.null(jaspResults[["introductoryText"]]))
    return()

  text <- gettextf("<p> In this module, you will play the role of a <b>biologist</b> who has just arrived on a <b>fictional island</b>, and your main task is to find out how many different species live there.</p>
                  <p>There are a few rules to follow along the way. First, you will make a <b>guess</b> about how many different species there might be. Then, you will <b>observe animals one at a time</b> and <b>update your belief</b> accordingly, in a completely logical way.</p>
                  <p>Excited? Let's dive into the sections below and get started! If you ever feel lost, just tick <i>Explanatory text</i> in each section for extra guidance.</p>
                  <p>Have fun, and your adventure begins here!</p>")

  jaspResults[["introductoryText"]] <- createJaspHtml(
    title        = gettext("Welcome!"),
    text         = text,
    dependencies = "introductoryText",
    position     = 1
  )
}

#################Prior#############################
.besCreatePriorState <- function(jaspResults, options) {

  priorBeliefContainer <- createJaspContainer(title = gettext("Prior Belief"))
  priorBeliefContainer$dependOn(c("priorType", "priorS1", "priorS2", "priorS3"))



  jaspResults[["priorBelief"]] <- priorBeliefContainer
}

.besDisplayPriorExp <- function(priorBeliefContainer, options) {

  if (!is.null(priorBeliefContainer[["priorIntroText"]]))
    return()

  text <- gettext("<p>Before looking at any data, we all have some kind of <b>gut feeling</b> about how the world works. Maybe it comes from experience, maybe it is just a hunch — and that is totally fine! In Bayesian statistics, we call this a <b>prior belief</b>. There is no right or wrong answer here. Different starting beliefs will send you on different \"adventures\", and it is really interesting to see where each one leads!</p>
                  <p>In this example, we want to figure out how many species live on a fictional island. We know there are three possible species — <b>cat</b>, <b>horse</b>, and <b>pigeon</b> — and there could be 1, 2, or all 3 of them living there. What do <i>you</i> think is most likely before seeing any evidence? Everyone might have a different answer, and that is exactly the point!</p>
                  <p>Here is what you can do in this section:</p>
                  <ul>
                    <li><b>Equal probability for each species count</b> — no information and want to play \"safe\"? Just treat all possibilities as equally likely to start.</li>
                    <li><b>Custom probability for each species count</b> — have a hunch? Click the <i>Custom probabilities for each species count</i> button and type in your own numbers. Don't worry about making them sum to 1 — the program handles that for you!</li>
                  </ul>
                  <p>You can also switch the display to <b>Species combination</b> to see the hypotheses split into actual species combinations (like \"cat + horse\") instead of just a count. Tick <b>Display prior plot</b> to see your beliefs as a bar chart!</p>")

  introHtml <- createJaspHtml(
    title    = gettext("Explanation of Prior Belief"),
    text     = text,
    position = 1
  )
  introHtml$dependOn("priorExpText")

  priorBeliefContainer[["priorIntroText"]] <- introHtml
}

.besInitializePriorBelief <- function(jaspResults, options) {
  if (options[["priorType"]] == "uniform")
    prior_vec <- c(1, 1, 1)
  else
    prior_vec <- c(options$priorS1, options$priorS2, options$priorS3)


  if (sum(prior_vec) == 0) {
    prior_vec <- NULL
    prior_df  <- NULL
  }
  else {
    prior_vec <- prior_vec / sum(prior_vec)
    species_num <- c(rep(1, 3), rep(2, 3), 3)
    species <- c("cat", "horse", "pigeon",
                 "cat, horse", "cat, pigeon", "horse, pigeon",
                 "cat, horse, pigeon")
    prior_prob <- c(rep(prior_vec[1]/3, 3), rep(prior_vec[2]/3, 3), prior_vec[3])

    prior_df <- data.frame(s = species_num,
                           species = species,
                           prior_prob = prior_prob)
  }

  prior_list <- list(
    priorS   = prior_vec,
    priorSpe = prior_df
  )

  priorState <- createJaspState(prior_list)
  priorState$dependOn(c("priorType", "priorS1", "priorS2", "priorS3"))
  jaspResults[["priorProbs"]] <- priorState
}

.besDisplayPriorTable <- function(jaspResults, priorBeliefContainer, options) {
  if (!is.null(priorBeliefContainer[["priorTable"]]))
    return()

  priorTable <- createJaspTable(title = gettext("Prior probabilities"))
  priorTable$dependOn(c("priorType", "priorS1", "priorS2", "priorS3",
                        "priorDisplay"))

  prior_list <- jaspResults[["priorProbs"]]$object
  prior_vec <- prior_list$priorS
  prior_df  <- prior_list$priorSpe

  if (options[["priorDisplay"]] == "numS") {
    priorTable$addColumnInfo(name = "hypName",       title = gettext("Hypothesis"),    type = "string")
    priorTable$addColumnInfo(name = "s",             title = gettext("Species count"),    type = "integer")
    priorTable$addColumnInfo(name = "priorPS",       title = gettext("Prior probability"), type = "number")

    if (is.null(prior_vec)) {
      priorTable$setError(gettext(
        "All prior probabilities are zero. Please assign a positive value to at least one hypothesis."
      ))

      priorBeliefContainer[["priorTable"]] <- priorTable
      return()
    }


    for (i in seq_along(prior_vec)) {
      priorTable$addRows(list(
        hypName = paste0("H<sub>", i, "</sub>"),
        s       = i,
        priorPS = prior_vec[i]
      ))
    }
  }

  if (options[["priorDisplay"]] == "comb") {
    priorTable$addColumnInfo(name = "hypName",                 title = gettext("Hypothesis"),           type = "string")
    priorTable$addColumnInfo(name = "s",                       title = gettext("Species count"),    type = "integer")
    priorTable$addColumnInfo(name = "namesS",        title = gettext("Names of species"),     type = "string")
    priorTable$addColumnInfo(name = "priorPS",                 title = gettext("Prior probability"), type = "number")

    if (is.null(prior_df)) {
      priorTable$setError(gettext(
        "All prior probabilities are zero. Please assign a positive value to at least one hypothesis."
      ))

      priorBeliefContainer[["priorTable"]] <- priorTable
      return()
    }

    hyp_labels <- .makeTableLabels(prior_df)

    for (i in seq_len(nrow(prior_df))) {
      s = prior_df$s[i]

      priorTable$addRows(list(
        hypName = hyp_labels[i],
        s       = prior_df$s[i],
        namesS  = prior_df$species[i],
        priorPS = prior_df$prior_prob[i]
      ))
    }
  }

  priorBeliefContainer[["priorTable"]] <- priorTable

}

.besDisplayPriorPlot <- function(jaspResults, priorBeliefContainer, options) {
  if (!is.null(priorBeliefContainer[["priorPlot"]]))
    return()

  priorPlot <- createJaspPlot(
    title  = gettext("Prior probabilities"),
    width  = 480,
    height = 320)
  priorPlot$dependOn(c("priorType", "priorS1", "priorS2", "priorS3",
                       "priorDisplay", "priorDispPlot"))

  prior_list <- jaspResults[["priorProbs"]]$object
  prior_vec <- prior_list$priorS
  prior_df  <- prior_list$priorSpe

  if (is.null(prior_vec)) {
    priorPlot$setError(gettext(
      "All prior probabilities are zero. Please assign a positive value to at least one hypothesis."
    ))
    priorBeliefContainer[["priorPlot"]] <- priorPlot
    return()
  }

  if (options[["priorDisplay"]] == "numS") {
    hyp_sub <- c(1, 2, 3)
    hyp_labels <- sprintf("H[%d]", hyp_sub)
    plot_df <- data.frame(
      hypName = factor(hyp_labels, levels = hyp_labels),
      probability = prior_vec
    )

    yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, max(plot_df$probability)))
    yMax    <- min(1, max(yBreaks))

    p <- ggplot2::ggplot(plot_df,
                         ggplot2::aes(x = hypName, y = probability)) +
      ggplot2::geom_col(fill = "#4DA3FF", width = 0.6) +
      ggplot2::scale_x_discrete(labels = function(x) parse(text = x)) +
      ggplot2::scale_y_continuous(
        limits = c(0, yMax),
        expand = ggplot2::expansion(mult = c(0, 0.08)),
        breaks = yBreaks[yBreaks <= yMax]
      )  +
      ggplot2::labs(
        x = gettext("Hypothesis (Species Count)"),
        y = gettext("Prior probability")
      ) +
      jaspGraphs::geom_rangeframe() +
      jaspGraphs::themeJaspRaw() +
      ggplot2::theme(plot.margin = ggplot2::margin(t = 5, r = 15, b = 15, l = 5))
  }

  if (options[["priorDisplay"]] == "comb") {
    hyp_labels <- .makePlotLabels(prior_df)
    plot_df <- prior_df
    plot_df$hyp_name <- factor(hyp_labels, levels = hyp_labels)

    yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, max(plot_df$prior_prob)))
    yMax    <- min(1, max(yBreaks))

    p <- ggplot2::ggplot(plot_df,
                         ggplot2::aes(x = hyp_name, y = prior_prob, fill = factor(s))) +
      ggplot2::geom_col( width = 0.6) +
      ggplot2::scale_x_discrete(labels = function(x) parse(text = x)) +
      ggplot2::scale_fill_manual(
        name   = gettext("Species count"),
        values = c("3" = "#9CC9F2", "2" = "#4DA3FF", "1" = "#1E5BB8")
      ) +
      ggplot2::scale_y_continuous(
        limits = c(0, yMax),
        expand = ggplot2::expansion(mult = c(0, 0.08)),
        breaks = yBreaks[yBreaks <= yMax]
      ) +

      ggplot2::labs(
        x = gettext("Hypothesis (Species Combination)"),
        y = gettext("Prior probability")
      ) +
      jaspGraphs::geom_rangeframe() +
      jaspGraphs::themeJaspRaw() +
      ggplot2::theme(plot.margin = ggplot2::margin(t = 5, r = 15, b = 15, l = 5))
  }

  priorPlot$plotObject <- p
  priorBeliefContainer[["priorPlot"]] <- priorPlot
}

#################Data########################
.besCreateDataContainer <- function(jaspResults, options) {
  dataContainer <- createJaspContainer(title = gettext("Data"))
  jaspResults[["dataContainer"]] <- dataContainer
}

.besDisplayDataExp <- function(jaspResults, options) {
  dataContainer <- jaspResults[["dataContainer"]]
  if (is.null(dataContainer))
    return()

  if (!is.null(dataContainer[["dataIntroText"]]))
    return()

  text <- gettext("<p>Now it is time to collect some <b>evidence</b>! The data collection process is central in scientific fields, and scientices usually try to gain insights into the underlying data generating process through the data. 
                  <p> In our example, the hypothetical data generating processes are our hypotheses - recall: how many species are on the island? What are they? Realistically, scientists go out into the field and observe animals. Here, we simulate that process by <b>sampling</b> animals from the island.</p>
                  <p>A few things to keep in mind:</p>
                  <ul>
                    <li>Use the <b><i>Sample an animal</i></b> button to make observations. Each time you click it, you will see a new animal that you have observed on the island popping up in the table on the right.</li>
                    <li>Each observation is drawn <b>randomly</b> from the island's true population — just like a real field study.</li>
                    <li>The <b>sampled data</b> table shows a <b>summary</b> of everything you have observed so far, and also what you saw in the last sample.</li>
                    <li>Try starting with just a few samples, and think intuitively: does your initial beliefs of the number of species change as you see the data? Is there specific beliefs that are ruled out by the data, or that become more likely?</li>
                    <li>Want to start fresh? Click the <b><i>Reset</i></b> button to get a new island and start again!</li>
                  </ul>")

  introHtml <- createJaspHtml(
    title    = gettext("Explanation of Data"),
    text     = text,
    position = 1
  )
  introHtml$dependOn("dataExpText")

  dataContainer[["dataIntroText"]] <- introHtml

}

.besCreateSampleIsland <- function(jaspResults, options) {

  spe_base <- c("cat", "horse", "pigeon")
  N <- length(spe_base)

  all_worlds <- unlist(
    lapply(1:N, function(k) {
      combn(spe_base, k, simplify = FALSE)
    }),
    recursive = FALSE
  )

  num_species <- sample(1:3, size = 1)
  spe_island <- sample(spe_base, num_species)
  truth_index <- which(sapply(all_worlds, function(w) setequal(w, spe_island)))
  ground_truth <- c(num_species, truth_index)

  print(ground_truth)

  island_list <- list(
    speciesN     = num_species,
    speciesNames = spe_island,
    groundTruth  = ground_truth
  )

  sampleIslandState <- createJaspState(island_list)
  sampleIslandState$dependOn(c("resetTrigger"))
  jaspResults[["islandState"]] <- sampleIslandState
}

.besCreateDataState <- function(jaspResults, options) {
  all_sample <- c()
  sample_size <- 0

  species <- c("cat", "horse", "pigeon")
  old_num <- rep(0, 3)
  new_num <- rep(0, 3)

  sample_df <- data.frame(
    species = species,
    old_num = old_num,
    new_num = new_num)

  sample_list <- list(
    allSample = all_sample,
    species = species,
    sampleDf = sample_df,
    sampleSize = sample_size
  )

  sampleDataState <- createJaspState(sample_list)
  sampleDataState$dependOn(c("resetTrigger"))
  jaspResults[["sampleData"]] <- sampleDataState
}

.besCreateTriggerState <- function(jaspResults, options) {
  sample_trig <- options[["sampleTrigger"]]
  reset_trig  <- options[["resetTrigger"]]
  trig <- c(sample_trig, reset_trig)
  triggerState <- createJaspState(trig)
  triggerState$dependOn(c("resetTrigger"))
  jaspResults[["triggerState"]] <- triggerState
}

.besUpdateData <- function(jaspResults, options) {

  sampleDataState <- jaspResults[["sampleData"]]
  if (is.null(sampleDataState))
    return()

  sampleDataList <- sampleDataState$object
  all_sample <- sampleDataList[["allSample"]]
  sample_df <- sampleDataList[["sampleDf"]]
  sample_size <- sampleDataList[["sampleSize"]]
  species <- sampleDataList[["species"]]

  island_list <- jaspResults[["islandState"]]$object
  spe_all <- island_list$speciesNames

  sample_result <- sample(spe_all, 1)

  all_sample <- c(all_sample, sample_result)

  new_num <- rep(0, length(species))
  new_num[which(sample_result == species)] = 1
  sample_df$old_num <- sample_df$old_num + sample_df$new_num
  sample_df$new_num <- new_num

  sample_size <- sample_size + 1

  sampleDataList <- list(
    allSample  = all_sample,
    sampleDf   = sample_df,
    sampleSize = sample_size,
    species    = species
  )

  sampleDataState <- createJaspState(sampleDataList)
  sampleDataState$dependOn(c("resetTrigger"))
  jaspResults[["sampleData"]] <- sampleDataState
}

.besDisplayDataTable <- function(jaspResults, options) {
  sampleDataState <- jaspResults[["sampleData"]]
  if (is.null(sampleDataState))
    return()

  dataContainer <- jaspResults[["dataContainer"]]
  if (is.null(dataContainer))
    return()

  if (is.null(dataContainer[["dataTable"]])){
    dataTable <- createJaspTable(title = gettext("Sampled data"))
    dataTable$dependOn(c("resetTrigger", "sampleTrigger"))

    sampleDataList <- sampleDataState$object
    all_sample <- sampleDataList[["allSample"]]
    sample_df <- sampleDataList[["sampleDf"]]
    sample_size <- sampleDataList[["sampleSize"]]
    species <- sampleDataList[["species"]]

    dataTable$addColumnInfo(name = "species",       title = gettext("Species"),           type = "string")
    dataTable$addColumnInfo(name = "old",           title = gettext("Previous samples"),    type = "integer")
    dataTable$addColumnInfo(name = "new",        title = gettext("Current sample"),     type = "integer")
    dataTable$addColumnInfo(name = "total",      title = gettext("Total"), type = "integer")

    for (i in seq_len(nrow(sample_df))) {
      s = sample_df$species[i]

      dataTable$addRows(list(
        species = s,
        old     = sample_df$old_num[i],
        new     = sample_df$new_num[i],
        total   = sample_df$old_num[i] + sample_df$new_num[i]
      ))
    }

    dataContainer[["dataTable"]] <- dataTable
  }
}


#################Likelihood###################
.besCreateLikelihoodContainer <- function(jaspResults, options) {
  likelihoodContainer <- createJaspContainer(title = gettext("Likelihood"))

  jaspResults[["likelihoodContainer"]] <- likelihoodContainer
}

.besDisplayLikelihoodExp <- function(jaspResults, options) {
  likelihoodContainer <- jaspResults[["likelihoodContainer"]]
  if (is.null(likelihoodContainer))
    return()

  if (!is.null(likelihoodContainer[["likelihoodIntroText"]]))
    return()

  text <- gettext("<p>Now that we have a bunch of hypotheses and some data — how do we use the data to <b>evaluate</b> those hypotheses?</p>
                  <p>In Bayesian statistics, we use something called the <b>likelihood</b>. The likelihood tells us: <i>if we assume a particular hypothesis is true, how probable is it to observe the data we actually got?</i> For example, if our hypothesis is that the island has cats and horses (50% each), and we spot a cat, then the likelihood of that hypothesis for this observation is 0.5. A higher likelihood means the hypothesis does a better job at explaining the data!</p>
                  <p>In this section, you can see the likelihood of each hypothesis in a table and/or a plot. A couple of handy options:
                  <ul>
                    <li>Switch between <b>Number of species</b> and <b>Species combinations</b> to view hypotheses at different levels of detail.</li>
                    <li>For the table, change the <b>Displayed value</b> to <b>Log likelihood</b> — the numbers can get really tiny as observations pile up, so the log-scale makes them easier to read.</li>
                    <li>For the plot, choose to show the likelihood of the <b>most recent sample</b> or the <b>overall likelihood</b> of all samples so far.</li>
                  </ul></p>
                  <p>Want to compare two hypotheses head-to-head? Tick the <b>Bayes factor</b> checkbox and drag hypotheses into the comparison panel. The Bayes factor is just the <i>ratio</i> of two likelihoods — it tells you how many times more likely the data are under one hypothesis compared to the other. We often visualise this with a <b>pizza plot</b>: the hypothesis that explains the data better takes up a bigger slice of the pizza. Imagine dipping your finger in — which topping would you expect to hit? That is exactly the intuition!</p>
                  <p><b>Something to think about:</b> Which hypotheses tend to have a larger likelihood if there are only a few species (let's say 1 or 2) in your sample? Can you figure out why?")

  introHtml <- createJaspHtml(
    title    = gettext("Explanation of Likelihood"),
    text     = text,
    position = 1
  )
  introHtml$dependOn("likelihoodExpText")

  likelihoodContainer[["likelihoodIntroText"]] <- introHtml

}

.besCreateLikelihoodState <- function(jaspResults, options) {

  priorState <- jaspResults[["priorProbs"]]
  if (is.null(priorState))
    return()

  prior_list <- priorState$object
  likelihood_df <- prior_list$priorSpe[, c("s", "species")]

  num_sample <- 0

  likelihood_list <- list(
    nSample = num_sample,
    likelihoodDf = likelihood_df
  )

  likelihoodState <- createJaspState(likelihood_list)
  likelihoodState$dependOn(c("resetTrigger"))
  jaspResults[["likelihoodState"]] <- likelihoodState
}

.besUpdateLikelihood <- function(jaspResults, options) {
  likelihoodState <- jaspResults[["likelihoodState"]]
  if (is.null(likelihoodState))
    return()

  likelihoodList <- likelihoodState$object
  num_sample <- likelihoodList$nSample
  likelihood_df <- likelihoodList$likelihoodDf

  sampleDataState <- jaspResults[["sampleData"]]
  if (is.null(sampleDataState))
    return()

  sampleDataList <- sampleDataState$object
  all_sample <- sampleDataList$allSample
  new_sample <- all_sample[length(all_sample)]

  sample_poss <- c(rep(1, 3), rep(1/2, 3), 1/3)
  spe_seq <- likelihood_df$species
  existence <- as.integer(grepl(new_sample, spe_seq))
  new_likelihood <- existence * sample_poss

  num_sample <- num_sample + 1
  new_col_name <- paste0("L_", num_sample)
  likelihood_df[[new_col_name]] <- new_likelihood

  likelihoodList <- list(
    nSample = num_sample,
    likelihoodDf = likelihood_df
  )

  likelihoodState <- createJaspState(likelihoodList)
  likelihoodState$dependOn(c("resetTrigger"))
  jaspResults[["likelihoodState"]] <- likelihoodState
}

.besDisplayLikelihoodTable <- function(jaspResults, options) {
  likelihoodContainer <- jaspResults[["likelihoodContainer"]]
  if (!is.null(likelihoodContainer[["likelihoodTable"]]))
    return()

  likelihoodState <- jaspResults[["likelihoodState"]]
  if (is.null(likelihoodState))
    return()

  if (!is.null(likelihoodContainer[["likelihoodTable"]]))
    return()

  likelihoodList <- likelihoodState$object

  likelihoodTable <- createJaspTable(title = gettext("Likelihood table"))
  likelihoodTable$dependOn(c("resetTrigger", "sampleTrigger","likelihoodTable",
                             "likelihoodTableDisplay","likelihoodTableValue",
                             "likelihoodTableHide"))

  num_sample <- likelihoodList$nSample

  if (num_sample == 0) {
    likelihoodTable$setError(gettext(
      "No data. Sample some animals to begin"
    ))
    likelihoodContainer[["likelihoodTable"]] <- likelihoodTable
    return()
  }

  res <- .computeSpeciesLikelihood(jaspResults)
  if (options[["likelihoodTableDisplay"]] == "numS") {
    table_df <- data.frame(
      s            = as.integer(names(res$logOverall_S)),
      lik_all      = exp(res$logOverall_S),
      lik_batch    = exp(res$logSample_S),
      log_lik_all = as.numeric(res$logOverall_S),
      stringsAsFactors = FALSE
    )
    table_df$hyp_names <- paste0("H<sub>", table_df$s, "</sub>")
    table_df$species   <- NA_character_
  } else {
    table_df <- data.frame(
      s           = res$s,
      species     = res$species,
      lik_all     = exp(res$logOverall_H),
      lik_batch   = res$lik_batch_H,
      log_lik_all = res$logOverall_H,
      stringsAsFactors = FALSE
    )
    table_df$hyp_names <- .makeTableLabels(table_df)
  }

  if (!isFALSE(options[["likelihoodTableHide"]])) {
    table_df <- table_df[is.finite(table_df$log_lik_all), ]
  }

  if (options[["likelihoodTableValue"]] == "logL") {
    table_df$display_batch <- log(table_df$lik_batch)
    table_df$display_all   <- table_df$log_lik_all
  }else {
    table_df$display_batch <- table_df$lik_batch
    table_df$display_all   <- table_df$lik_all
  }


  likelihoodTable$addColumnInfo(name = "hypName", title = gettext("Hypothesis"),       type = "string")
  likelihoodTable$addColumnInfo(name = "s",       title = gettext("Species count"), type = "integer")
  if (options[["likelihoodTableDisplay"]] == "comb")
    likelihoodTable$addColumnInfo(name = "spe",   title = gettext("Species"),           type = "string")

  likelihoodTable$addColumnInfo(name = "batchL",  title = gettext("Sample likelihood"), type = "number")
  likelihoodTable$addColumnInfo(name = "allL",    title = gettext("Overall likelihood"), type = "number")

  for (i in seq_len(nrow(table_df))) {
    row <- list(
      hypName = table_df$hyp_names[i],
      s       = table_df$s[i],
      batchL  = table_df$display_batch[i],
      allL    = table_df$display_all[i]
    )
    if (options[["likelihoodTableDisplay"]] == "comb")
      row$spe <- table_df$species[i]
    likelihoodTable$addRows(row)
  }
  likelihoodContainer[["likelihoodTable"]] <- likelihoodTable
}

.besDisplayLikelihoodPlot <- function(jaspResults, options) {
  likelihoodContainer <- jaspResults[["likelihoodContainer"]]
  if (is.null(likelihoodContainer))
    return()

  likelihoodState <- jaspResults[["likelihoodState"]]
  if (is.null(likelihoodState))
    return()

  likelihoodList <- likelihoodState$object

  if (!is.null(likelihoodContainer[["likelihoodPlot"]]))
    return()

  likelihoodPlot <- createJaspPlot(
    title = gettext("Likelihood"),
    width = 480,
    height = 320
  )
  likelihoodPlot$dependOn(c("resetTrigger", "sampleTrigger", "likelihoodPlot",
                            "likelihoodPlotDisplay",
                            "likelihoodPlotData",
                            "likelihoodPlotHide"))

  num_sample <- likelihoodList$nSample

  if (num_sample == 0) {
    likelihoodPlot$setError(gettext(
      "No data. Sample some animals to begin"
    ))
    likelihoodContainer[["likelihoodPlot"]] <- likelihoodPlot
    return()
  }

  res <- .computeSpeciesLikelihood(jaspResults)

  if (options[["likelihoodPlotDisplay"]] == "numS") {
    plot_df <- data.frame(
      s          = as.integer(names(res$logOverall_S)),
      lik_all    = exp(res$logOverall_S),
      lik_batch  = exp(res$logSample_S),
      log_lik_all = as.numeric(res$logOverall_S),
      stringsAsFactors = FALSE
    )
    plot_df$hyp <- sprintf("H[%d]", plot_df$s)
    plot_df$species   <- NA_character_
  } else {
    plot_df <- data.frame(
      s           = res$s,
      species     = res$species,
      lik_all     = exp(res$logOverall_H),
      lik_batch   = res$lik_batch_H,
      log_lik_all = res$logOverall_H,
      stringsAsFactors = FALSE
    )
    plot_df$hyp <- .makePlotLabels(plot_df)
  }

  if (!isFALSE(options[["likelihoodPlotHide"]])) {
    plot_df <- plot_df[is.finite(plot_df$log_lik_all), ]
  }

  plot_df$yval <- if (options[["likelihoodPlotData"]] == "batch")
    plot_df$lik_batch else plot_df$lik_all

  plot_df$hyp <- factor(plot_df$hyp, levels = plot_df$hyp)

  ymax <- max(plot_df$yval, na.rm = TRUE)
  if (!is.finite(ymax) || ymax <= 0) ymax <- 1

  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, ymax))
  yMax    <- min(1, max(yBreaks))

  if (options[["likelihoodPlotDisplay"]] == "numS") {
    p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = hyp, y = yval)) +
      ggplot2::geom_col(width = 0.6, fill = "#4DA3FF") +
      ggplot2::scale_x_discrete(labels = function(l) parse(text = l)) +
      ggplot2::scale_y_continuous(limits = c(0, yMax),
                                  expand = ggplot2::expansion(mult = c(0, 0.08)),
                                  breaks = yBreaks[yBreaks <= yMax]) +
      ggplot2::labs(x = gettext("Hypothesis (Species Count)"),
                    y = gettext("Likelihood")) +
      jaspGraphs::geom_rangeframe() +
      jaspGraphs::themeJaspRaw() +
      ggplot2::theme(plot.margin = ggplot2::margin(t = 5, r = 15, b = 15, l = 5))
  } else {
    p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = hyp, y = yval, fill = factor(s))) +
      ggplot2::geom_col(width = 0.6) +
      ggplot2::scale_x_discrete(labels = function(l) parse(text = l)) +
      ggplot2::scale_fill_manual(
        name   = gettext("Species count"),
        values = c("1" = "#1E5BB8", "2" = "#4DA3FF", "3" = "#9CC9F2")
      ) +
      ggplot2::scale_y_continuous(limits = c(0, yMax),
                                  expand = ggplot2::expansion(mult = c(0, 0.08)),
                                  breaks = yBreaks[yBreaks <= yMax]) +
      ggplot2::labs(x = gettext("Hypothesis (Species Combination)"),
                    y = gettext("Likelihood")) +
      jaspGraphs::geom_rangeframe() +
      jaspGraphs::themeJaspRaw() +
      ggplot2::theme(plot.margin = ggplot2::margin(t = 5, r = 15, b = 15, l = 5))
  }

  likelihoodPlot$plotObject <- p

  likelihoodContainer[["likelihoodPlot"]] <- likelihoodPlot

}

.besDisplayLikelihoodPz <- function(jaspResults, options) {
  likelihoodContainer <- jaspResults[["likelihoodContainer"]]
  if (is.null(likelihoodContainer))
    return()

  likelihoodState <- jaspResults[["likelihoodState"]]
  if (is.null(likelihoodState))
    return()

  likelihoodList <- likelihoodState$object

  if (is.null(likelihoodContainer[["likelihoodPizzaContainer"]])) {
    likelihoodPizzaContainer <- createJaspContainer(title = gettext("Bayes Factor"))
    likelihoodPizzaContainer$dependOn(c("resetTrigger", "sampleTrigger", "bfPairsSpe",
                                        "bfPairsComb","showBF", "bFDisplay"))
    likelihoodContainer[["likelihoodPizzaContainer"]] <- likelihoodPizzaContainer
  }

  likelihoodPizzaContainer <- likelihoodContainer[["likelihoodPizzaContainer"]]

  num_sample <- likelihoodState$object$nSample
  if (num_sample == 0)
    return()

  res <- .computeSpeciesLikelihood(jaspResults)

  if (options[["bFDisplay"]] == "numS") {
    pairs  <- options[["bfPairsSpe"]]
    keys   <- c("h1", "h2", "h3")
    lookup <- stats::setNames(res$logOverall_S, keys)
    key_to_pizza_label <- stats::setNames(
      c("H₁", "H₂", "H₃"),
      keys
    )
  } else {
    pairs  <- options[["bfPairsComb"]]
    keys   <- c("h1a", "h1b", "h1c", "h2a", "h2b", "h2c", "h3")
    lookup <- stats::setNames(res$logOverall_H, keys)
    key_to_pizza_label <- stats::setNames(
      c("H₁α", "H₁β", "H₁γ",
        "H₂α", "H₂β", "H₂γ",
        "H₃"),
      keys
    )
  }

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

    if (!is.null(likelihoodPizzaContainer[[plotName]])) next

    llA <- lookup[keyA]; llB <- lookup[keyB]

    if (is.na(llA) || is.na(llB)) next

    log_bf <- llA - llB
    bf_ab  <- exp(log_bf)
    bf_ba  <- exp(-log_bf)

    if (!is.finite(llA) && !is.finite(llB)) {
      prop_a <- 0.5; prop_b <- 0.5
    } else {
      prop_a <- 1 / (1 + exp(-(llA - llB)))
      prop_b <- 1 - prop_a
    }

    dispA <- key_to_pizza_label[[keyA]]
    dispB <- key_to_pizza_label[[keyB]]

    title_txt <- gettextf("Comparison %d: %s vs. %s", comp_i, dispA, dispB)

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

    bfPlot <- createJaspPlot(title = title_txt, width = 338, height = 300)
    bfPlot$position <- comp_i

    labelA <- paste0("data | ", dispA)
    labelB <- paste0("data | ", dispB)

    p <- jaspGraphs::drawBFpizza(
      c(prop_a, prop_b),
      labels = c(labelB, labelA)
    ) +
      ggplot2::labs(caption = note) +
      ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0.5, size = 11), # nolint
                     plot.margin = ggplot2::margin(t = 5, r = 15, b = 15, l = 5))

    bfPlot$plotObject <- cowplot::plot_grid(p)
    likelihoodPizzaContainer[[plotName]] <- bfPlot
  }
}


#################Posterior####################
.besCreatePosteriorContainer <- function(jaspResults, options) {
  posteriorContainer <- createJaspContainer(title = gettext("Posterior"))

  jaspResults[["posteriorContainer"]] <- posteriorContainer
}

.besDisplayPosteriorExp <- function(jaspResults, options) {
  posteriorContainer <- jaspResults[["posteriorContainer"]]
  if (is.null(posteriorContainer))
    return()

  if (!is.null(posteriorContainer[["posteriorIntroText"]]))
    return()

  text <- gettext("<p>Great news! We have already evaluated how well different hypothetical species counts align with the data by calculating the likelihood, and you also have some initial beliefs about the species counts from before you set foot on this island. Now, your supervisor wants you to tell them what you believe now. So what should you do? Don't worry, because that is exactly what we are going to do in this <b>Posterior Belief</b> section!</p>
                  <p>In Bayesian statistics, a central rule is that \"what we believe now is the product of what we believed before and what the data tell us\", or in other words, \"posterior equals prior times likelihood\". Think about your own life: have you ever had some expectation about something, until new information came in and you changed your mind? That is exactly what we are talking about!</p>
                  <p>There are three features in this section: the belief update table and plot, the evidence accumulation plot, and compare hypotheses. Don't worry, we will explain them one by one.</p>
                  <p>The first thing you will see is the belief update table and plot. They will appear right after you tick the respective checkbox — magic! To avoid getting lost, try to extract the following information: What did you believe initially? What did the data tell you? And what do you believe now? Yes, just like you may have noticed, hypotheses that predict the data well gain in probability, while those that predict the data poorly lose in probability. If you push yourself a little further, you will find that the posterior is simply the normalised product of the prior and the likelihood. A few options you can change in the table and plot:</p>
                  <ul>
                    <li>Switch between <b>Number of species</b> and <b>Species combinations</b> to view hypotheses at different levels of detail.</li>
                    <li>Switch between <b>Last sample</b> and <b>Overall/Initial</b> to select what to focus on: the current sample only, or all samples combined.</li>
                    <li>Tick <b>Hide impossible hypotheses</b> to simplify the output, so that you are not bothered by hypotheses that are impossible given the data.</li>
                  </ul>
                  <p>Next up is the <b>Evidence Accumulation</b> plot. While the belief update table and plot give you a snapshot after each sample, this plot lets you watch your belief evolve over time, sample by sample, all in one picture. Watch closely: does your belief settle down and become more confident in one hypothesis as more data comes in? That is Bayesian updating in motion!</p>
                  <p>Finally, remember the <b>pizza plot</b> from the Likelihood section? It is back — but now comparing full <b>posterior beliefs</b> instead of just likelihoods. Tick <b>Compare Hypotheses</b> to see the table and/or plot, drag two hypotheses into the comparison panel, and see how much more (or less) probable one hypothesis is than the other, now that both your prior belief and the data have been taken into account.</p>
                  <p><b>Something to think about:</b> as you collect more samples, do the hypotheses you once thought were plausible get ruled out? How quickly does that happen?</p></p>")

  introHtml <- createJaspHtml(
    title    = gettext("Explanation of Posterior Belief"),
    text     = text,
    position = 1
  )
  introHtml$dependOn("posteriorExpText")

  posteriorContainer[["posteriorIntroText"]] <- introHtml
}

.besDisplayPosteriorTable <- function(jaspResults, options) {
  posteriorContainer <- jaspResults[["posteriorContainer"]]
  if (is.null(posteriorContainer))
    return()

  likelihoodState <- jaspResults[["likelihoodState"]]
  if (is.null(likelihoodState))
    return()

  priorProbs <- jaspResults[["priorProbs"]]
  if (is.null(priorProbs))
    return()

  if (!is.null(posteriorContainer[["posteriorTable"]]))
    return()

  posteriorTable <- createJaspTable(title = gettext("Prior and posterior table"))
  posteriorTable$dependOn(c("priorType", "priorS1", "priorS2", "priorS3",
                            "resetTrigger", "sampleTrigger",
                            "beliefUpdateTable", "bfUpdateTableDisp",
                            "bfUpdateTableStat", "bfUpdateTableHide"))

  num_sample <- likelihoodState$object$nSample
  if (num_sample == 0) {
    posteriorTable$setError(gettext(
      "No data. Sample some animals to begin"
    ))
    posteriorContainer[["posteriorTable"]] <- posteriorTable
    return()
  }

  posteriorRes <- .computePosterior(jaspResults, options)

  posteriorTable$addColumnInfo(name = "hypName", title = gettext("Hypothesis"), type = "string")
  posteriorTable$addColumnInfo(name = "s", title = gettext("Species count"), type = "integer")



  if (options[["bfUpdateTableDisp"]] == "comb") {
    posteriorTable$addColumnInfo(name = "namesS", title = gettext("Names of species"), type = "string")
  }

  table_df <- .generatePosteriorDf(posteriorRes, options, "table")

  if (!isFALSE(options[["bfUpdateTableHide"]])) {
    table_df <- table_df[table_df$posterior != 0, ]
  }

  posteriorTable$addColumnInfo(name = "prior", title = gettext("Prior"), type = "number")
  posteriorTable$addColumnInfo(name = "likelihood", title = gettext("Likelihood"), type = "number")
  posteriorTable$addColumnInfo(name = "raw_post", title = gettext("Raw posterior"), type = "number")
  posteriorTable$addColumnInfo(name = "posterior", title = gettext("Posterior"), type = "number")

  for (i in seq_len(nrow(table_df))) {
    row_list <- list()
    row_list$hypName = table_df$hyp[i]
    row_list$s = table_df$s[i]
    row_list$prior = table_df$prior[i]
    row_list$likelihood = table_df$likelihood[i]
    row_list$raw_post = table_df$raw_posterior[i]
    row_list$posterior = table_df$posterior[i]

    if (options[["bfUpdateTableDisp"]] == "comb")
      row_list$namesS <- table_df$spe[i]

    posteriorTable$addRows(row_list)
  }

  posteriorContainer[["posteriorTable"]] <- posteriorTable
}

.besDisplayPosteriorPlot <- function(jaspResults, options) {
  posteriorContainer <- jaspResults[["posteriorContainer"]]
  if (is.null(posteriorContainer))
    return()

  likelihoodState <- jaspResults[["likelihoodState"]]
  if (is.null(likelihoodState))
    return()



  priorProbs <- jaspResults[["priorProbs"]]
  if (is.null(priorProbs))
    return()

  if (!is.null(posteriorContainer[["posteriorPlot"]]))
    return()

  posteriorPlot <- createJaspPlot(title = gettext("Prior and posterior plot"),
                                  width = 480,
                                  height = 320)

  posteriorPlot$dependOn(c("priorType", "priorS1", "priorS2", "priorS3",
                            "resetTrigger", "sampleTrigger",
                            "beliefUpdatePlot", "bfUpdatePlotDisp",
                            "bfUpdatePlotStat", "bfUpdatePlotHide"))

  num_sample <- likelihoodState$object$nSample
  if (num_sample == 0) {
    posteriorPlot$setError(gettext(
      "No data. Sample some animals to begin"
    ))
    posteriorContainer[["posteriorPlot"]] <- posteriorPlot
    return()
  }



  posteriorRes <- .computePosterior(jaspResults, options)
  table_df <- .generatePosteriorDf(posteriorRes, options, "plot")

  if (!isFALSE(options[["bfUpdatePlotHide"]])) {
    table_df <- table_df[table_df$posterior != 0, ]
  }

  plot_df <- data.frame(
    hyp  = rep(table_df$hyp, times = 2),
    type = factor(rep(c("Prior", "Posterior"), each = nrow(table_df)),
                  levels = c("Prior", "Posterior")),
    prob = c(table_df$prior, table_df$posterior)
  )
  plot_df$hyp <- factor(plot_df$hyp, levels = table_df$hyp)

  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, max(plot_df$prob)))
  yMax    <- min(1, max(yBreaks))

  xTitle <- if (options[["bfUpdatePlotDisp"]] == "comb")
    gettext("Hypothesis (Species Combination)")
  else
    gettext("Hypothesis (Species Count)")

  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = hyp, y = prob, fill = type)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.7) +
    ggplot2::scale_x_discrete(labels = function(l) parse(text = l)) +     # 解析 plotmath
    ggplot2::scale_y_continuous(name = gettext("Probability"),
                                breaks = yBreaks[yBreaks <= yMax], limits = c(0, yMax),
                                expand = ggplot2::expansion(mult = c(0, 0.08))) +
    ggplot2::scale_fill_manual(name = NULL,
                               values = c("Prior" = "#B0B0B0", "Posterior" = "#4DA3FF")) +
    ggplot2::labs(x = xTitle) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw(legend.position = "right") +
    ggplot2::theme(plot.margin = ggplot2::margin(t = 5, r = 15, b = 15, l = 5))

  posteriorPlot$plotObject <- p
  posteriorContainer[["posteriorPlot"]] <- posteriorPlot
}

.besDisplayPosteriorAccumu <- function(jaspResults, options) {
  posteriorContainer <- jaspResults[["posteriorContainer"]]
  if (is.null(posteriorContainer))
    return()

  likelihoodState <- jaspResults[["likelihoodState"]]
  if (is.null(likelihoodState))
    return()

  priorProbs <- jaspResults[["priorProbs"]]
  if (is.null(priorProbs))
    return()

  if (!is.null(posteriorContainer[["evidencePlot"]]))
    return()

  evidencePlot <- createJaspPlot(title = gettext("Evidence accumulation"),
                                  width = 480,
                                  height = 320)

  evidencePlot$dependOn(c("priorType", "priorS1", "priorS2", "priorS3",
                           "resetTrigger", "sampleTrigger",
                           "evidPlot", "eviPlotDisp",
                           "eviSpe", "eviComb"))

  num_sample <- likelihoodState$object$nSample
  if (num_sample == 0) {
    evidencePlot$setError(gettext(
      "No data. Sample some animals to begin"
    ))
    posteriorContainer[["evidencePlot"]] <- evidencePlot
    return()
  }

  res <- .computePosterior(jaspResults, options)

  if (options$eviPlotDisp == "numS") {
    traj   <- res$postS
    s_grp  <- sort(unique(res$s))
    labels <- sprintf("H[%d]", s_grp)

    selected <- options[["eviSpe"]]
    sel_s    <- as.integer(sub("[^0-9]*([0-9]+).*", "\\1", selected))  # "H₁ – 1 species" → 1
    keep     <- s_grp %in% sel_s

  } else {
    traj     <- res$postH
    prior_df <- jaspResults[["priorProbs"]]$object$priorSpe
    labels   <- .makePlotLabels(prior_df)

    selected    <- options[["eviComb"]]
    sp_base    <- c("cat", "horse", "pigeon")
    extractSet <- function(x) sort(regmatches(x, gregexpr(paste(sp_base, collapse = "|"), x))[[1]])

    sel_sets <- lapply(selected,    extractSet)
    res_sets <- lapply(res$species, extractSet)

    keep <- vapply(res_sets, function(rs)
      any(vapply(sel_sets, function(ss) setequal(ss, rs), logical(1))),
      logical(1))
  }

  if (length(selected) == 0) {
    evidencePlot$setError(gettext(
      "No hypothesis selected."
    ))
    posteriorContainer[["evidencePlot"]] <- evidencePlot
    return()
  }


  if (length(selected) > 0) {
    traj <- traj[keep, , drop = FALSE]
    labels <- labels[keep]

    if (nrow(traj) == 0)
      return()

  }

  plot_df <- data.frame(
    t    = rep(res$tSeq, each  = nrow(traj)),
    hyp  = factor(rep(labels, times = ncol(traj)), levels = labels),
    prob = as.vector(traj)
  )

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(res$tSeq)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, 1))
  pal     <- grDevices::hcl.colors(nlevels(plot_df$hyp), palette = "Dark 3")

  p <- ggplot2::ggplot(plot_df,
                       ggplot2::aes(x = t, y = prob, color = hyp, group = hyp)) +
    ggplot2::geom_line(linewidth = 1) +
    jaspGraphs::geom_point() +
    ggplot2::scale_x_continuous(name = gettext("Number of Samples"),
                                breaks = function(x) {
                                  b <- scales::pretty_breaks(n = 6)(x)
                                  b[b == floor(b) & b >= 0]
                                },
                                limits = range(xBreaks),
                                expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
    ggplot2::scale_y_continuous(name = gettext("Posterior Probability"),
                                breaks = yBreaks, limits = c(0, 1),
                                expand = ggplot2::expansion(mult = c(0, 0.02))) +
    ggplot2::scale_color_manual(name   = NULL,
                                values = pal,
                                labels = function(l) parse(text = l)) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw(legend.position = "right") +
    ggplot2::theme(plot.margin = ggplot2::margin(t = 5, r = 15, b = 15, l = 5))

  evidencePlot$plotObject <- p

  posteriorContainer[["evidencePlot"]] <- evidencePlot

}

.besDisplayPosteriorCompTable <- function(jaspResults, options) {
  posteriorContainer <- jaspResults[["posteriorContainer"]]
  if (is.null(posteriorContainer))
    return()

  likelihoodState <- jaspResults[["likelihoodState"]]
  if (is.null(likelihoodState))
    return()

  priorProbs <- jaspResults[["priorProbs"]]
  if (is.null(priorProbs))
    return()

  if (!is.null(posteriorContainer[["posteriorCompTable"]]))
    return()

  compTable <- createJaspTable(title = gettext("Posterior comparison table"))
  compTable$dependOn(c("priorType", "priorS1", "priorS2", "priorS3",
                            "resetTrigger", "sampleTrigger",
                            "compPostTable", "compPostHyp",
                            "compPostStat", "compPostSpe",
                            "compPostComb"))
  
  num_sample <- likelihoodState$object$nSample
  if (num_sample == 0) {
    compTable$setError(gettext(
      "No data. Sample some animals to begin"
    ))
    posteriorContainer[["posteriorCompTable"]] <- compTable
    return()
  }

  posteriorRes <- .computePosterior(jaspResults, options)
  table_df <- .generatePosteriorDfOdds(posteriorRes, options)
  
  if (options[["compPostHyp"]] == "numS") {
    pairs <- options[["compPostSpe"]]
    lookup_prior <- stats::setNames(table_df$prior, c("h1", "h2", "h3"))
    lookup_likelihood <- stats::setNames(table_df$likelihood, c("h1", "h2", "h3"))
    lookup_posterior <- stats::setNames(table_df$posterior, c("h1", "h2", "h3"))
    key_to_label <- stats::setNames(table_df$hyp,       c("h1", "h2", "h3"))
  } else {
    pairs <- options[["compPostComb"]]
    lookup_prior <- stats::setNames(table_df$prior,
                              c("h1a", "h1b","h1c",
                                "h2a", "h2b","h2c",
                                "h3"))
    lookup_likelihood <- stats::setNames(table_df$likelihood,
                              c("h1a", "h1b","h1c",
                                "h2a", "h2b","h2c",
                                "h3"))
    lookup_posterior <- stats::setNames(table_df$posterior,
                              c("h1a", "h1b","h1c",
                                "h2a", "h2b","h2c",
                                "h3"))
    key_to_label <- stats::setNames(table_df$hyp,       
                              c("h1a", "h1b","h1c",
                                "h2a", "h2b","h2c",
                                "h3"))
  }

  if (length(pairs) == 0) {
    compTable$setError(gettext("No comparisons selected."))
    posteriorContainer[["posteriorCompTable"]] <- compTable
    return()
  }

  compTable$addColumnInfo(name = "hypA", title = gettext("Hypothesis 1"), type = "string")
  compTable$addColumnInfo(name = "hypB", title = gettext("Hypothesis 2"), type = "string")
  compTable$addColumnInfo(name = "priorOdds", title = gettext("Prior odds"), type = "number")
  compTable$addColumnInfo(name = "bf", title = gettext("Likelihood ratio (BF)"), type = "number")
  compTable$addColumnInfo(name = "postOdds", title = gettext("Posterior odds"), type = "number")
  comp_i <- 0
  nan_prior <- c()
  nan_lik <- c()
  nan_post <- c()
  for (pair in pairs) {
    if (length(pair) < 2) next
    keyA <- pair[[1]]
    keyB <- pair[[2]]
    if (keyA == "" || keyB == "") next
    comp_i <- comp_i + 1

    hyp1_name <- key_to_label[keyA]
    hyp2_name <- key_to_label[keyB]
    prior_A <- lookup_prior[keyA]
    prior_B <- lookup_prior[keyB]
    lik_A   <- lookup_likelihood[keyA]
    lik_B   <- lookup_likelihood[keyB]
    post_A  <- lookup_posterior[keyA]
    post_B  <- lookup_posterior[keyB]

    prior_odds <- prior_A / prior_B
    bf         <- lik_A / lik_B
    post_odds  <- post_A / post_B

    if (is.nan(prior_odds))
      nan_prior <- c(nan_prior, comp_i)
    if (is.nan(bf))
      nan_lik <- c(nan_lik, comp_i)
    if (is.nan(post_odds))
      nan_post <- c(nan_post, comp_i)

    
    row_list <- list(
      hypA = hyp1_name,
      hypB = hyp2_name,
      priorOdds = prior_odds,
      bf = bf,
      postOdds = post_odds
    )

    compTable$addRows(row_list, rowNames = paste0("comp_", comp_i))  
  }

  for (i in nan_prior) {
    compTable$addFootnote(
      message = gettext("Neither of these hypotheses are likely a-priori."),
      colNames = "priorOdds",
      rowNames = paste0("comp_", i)
    )
  }

  for (i in nan_lik) {
    compTable$addFootnote(
      message = gettext("The data ruled out both hypotheses."),
      colNames = "bf",
      rowNames = paste0("comp_", i)
    )
  }

  for (i in nan_post) {
    compTable$addFootnote(
      message = gettext("Neither of these hypotheses are likely given prior belief and data."),
      colNames = "postOdds",
      rowNames = paste0("comp_", i)
    )
  }

  posteriorContainer[["posteriorCompTable"]] <- compTable
}

.besDisplayPosteriorCompPlot <- function(jaspResults, options) {
  posteriorContainer <- jaspResults[["posteriorContainer"]]
  if (is.null(posteriorContainer))
    return()
  
  likelihoodState <- jaspResults[["likelihoodState"]]
  if (is.null(likelihoodState))
    return()

  priorProbs <- jaspResults[["priorProbs"]]
  if (is.null(priorProbs))
    return()  
  
  if (!is.null(posteriorContainer[["posteriorCompPlot"]]))
    return()

    compPlotContainer <- createJaspContainer(title = gettext("Posterior Comparison"))
    compPlotContainer$dependOn(c("priorType", "priorS1", "priorS2", "priorS3",
                                  "resetTrigger", "sampleTrigger",
                                  "compPostPlot", "compPostHyp",
                                  "compPostStat", "compPostSpe",
                                  "compPostComb"))

    posteriorContainer[["posteriorCompPlot"]] <- compPlotContainer

    num_sample <- likelihoodState$object$nSample
    if (num_sample == 0) {
       return()
    }

    posteriorRes <- .computePosterior(jaspResults, options)
    table_df <- .generatePosteriorDfOdds(posteriorRes, options)

    if (options[["compPostHyp"]] == "numS") {
      pairs <- options[["compPostSpe"]]
      keys  <- c("h1", "h2", "h3")
      lookup_prior      <- stats::setNames(table_df$prior,      keys)
      lookup_likelihood <- stats::setNames(table_df$likelihood, keys)
      lookup_posterior  <- stats::setNames(table_df$posterior,  keys)
      key_to_label      <- stats::setNames(table_df$hyp,        keys)

      key_to_pizza_label <- stats::setNames(
        c("H₁", "H₂", "H₃"),
        keys
      )
    } else {
      pairs <- options[["compPostComb"]]
      keys  <- c("h1a", "h1b", "h1c", "h2a", "h2b", "h2c", "h3")
      lookup_prior      <- stats::setNames(table_df$prior,      keys)
      lookup_likelihood <- stats::setNames(table_df$likelihood, keys)
      lookup_posterior  <- stats::setNames(table_df$posterior,  keys)
      key_to_label      <- stats::setNames(table_df$hyp,        keys)

      key_to_pizza_label <- stats::setNames(
        c("H₁α", "H₁β", "H₁γ",
          "H₂α", "H₂β", "H₂γ",
          "H₃"),
        keys
      )
    }

    if (length(pairs) == 0) {
      compPlotContainer$setError(gettext("No comparisons selected."))
      return()
    }


    op_plot <- function(text) {
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5, label = text, size = 8) +
        ggplot2::theme_void()
    }

    comp_i <- 0

    for (pair in pairs) {
      if (length(pair) < 2) next
      keyA <- pair[[1]]
      keyB <- pair[[2]]
      if (keyA == "" || keyB == "") next

      comp_i <- comp_i + 1

      plotName <- paste0("Comparison_", comp_i)

      if (!is.null(compPlotContainer[[plotName]])) next

      hyp1_name       <- key_to_label[keyA]        # 给 message 文字用（plotmath/HTML）
      hyp2_name       <- key_to_label[keyB]
      hyp1_pizza_name <- key_to_pizza_label[keyA]  # 给 pizza labels 用（纯 Unicode）
      hyp2_pizza_name <- key_to_pizza_label[keyB]
      prior_A <- lookup_prior[keyA]
      prior_B <- lookup_prior[keyB]
      lik_A   <- lookup_likelihood[keyA]
      lik_B   <- lookup_likelihood[keyB]
      post_A  <- lookup_posterior[keyA]
      post_B  <- lookup_posterior[keyB]

      if (is.infinite(prior_A) && is.infinite(prior_B)) {
        prior_A <- 0.5; prior_B <- 0.5
        message_prior <- gettext("Neither hypotheses are likely a-priori.")
      } else if (prior_A == 0) {
        message_prior <- gettextf("%s is not likely a-priori.", hyp1_pizza_name)
      } else if (prior_B == 0) {
        message_prior <- gettextf("%s is not likely a-priori.", hyp2_pizza_name)
      } else if (prior_A / prior_B > 1) {
        message_prior <- gettextf("Prior odds are %.2f in favor of %s over %s.", prior_A / prior_B, hyp1_pizza_name, hyp2_pizza_name)
      } else if (prior_A / prior_B == 1) {
        message_prior <- gettextf("Prior odds are equal for %s and %s.", hyp1_pizza_name, hyp2_pizza_name)
      } else {
        message_prior <- gettextf("Prior odds are %.2f in favor of %s over %s.", prior_B / prior_A, hyp2_pizza_name, hyp1_pizza_name)
      }

      lik_ruled_A <- !is.finite(lik_A) || lik_A == 0
      lik_ruled_B <- !is.finite(lik_B) || lik_B == 0
      if (lik_ruled_A && lik_ruled_B) {
        message_lik <- gettext("The data ruled out both hypotheses.")
      } else if (lik_ruled_A) {
        message_lik <- gettextf("The data ruled out %s.", hyp1_pizza_name)
      } else if (lik_ruled_B) {
        message_lik <- gettextf("The data ruled out %s.", hyp2_pizza_name)
      } else if (lik_A / lik_B > 1) {
        message_lik <- gettextf("The data are %.2f times more likely under %s than under %s.", lik_A / lik_B, hyp1_pizza_name, hyp2_pizza_name)
      } else if (lik_A / lik_B == 1) {
        message_lik <- gettextf("The data are equally likely under %s and %s.", hyp1_pizza_name, hyp2_pizza_name)
      } else {
        message_lik <- gettextf("The data are %.2f times more likely under %s than under %s.", lik_B / lik_A, hyp2_pizza_name, hyp1_pizza_name)
      }

      post_ruled_A <- !is.finite(post_A) || post_A == 0
      post_ruled_B <- !is.finite(post_B) || post_B == 0
      if (post_ruled_A && post_ruled_B) {
        message_post <- gettext("Neither hypothesis is likely given the prior belief and data.")
      } else if (post_ruled_A) {
        message_post <- gettextf("%s is not likely given prior belief and data.", hyp1_pizza_name)
      } else if (post_ruled_B) {
        message_post <- gettextf("%s is not likely given prior belief and data.", hyp2_pizza_name)
      } else if (post_A / post_B > 1) {
        message_post <- gettextf("Posterior odds are %.2f in favor of %s over %s.", post_A / post_B, hyp1_pizza_name, hyp2_pizza_name)
      } else if (post_A / post_B == 1) {
        message_post <- gettextf("Posterior odds are equal for %s and %s.", hyp1_pizza_name, hyp2_pizza_name)
      } else {
        message_post <- gettextf("Posterior odds are %.2f in favor of %s over %s.", post_B / post_A, hyp2_pizza_name, hyp1_pizza_name)
      }

      # drawBFpizza 遇到 0/0, NA, NaN, Inf 会崩，is.finite() 统一拦截
      safe_vals <- function(a, b) {
        a <- unname(a); b <- unname(b)
        a_ok <- is.finite(a) && a > 0
        b_ok <- is.finite(b) && b > 0
        if ( a_ok &&  b_ok) return(c(a, b))
        if ( a_ok && !b_ok) return(c(1, 0))
        if (!a_ok &&  b_ok) return(c(0, 1))
        return(c(1, 1))
      }

      prop_prior <- safe_vals(prior_A, prior_B)
      prop_lik   <- safe_vals(lik_A,   lik_B)
      prop_post  <- safe_vals(post_A,  post_B)

      pizza_theme <- ggplot2::theme(
        plot.title   = ggplot2::element_text(hjust = 0.5, size = 16),
        plot.caption = ggplot2::element_text(hjust = 0.5, size = 15),
        plot.margin  = ggplot2::margin(t = 5, r = 15, b = 15, l = 5)
      )

      wrap_caption <- function(msg, width = 35)
        paste(strwrap(msg, width = width), collapse = "\n")

      prior_labels <- c(paste0("p(", hyp2_pizza_name, ")"),
                        paste0("p(", hyp1_pizza_name, ")"))
      lik_labels   <- c(paste0("p(data | ", hyp2_pizza_name, ")"),
                        paste0("p(data | ", hyp1_pizza_name, ")"))
      post_labels  <- c(paste0("p(", hyp2_pizza_name, " | data)"),
                        paste0("p(", hyp1_pizza_name, " | data)"))

      p_prior <- jaspGraphs::drawBFpizza(
        prop_prior,
        labels = prior_labels
      ) + ggplot2::labs(title   = gettext("Prior Odds"),
                        caption = wrap_caption(message_prior)) + pizza_theme

      p_lik <- jaspGraphs::drawBFpizza(
        prop_lik,
        labels = lik_labels
      ) + ggplot2::labs(title   = gettext("Likelihood Ratio"),
                        caption = wrap_caption(message_lik)) + pizza_theme

      p_post <- jaspGraphs::drawBFpizza(
        prop_post,
        labels = post_labels
      ) + ggplot2::labs(title   = gettext("Posterior Odds"),
                        caption = wrap_caption(message_post)) + pizza_theme

      p_combined <- cowplot::plot_grid(
        p_prior, op_plot("×"), p_lik, op_plot("="), p_post,
        nrow       = 1,
        rel_widths = c(3, 0.5, 3, 0.5, 3)
      )

      bfPlot <- createJaspPlot(title = plotName, width = 750, height = 300)
      bfPlot$position   <- comp_i
      bfPlot$plotObject <- p_combined
      compPlotContainer[[plotName]] <- bfPlot
    }
}

####################Posterior Prediction##################
.besCreatePredContainer <- function(jaspResults, options) {
  predContainer <- createJaspContainer(title = gettext("Posterior Prediction"))

  jaspResults[["predContainer"]] <- predContainer
}


.besDisplayPredExp <- function(jaspResults, options) {
  predContainer <- jaspResults[["predContainer"]]
  if (is.null(predContainer))
    return()

  if (!is.null(predContainer[["predIntroText"]]))
    return()

  text <- gettext("<p>Nice! Now we have our posterior belief ready. But before you leave the island, your boss wants to know about one more thing: if you observe another animal on the island, what would be the chances of seeing each of the three species, and what would be the chance of seeing something you have not observed before?</p>
                  <p>Think about it for a while, and you'll find a clever solution! For each species, you split the scenario according to different hypothetical species counts, calculate the probability of observing it accordingly, and add it up! This is what we will do in this section!</p>
                  <p>There are two features in this section: the posterior predictive table and plot, and the prediction change plot.</p>
                  <p>You can see the detailed breakdown of the probability of observing each of the three species, or seen vs. unseen species, in the posterior predictive table. The total probability of observing a species is simply the sum of the chances of seeing it under different hypotheses. The posterior predictive plot allows you to compare your posterior prediction with your prior prediction, so that you can see how the data changed your predictions. Again, you can make the following specifications:</p>
                  <ul>
                    <li>Switch between <b>Seen vs. Unseen</b> and <b>By individual species</b> to view predictions at different levels of detail.</li>
                    <li>For the table, switch between <b>Number of species</b> and <b>Species combinations</b> to break up the predictive probabilities by different hypothesis types.</li>
                    <li>For the plot, switch between <b>Last sample</b> and <b>Overall/Initial</b> to select what to focus on: the current sample only, or all samples combined.</li>
                  </ul>
                  <p>You can also view how your predictions change as you collect more samples, by enabling the prediction change plot. Just like the previous tables and plots, you can choose between <b>Seen vs. Unseen</b> and <b>By individual species</b> to control which data are displayed.</p>
                  <p><b>Something to think about:</b> do you find anything that does not really make sense in the posterior predictions? If so, how does it connect to the assumptions behind the mathematical model that we used? How are you going to improve the model?</p>
                  <p>Up to now, we have done all our tasks and are ready to go home, because a big reward awaits! However, something deep down in you is craving for another adventure. We got you! More adventures await you in the <b>Biodiversity Estimation (Full)</b> module! Take care and see you there!</p>")
  introHtml <- createJaspHtml(
    title    = gettext("Explanation of Posterior Prediction"),
    text     = text,
    position = 1
  )
  introHtml$dependOn("predExpText")

  predContainer[["predIntroText"]] <- introHtml
}

.besDisplayPredTable <- function(jaspResults, options) {
  predContainer <- jaspResults[["predContainer"]]
  if (is.null(predContainer)) return()

  likelihoodState <- jaspResults[["likelihoodState"]]
  if (is.null(likelihoodState)) return()

  if (is.null(jaspResults[["priorProbs"]])) return()

  if (!is.null(predContainer[["predTable"]]))
    return()
  
  predTable <- createJaspTable(title = gettext("Posterior prediction table"))
  predTable$dependOn(c("priorType", "priorS1", "priorS2", "priorS3",
                            "resetTrigger", "sampleTrigger",
                            "postPredTable", "postPredTableTyp",
                            "postPredTableHyp"))
  
  num_sample <- likelihoodState$object$nSample
  if (num_sample == 0) {
    predTable$setError(gettext(
      "No data. Sample some animals to begin"
    ))
    predContainer[["predTable"]] <- predTable
    return()
  }

  posteriorRes <- .computePosterior(jaspResults, options)
  post_vec <- posteriorRes$finalH   
  s_vec    <- posteriorRes$s        


  abundance_mat <- data.frame(
    cat    = c(1, 0, 0, 1, 1, 0, 1),
    horse  = c(0, 1, 0, 1, 0, 1, 1),
    pigeon = c(0, 0, 1, 0, 1, 1, 1)
  ) / c(rep(1, 3), rep(2, 3), 3)

  joint_mat <- abundance_mat * post_vec   


  norm_w <- function(v) { s <- sum(v); if (s == 0) rep(1/length(v), length(v)) else v/s }


  agg_by_S <- function(vec_H)
    c(sum(vec_H[1:3] * norm_w(post_vec[1:3])),
      sum(vec_H[4:6] * norm_w(post_vec[4:6])),
      vec_H[7])

  is_oldNew <- options[["postPredTableTyp"]] == "oldNew"
  is_numS   <- options[["postPredTableHyp"]] == "numS"


  if (is_oldNew) {
    spe_vec    <- c("cat", "horse", "pigeon")
    seen_flags <- rowSums(posteriorRes$likBatchH[1:3, -1, drop = FALSE]) != 0
    seen_spe   <- spe_vec[ seen_flags]
    unseen_spe <- spe_vec[!seen_flags]

    old_abund_H <- rowSums(abundance_mat[, seen_spe,   drop = FALSE])
    new_abund_H <- rowSums(abundance_mat[, unseen_spe, drop = FALSE])
  }


  if (is_oldNew && is_numS) {
    old_abund_S <- agg_by_S(old_abund_H)
    new_abund_S <- 1 - old_abund_S        
    post_S      <- posteriorRes$finalS
    table_df <- data.frame(
      s         = 1:3,
      posterior = post_S,
      old_abund = old_abund_S,
      new_abund = new_abund_S,
      old_joint = old_abund_S * post_S,
      new_joint = new_abund_S * post_S
    )

  } else if (is_oldNew && !is_numS) {
    table_df <- data.frame(
      s         = s_vec,
      spe_name  = posteriorRes$species,
      posterior = post_vec,
      old_abund = old_abund_H,
      new_abund = new_abund_H,
      old_joint = old_abund_H * post_vec,
      new_joint = new_abund_H * post_vec
    )

  } else if (!is_oldNew && is_numS) {
    post_S <- posteriorRes$finalS
    table_df <- data.frame(
      s            = 1:3,
      posterior    = post_S,
      cat_abund    = agg_by_S(abundance_mat$cat),
      horse_abund  = agg_by_S(abundance_mat$horse),
      pigeon_abund = agg_by_S(abundance_mat$pigeon),
      cat_joint    = as.numeric(rowsum(joint_mat$cat,    group = s_vec)),
      horse_joint  = as.numeric(rowsum(joint_mat$horse,  group = s_vec)),
      pigeon_joint = as.numeric(rowsum(joint_mat$pigeon, group = s_vec))
    )

  } else {  
    table_df <- data.frame(
      s            = s_vec,
      spe_name     = posteriorRes$species,
      posterior    = post_vec,
      cat_abund    = abundance_mat$cat,
      horse_abund  = abundance_mat$horse,
      pigeon_abund = abundance_mat$pigeon,
      cat_joint    = joint_mat$cat,
      horse_joint  = joint_mat$horse,
      pigeon_joint = joint_mat$pigeon
    )
  }

  table_df$hyp <- .makeTableLabels(table_df)
  table_df     <- table_df[table_df$posterior != 0, ]

  predTable$addColumnInfo(name = "hyp",       title = gettext("Hypothesis"),        type = "string")
  predTable$addColumnInfo(name = "s",         title = gettext("Species count"), type = "integer")
  if (!is_numS)
    predTable$addColumnInfo(name = "speName", title = gettext("Species"),           type = "string")
  predTable$addColumnInfo(name = "posterior", title = gettext("Posterior"),         type = "number")

  if (is_oldNew) {
    predTable$addColumnInfo(name = "oldAbundance", title = gettext("Seen"),   type = "number", overtitle = gettext("Abundance"))
    predTable$addColumnInfo(name = "newAbundance", title = gettext("Unseen"), type = "number", overtitle = gettext("Abundance"))
    predTable$addColumnInfo(name = "oldJoint",     title = gettext("Seen"),   type = "number", overtitle = gettext("Joint probability"))
    predTable$addColumnInfo(name = "newJoint",     title = gettext("Unseen"), type = "number", overtitle = gettext("Joint probability"))
  } else {
    predTable$addColumnInfo(name = "catAbundance",    title = gettext("Cat"),    type = "number", overtitle = gettext("Abundance"))
    predTable$addColumnInfo(name = "horseAbundance",  title = gettext("Horse"),  type = "number", overtitle = gettext("Abundance"))
    predTable$addColumnInfo(name = "pigeonAbundance", title = gettext("Pigeon"), type = "number", overtitle = gettext("Abundance"))
    predTable$addColumnInfo(name = "catJoint",        title = gettext("Cat"),    type = "number", overtitle = gettext("Joint probability"))
    predTable$addColumnInfo(name = "horseJoint",      title = gettext("Horse"),  type = "number", overtitle = gettext("Joint probability"))
    predTable$addColumnInfo(name = "pigeonJoint",     title = gettext("Pigeon"), type = "number", overtitle = gettext("Joint probability"))
  }

  for (i in seq_len(nrow(table_df))) {
    if (is_oldNew) {
      row_list <- list(
        hyp          = table_df$hyp[i],
        s            = table_df$s[i],
        posterior    = table_df$posterior[i],
        oldAbundance = table_df$old_abund[i],
        newAbundance = table_df$new_abund[i],
        oldJoint     = table_df$old_joint[i],
        newJoint     = table_df$new_joint[i]
      )
    } else {
      row_list <- list(
        hyp             = table_df$hyp[i],
        s               = table_df$s[i],
        posterior       = table_df$posterior[i],
        catAbundance    = table_df$cat_abund[i],
        horseAbundance  = table_df$horse_abund[i],
        pigeonAbundance = table_df$pigeon_abund[i],
        catJoint        = table_df$cat_joint[i],
        horseJoint      = table_df$horse_joint[i],
        pigeonJoint     = table_df$pigeon_joint[i]
      )
    }
    if (!is_numS) row_list$speName <- table_df$spe_name[i]
    predTable$addRows(row_list, rowNames = paste0("hyp_", i))
  }

  if (is_oldNew) {
    jt <- colSums(table_df[, c("old_joint", "new_joint")])
    total_row <- list(hyp = gettext("Total"), s = NA, posterior = NA,
                      oldAbundance = NA, newAbundance = NA,
                      oldJoint = unname(jt[1]), newJoint = unname(jt[2]))
  } else {
    jt <- colSums(table_df[, c("cat_joint", "horse_joint", "pigeon_joint")])
    total_row <- list(hyp = gettext("Total"), s = NA, posterior = NA,
                      catAbundance = NA, horseAbundance = NA, pigeonAbundance = NA,
                      catJoint    = unname(jt[1]),
                      horseJoint  = unname(jt[2]),
                      pigeonJoint = unname(jt[3]))
  }
  if (!is_numS) total_row$speName <- ""
  predTable$addRows(total_row, rowNames = "total")

  predContainer[["predTable"]] <- predTable
}

.besDisplayPredPlot <- function(jaspResults, options) {
  predContainer <- jaspResults[["predContainer"]]
  if (is.null(predContainer)) return()

  likelihoodState <- jaspResults[["likelihoodState"]]
  if (is.null(likelihoodState)) return()

  if (is.null(jaspResults[["priorProbs"]])) return()

  if (!is.null(predContainer[["predPlot"]]))
    return()

  predPlot <- createJaspPlot(title  = gettext("Posterior predictive plot"),
                              width  = 480,
                              height = 320)
  predPlot$dependOn(c("priorType", "priorS1", "priorS2", "priorS3",
                       "resetTrigger", "sampleTrigger",
                       "postPredPlot", "postPredPlotTyp", "postPredPlotPrior"))
  predContainer[["predPlot"]] <- predPlot

  num_sample <- likelihoodState$object$nSample
  if (num_sample == 0) {
    predPlot$setError(gettext("No data. Sample some animals to begin"))
    return()
  }

  posteriorRes <- .computePosterior(jaspResults, options)
  Tn           <- length(posteriorRes$tSeq) - 1  

  abundance_mat <- data.frame(
    cat    = c(1, 0, 0, 1, 1, 0, 1),
    horse  = c(0, 1, 0, 1, 0, 1, 1),
    pigeon = c(0, 0, 1, 0, 1, 1, 1)
  ) / c(rep(1, 3), rep(2, 3), 3)

  if (options[["postPredPlotPrior"]] == "init") {
    prior_H <- posteriorRes$postH[, 1]
  } else {
    prior_H <- posteriorRes$postH[, Tn]
  }
  post_H <- posteriorRes$finalH

  is_oldNew <- options[["postPredPlotTyp"]] == "oldNew"

  if (is_oldNew) {
    spe_vec <- c("cat", "horse", "pigeon")

    # Posterior: seen = all species observed so far (columns 2..Tn+1)
    post_seen_flags <- rowSums(posteriorRes$likBatchH[1:3, -1, drop = FALSE]) != 0

    # Prior: seen = species observed BEFORE the prior's time point
    if (options[["postPredPlotPrior"]] == "init") {
      prior_seen_flags <- rep(FALSE, 3)          # t=0: nothing seen yet
    } else {
      # lastBatch: t=Tn-1, seen = samples 1..(Tn-1) = columns 2..Tn
      if (Tn >= 2) {
        prior_seen_flags <- rowSums(posteriorRes$likBatchH[1:3, 2:Tn, drop = FALSE]) != 0
      } else {
        prior_seen_flags <- rep(FALSE, 3)        # only 1 sample → nothing seen before it
      }
    }

    # Abundance vectors for prior and posterior (based on their respective seen sets)
    prior_old_H <- rowSums(abundance_mat[,  prior_seen_flags, drop = FALSE])
    prior_new_H <- rowSums(abundance_mat[, !prior_seen_flags, drop = FALSE])
    post_old_H  <- rowSums(abundance_mat[,  post_seen_flags,  drop = FALSE])
    post_new_H  <- rowSums(abundance_mat[, !post_seen_flags,  drop = FALSE])

    x_labels <- c(gettext("Seen"), gettext("Unseen"))
    plot_df  <- data.frame(
      x    = factor(rep(x_labels, times = 2), levels = x_labels),
      type = factor(rep(c("Prior", "Posterior"), each = 2),
                    levels = c("Prior", "Posterior")),
      prob = c(sum(prior_old_H * prior_H), sum(prior_new_H * prior_H),
               sum(post_old_H  * post_H),  sum(post_new_H  * post_H))
    )

  } else {
    prior_spe <- as.numeric(colSums(abundance_mat * prior_H))
    post_spe  <- as.numeric(colSums(abundance_mat * post_H))

    x_labels <- c(gettext("Cat"), gettext("Horse"), gettext("Pigeon"))
    plot_df  <- data.frame(
      x    = factor(rep(x_labels, times = 2), levels = x_labels),
      type = factor(rep(c("Prior", "Posterior"), each = 3),
                    levels = c("Prior", "Posterior")),
      prob = c(prior_spe, post_spe)
    )
  }

  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, max(plot_df$prob, na.rm = TRUE)))
  yMax    <- min(1, max(yBreaks))

  p <- ggplot2::ggplot(plot_df,
                       ggplot2::aes(x = x, y = prob, fill = type)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.7) +
    ggplot2::scale_y_continuous(name   = gettext("Predictive Probability"),
                                breaks = yBreaks[yBreaks <= yMax],
                                limits = c(0, yMax),
                                expand = ggplot2::expansion(mult = c(0, 0.08))) +
    ggplot2::scale_fill_manual(name   = NULL,
                               values = c("Prior"     = "#B0B0B0",
                                          "Posterior" = "#4DA3FF")) +
    ggplot2::labs(x = gettext("Species Type")) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw(legend.position = "right") +
    ggplot2::theme(plot.margin = ggplot2::margin(t = 5, r = 15, b = 15, l = 5))

  predPlot$plotObject <- p
}

.besDisplayPredChangePlot <- function(jaspResults, options) {
  predContainer <- jaspResults[["predContainer"]]
  if (is.null(predContainer)) return()

  likelihoodState <- jaspResults[["likelihoodState"]]
  if (is.null(likelihoodState)) return()

  if (is.null(jaspResults[["priorProbs"]])) return()

  if (!is.null(predContainer[["predChangePlot"]]))
    return()

  predChangePlot <- createJaspPlot(
    title  = gettext("Predictive change plot"),
    width  = 480,
    height = 320
  )

  predChangePlot$dependOn(c("priorType", "priorS1", "priorS2", "priorS3",
                       "resetTrigger", "sampleTrigger",
                       "predChangePlot", "predChangePlotTyp"
  ))
  predContainer[["predChangePlot"]] <- predChangePlot

  num_sample <- likelihoodState$object$nSample
  if (num_sample == 0) {
    predChangePlot$setError(gettext("No data. Sample some animals to begin"))
    return()
  }

  posteriorRes <- .computePosterior(jaspResults, options)
  Tn           <- length(posteriorRes$tSeq) - 1   

  abundance_mat <- data.frame(
    cat    = c(1, 0, 0, 1, 1, 0, 1),
    horse  = c(0, 1, 0, 1, 0, 1, 1),
    pigeon = c(0, 0, 1, 0, 1, 1, 1)
  ) / c(rep(1, 3), rep(2, 3), 3)

  is_oldNew <- options[["predChangePlotTyp"]] == "oldNew"

  if (!is_oldNew) {
    pred_cat    <- numeric(Tn + 1)
    pred_horse  <- numeric(Tn + 1)
    pred_pigeon <- numeric(Tn + 1)

    for (t in 0:Tn) {
      post_t <- posteriorRes$postH[, t + 1]   

      pred_cat[t + 1]    <-  sum(abundance_mat$cat * post_t)
      pred_horse[t + 1]  <-  sum(abundance_mat$horse  * post_t)
      pred_pigeon[t + 1] <-  sum(abundance_mat$pigeon * post_t)
    }

    plot_df <- data.frame(
      batch = rep(0:Tn, 3),
      line  = factor(rep(c(gettext("Cat"), gettext("Horse"), gettext("Pigeon")), each = Tn + 1)),
      prob  = c(pred_cat, pred_horse, pred_pigeon)
    )

  } else {
    

    pred_old <- numeric(Tn + 1)   
    pred_new <- numeric(Tn + 1)   

    for (t in 0:Tn) {
      post_t <- posteriorRes$postH[, t + 1]

      if (t == 0) {
        seen_flags <- rep(FALSE, 3)
      } else {
        seen_flags <- rowSums(posteriorRes$likBatchH[1:3, 2:(t+1), drop=FALSE]) > 0
      }

      old_abund_H <- rowSums(abundance_mat[,  seen_flags, drop=FALSE])
      new_abund_H <- rowSums(abundance_mat[, !seen_flags, drop=FALSE])

      pred_old[t + 1] <- sum(old_abund_H * post_t)
      pred_new[t + 1] <- sum(new_abund_H * post_t)
    }

    plot_df <- data.frame(
      batch = rep(0:Tn, 2),
      line  = factor(rep(c(gettext("Seen"), gettext("Unseen")), each = Tn + 1)),
      prob  = c(pred_old, pred_new)
    )
  }

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(0:Tn)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, 1))
  pal     <- grDevices::hcl.colors(nlevels(plot_df$line), palette = "Dark 3")

  p <- ggplot2::ggplot(plot_df,
                      ggplot2::aes(x = batch, y = prob,
                                    color = line, group = line)) +  
                      ggplot2::geom_line(linewidth = 1) +
                      jaspGraphs::geom_point() +
                      ggplot2::scale_x_continuous(name = gettext("Number of Samples"), breaks = 0:Tn, limits = c(0, Tn),
                                          expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
                      ggplot2::scale_y_continuous(name = gettext("Predictive Probability"), breaks = yBreaks, limits = c(0, 1),
                                                   expand = ggplot2::expansion(mult = c(0, 0.02))) +
                      ggplot2::scale_color_manual(name = NULL, values = pal,
                                 labels = function(l) l) +
                      jaspGraphs::geom_rangeframe() +
                      jaspGraphs::themeJaspRaw(legend.position = "right") +
                      ggplot2::theme(plot.margin = ggplot2::margin(t = 5, r = 15, b = 15, l = 5))
  predChangePlot$plotObject <- p
  predContainer[["predChangePlot"]] <- predChangePlot
}

####################Helper Function#######################
.makePlotLabels <- function(prior_df) {
  greek_letters <- c("alpha", "beta", "gamma", "delta", "epsilon")
  vapply(seq_len(nrow(prior_df)), function(i) {
    s      <- prior_df$s[i]
    same_s <- which(prior_df$s == s)
    if (length(same_s) == 1)
      sprintf("H[%d]", s)
    else
      sprintf("H[%d*%s]", s, greek_letters[which(same_s == i)])
  }, character(1))
}

.makeTableLabels <- function(prior_df) {
  greek_unicode <- c("\u03b1", "\u03b2", "\u03b3", "\u03b4", "\u03b5")   # α β γ δ ε
  vapply(seq_len(nrow(prior_df)), function(i) {
    s      <- prior_df$s[i]
    same_s <- which(prior_df$s == s)
    if (length(same_s) == 1)
      sprintf("H<sub>%d</sub>", s)
    else
      sprintf("H<sub>%d%s</sub>", s, greek_unicode[which(same_s == i)])
  }, character(1))
}

.computeSpeciesLikelihood <- function(jaspResults) {
  likelihoodState <- jaspResults[["likelihoodState"]]
  likelihoodList <- likelihoodState$object
  likelihood_df <- likelihoodList$likelihoodDf

  prior_df <- jaspResults[["priorProbs"]]$object$priorSpe

  Lcols <- setdiff(names(likelihood_df), c("s", "species"))

  llH <- function(cols) if (length(cols) == 0) rep(0, nrow(likelihood_df))
                        else rowSums(log(as.matrix(likelihood_df[, cols, drop = FALSE])))

  logLik_H_now <- llH(Lcols)
  logLik_H_prev <- llH(if (length(Lcols) > 0) Lcols[-length(Lcols)] else character(0))

  lik_batch_H <- if (length(Lcols) == 0) rep(1, nrow(likelihood_df))
                 else likelihood_df[[Lcols[length(Lcols)]]]

  marginalS <- function(llH) tapply(llH, likelihood_df$s, function(x) {
    m <- max(x)
    if (!is.finite(m)) return(-Inf)
    m + log(sum(exp(x - m))) - log(length(x))
  })
  logOverall_S <- marginalS(logLik_H_now)
  logPrev_S <- marginalS(logLik_H_prev)
  logSample_S  <- logOverall_S - logPrev_S
  logSample_S[is.nan(logSample_S)] <- -Inf


  list(
    s            = likelihood_df$s,
    species      = likelihood_df$species,
    logPrior_H   = log(prior_df$prior_prob),
    logOverall_H = logLik_H_now,
    lik_batch_H  = lik_batch_H,
    logOverall_S = logOverall_S,
    logSample_S  = logSample_S
  )
}

.computePosterior <- function(jaspResults, options) {
  likelihood_df <- jaspResults[["likelihoodState"]]$object$likelihoodDf
  prior_df      <- jaspResults[["priorProbs"]]$object$priorSpe
  Lcols <- setdiff(names(likelihood_df), c("s", "species"))
  Tn    <- length(Lcols)
  nH    <- nrow(likelihood_df)
  s_vec <- likelihood_df$s
  logPrior_H <- log(prior_df$prior_prob)

  lse <- function(x) { m <- max(x); if (!is.finite(m)) return(-Inf); m + log(sum(exp(x - m))) }

  groups     <- sort(unique(s_vec))
  nS         <- length(groups)
  logPrior_S <- tapply(logPrior_H, s_vec, lse)

  postH   <- matrix(NA_real_, nH, Tn + 1)
  postS   <- matrix(NA_real_, nS, Tn + 1)
  likAllH <- matrix(NA_real_, nH, Tn + 1)
  likAllS <- matrix(NA_real_, nS, Tn + 1)

  for (t in 0:Tn) {
    cols_t   <- if (t == 0) character(0) else Lcols[seq_len(t)]
    logCum_H <- if (t == 0) rep(0, nH)
    else rowSums(log(as.matrix(likelihood_df[, cols_t, drop = FALSE])))
    logJoint_H <- logPrior_H + logCum_H
    logJoint_S <- tapply(logJoint_H, s_vec, lse)

    postH[, t + 1]   <- exp(logJoint_H - lse(logJoint_H))
    postS[, t + 1]   <- exp(logJoint_S - lse(logJoint_S))
    likAllH[, t + 1] <- exp(logCum_H)
    likAllS[, t + 1] <- exp(logJoint_S - logPrior_S)
  }


  likBatchH <- cbind(rep(1, nH),
                     if (Tn == 0) NULL else as.matrix(likelihood_df[, Lcols, drop = FALSE]))
  likBatchS <- matrix(1, nS, Tn + 1)
  if (Tn >= 1) {
    likBatchS[, -1] <- likAllS[, -1, drop = FALSE] / likAllS[, -(Tn + 1), drop = FALSE]
    likBatchS[is.nan(likBatchS)] <- 0
  }

  list(
    s         = s_vec,
    species   = likelihood_df$species,
    tSeq      = 0:Tn,
    postH     = postH,
    postS     = postS,
    likAllH   = likAllH,
    likAllS   = likAllS,
    likBatchH = likBatchH,
    likBatchS = likBatchS,
    priorH    = postH[, 1],
    finalH    = postH[, Tn + 1],
    priorS    = postS[, 1],
    finalS    = postS[, Tn + 1]
  )
}

.generatePosteriorDf <- function(posteriorRes, options, type) {
  s_num <- posteriorRes$s
  species_name <- posteriorRes$species
  time_seq <- posteriorRes$tSeq
  post_hyp <- posteriorRes$postH
  post_spe <- posteriorRes$postS
  prior_hyp <- posteriorRes$priorH
  prior_spe <- posteriorRes$priorS
  final_hyp <- posteriorRes$finalH
  final_spe <- posteriorRes$finalS
  lik_hyp_batch <- posteriorRes$likBatchH
  lik_hyp_all <- posteriorRes$likAllH
  lik_spe_batch <- posteriorRes$likBatchS
  lik_spe_all <- posteriorRes$likAllS
  if (type == "table") {
    if (options[["bfUpdateTableDisp"]] == "numS") {
      table_df <- data.frame(row.names = 1:3)
      table_df$hyp <- c(paste0("H<sub>", 1, "</sub>"),
                        paste0("H<sub>", 2, "</sub>"),
                        paste0("H<sub>", 3, "</sub>"))
      table_df$s <- c(1, 2, 3)

      if (options[["bfUpdateTableStat"]] == "batch") {
        table_df$prior <- post_spe[, ncol(post_spe) - 1]
        table_df$likelihood <- lik_spe_batch[, ncol(lik_spe_batch)]

        table_df$raw_posterior <- table_df$prior * table_df$likelihood
        table_df$posterior <- post_spe[, ncol(post_spe)]
      } else {
        table_df$prior <- prior_spe
        table_df$likelihood <- lik_spe_all[, ncol(lik_spe_all)]
        table_df$raw_posterior <- table_df$prior * table_df$likelihood
        table_df$posterior <- post_spe[, ncol(post_spe)]
      }
    }

    if (options[["bfUpdateTableDisp"]] == "comb") {
      table_df <- data.frame(row.names = 1:7)
      hyp_name <- .makeTableLabels(data.frame(s = s_num))
      table_df$hyp <- hyp_name
      table_df$s <- s_num
      table_df$spe <- species_name

      if (options[["bfUpdateTableStat"]] == "batch") {
        table_df$prior <- post_hyp[, ncol(post_hyp) - 1]
        table_df$likelihood <- lik_hyp_batch[, ncol(lik_hyp_batch) - 1]

        table_df$raw_posterior <- table_df$prior * table_df$likelihood
        table_df$posterior <- post_hyp[, ncol(post_hyp)]
      } else {
        table_df$prior <- prior_hyp
        table_df$likelihood <- lik_hyp_all[, ncol(lik_hyp_all)]

        table_df$raw_posterior <- table_df$prior * table_df$likelihood
        table_df$posterior <- post_hyp[, ncol(post_hyp)]
      }
    }


  }
  if (type == "plot") {
    if (options[["bfUpdatePlotDisp"]] == "numS") {
      table_df <- data.frame(row.names = 1:3)
      table_df$hyp <- .makePlotLabels(data.frame(s = 1:3))
      table_df$s <- c(1, 2, 3)

      if (options[["bfUpdatePlotStat"]] == "batch") {
        table_df$prior <- post_spe[, ncol(post_spe) - 1]
        table_df$likelihood <- lik_spe_batch[, ncol(lik_spe_batch)]

        table_df$raw_posterior <- table_df$prior * table_df$likelihood
        table_df$posterior <- post_spe[, ncol(post_spe)]
      } else {
        table_df$prior <- prior_spe
        table_df$likelihood <- lik_spe_all[, ncol(lik_spe_all)]
        table_df$raw_posterior <- table_df$prior * table_df$likelihood
        table_df$posterior <- post_spe[, ncol(post_spe)]
      }
    }

    if (options[["bfUpdatePlotDisp"]] == "comb") {
      table_df <- data.frame(row.names = 1:7)
      hyp_name <- .makePlotLabels(data.frame(s = s_num))
      table_df$hyp <- hyp_name
      table_df$s <- s_num
      table_df$spe <- species_name

      if (options[["bfUpdatePlotStat"]] == "batch") {
        table_df$prior <- post_hyp[, ncol(post_hyp) - 1]
        table_df$likelihood <- lik_hyp_batch[, ncol(lik_hyp_batch) - 1]

        table_df$raw_posterior <- table_df$prior * table_df$likelihood
        table_df$posterior <- post_hyp[, ncol(post_hyp)]
      } else {
        table_df$prior <- prior_hyp
        table_df$likelihood <- lik_hyp_all[, ncol(lik_hyp_all)]

        table_df$raw_posterior <- table_df$prior * table_df$likelihood
        table_df$posterior <- post_hyp[, ncol(post_hyp)]
      }
    }
  }
  return(table_df)
}

.generatePosteriorDfOdds <- function(posteriorRes, options) {
  s_num <- posteriorRes$s
  species_name <- posteriorRes$species
  time_seq <- posteriorRes$tSeq
  post_hyp <- posteriorRes$postH
  post_spe <- posteriorRes$postS
  prior_hyp <- posteriorRes$priorH
  prior_spe <- posteriorRes$priorS
  final_hyp <- posteriorRes$finalH
  final_spe <- posteriorRes$finalS
  lik_hyp_batch <- posteriorRes$likBatchH
  lik_hyp_all <- posteriorRes$likAllH
  lik_spe_batch <- posteriorRes$likBatchS
  lik_spe_all <- posteriorRes$likAllS
  if (options[["compPostHyp"]] == "numS") {
    table_df <- data.frame(row.names = 1:3)
    table_df$hyp <- c(paste0("H<sub>", 1, "</sub>"),
                      paste0("H<sub>", 2, "</sub>"),
                      paste0("H<sub>", 3, "</sub>"))
    table_df$s <- c(1, 2, 3)

    if (options[["compPostStat"]] == "batch") {
      table_df$prior <- post_spe[, ncol(post_spe) - 1]
      table_df$likelihood <- lik_spe_batch[, ncol(lik_spe_batch)]

      table_df$raw_posterior <- table_df$prior * table_df$likelihood
      table_df$posterior <- post_spe[, ncol(post_spe)]
    } else {
      table_df$prior <- prior_spe
      table_df$likelihood <- lik_spe_all[, ncol(lik_spe_all)]
      table_df$raw_posterior <- table_df$prior * table_df$likelihood
      table_df$posterior <- post_spe[, ncol(post_spe)]
    }
  }

  if (options[["compPostHyp"]] == "comb") {
    table_df <- data.frame(row.names = 1:7)
    hyp_name <- .makeTableLabels(data.frame(s = s_num))
    table_df$hyp <- hyp_name
    table_df$s <- s_num
    table_df$spe <- species_name

    if (options[["compPostStat"]] == "batch") {
      table_df$prior <- post_hyp[, ncol(post_hyp) - 1]
      table_df$likelihood <- lik_hyp_batch[, ncol(lik_hyp_batch) - 1]

      table_df$raw_posterior <- table_df$prior * table_df$likelihood
      table_df$posterior <- post_hyp[, ncol(post_hyp)]
    } else {
      table_df$prior <- prior_hyp
      table_df$likelihood <- lik_hyp_all[, ncol(lik_hyp_all)]

      table_df$raw_posterior <- table_df$prior * table_df$likelihood
      table_df$posterior <- post_hyp[, ncol(post_hyp)]
    }
  }
  return(table_df)
}


