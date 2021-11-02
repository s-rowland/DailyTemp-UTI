# Plot Exposure-Response Curve for First 4 Even Lags 
# Present Results
# Daily Temperature-UTI Project
# Joan Casey & Sebastian T. Rowland
# Updated 11/01/2021

####***********************
#### Table of Contents ####
####***********************

# 0: Preparation
# 1: Create Plotting Function
  # 1A: Read Model Predictions
  # 1B: Make Plot
# 2: Create Plots of Single-Lag Associations
# 3: Create Plots of Cumulative Associations

####********************
#### 0: Preparation ####
####********************

# 0a Create the folder structure, if you haven't already
if (!exists("ran_0_01")) {
  here::i_am("README.md")
  source(here::here("scripts", "0_01_setUp_for_analysis.R"))
}

# 0b Create the plotting objects, if you haven't already
if (!exists("ran_g_01")) {
  source(here::here("scripts", "g_01_set_plottingObjectsManuscript.R"))
}

####*********************************
#### 1: Create Plotting Function ####
####*********************************

# 1a Name function
plotERCurvePerLag <- function(sensitivity, subSetVar, subSet,
                              ERConstraint, LRConstraint, expRange,
                              indCumul, refT, lagSet) {
  # sensitivity <- 'main' ; subSetVar <- 'fullSet'; subSet <- 'fullSet'
  # ERConstraint <- 'selected'; LRConstraint <- 'selected';expRange <- '0_100'
  # indCumul <- 'EstInd'
  # examineThreshold <- ''

  ####********************************
  #### 1A: Read Model Predictions ####
  ####********************************

  # 1A.a Read Table
  est.table <- readEstimates(
    sensitivity, subSetVar, subSet,
    ERConstraint, LRConstraint, indCumul
  )

  # 1A.b Restrict to effect estimates based on the reference temperature level
  # we chose
  est.table <- est.table %>%
    filter(refT == !!refT)

  # 1A.c Process Table
  est.table <- est.table %>%
    dplyr::select(-"sensitivity", -subSetVar, -subSet, -indCumul, -label, -refT) %>%
    gather("lagName", "estimate", -counterfactual_temp) %>%
    mutate(var_name = str_sub(lagName, 1, 3), lagNum = as.numeric(str_sub(lagName, 11))) %>%
    dplyr::select(-lagName) %>%
    spread(var_name, estimate) %>%
    mutate(
      fit_pc = convertToPercent(fit),
      lci_pc = convertToPercent(lci), uci_pc = convertToPercent(uci)
    )

  # 1A.d Restrict to Lags of Interest
  if (lagSet == "evenLags") {
    activeLags <- c(0, 2, 4, 6)
  }
  if (lagSet == "allCumulLags") {
    activeLags <- c(13)
  }
  est.table <- est.table %>%
    filter(lagNum %in% activeLags) %>%
    mutate(lagNum = as.character(lagNum)) %>%
    rename(Lag = lagNum)

  # 1A.e Determine temperature values used in all plots
  # the principle is that the we the x-axis will cover the whole range of observed
  # temperatures,
  # but the actual meat of the plot will only cover the relevant range for that subset
  # aka the values included in the histogram and the data
  # 1A.e.i Parse the upper and lower percentiles
  expRangeVec <- str_split_fixed(expRange, "_", 2)[1, ]
  # 1A.e.ii Calculate values
  temp.per05 <- quantile(tempObs$temp, 0.05, type = 1)
  temp.per95 <- quantile(tempObs$temp, 0.95, type = 1)
  temp.lowerLim <- quantile(tempObs$temp, as.numeric(expRangeVec[1]) / 100, type = 1)
  temp.upperLim <- quantile(tempObs$temp, as.numeric(expRangeVec[2]) / 100, type = 1)

  # 1A.f Determine temperature values for the histogram specific for this subset
  # 1A.f.i Subset the temperature observations
  if (subSetVar == "season" | subSetVar == "catchementArea") {
    tempObs.sub <- tempObs %>%
      rename(subSetVar = !!subSetVar) %>%
      filter(subSetVar == subSet)
  } else {
    tempObs.sub <- tempObs
  }

  # 1A.g Set range for y-axis
  if (expRange == "00_100" & lagSet == "evenLags") {
    est.min <- -10
    est.max <- 5.5
    stepY <- 2
  }
  if (expRange == "05_95" & lagSet == "evenLags") {
    est.min <- -3
    est.max <- 1.5
    stepY <- 0.5
  }

  if (expRange == "00_100" & lagSet == "allCumulLags") {
    est.min <- -17
    est.max <- 12
    stepY <- 2
  }
  if (expRange == "05_95" & lagSet == "allCumulLags") {
    est.min <- -10
    est.max <- 10
    stepY <- 2
  }

  ####*******************
  #### 1B: Make Plot ####
  ####*******************

  # 1B.a Make the histogram
  hist.plot <- ggplot(tempObs.sub) +
    geom_histogram(aes(temp), binwidth = 1) +
    geom_vline(xintercept = temp.per05, color = "grey", linetype = "dashed") +
    geom_vline(xintercept = temp.per95, color = "grey", linetype = "dashed") +
    xlim(c(temp.lowerLim, temp.upperLim)) +
    labs(
      x = expression(Observed ~ Daily ~ Temperature ~ (degree * C)),
      y = "Count"
    ) +
    tema +
    annotate("text", x = -2, y = 2100, label = "B", size = 6)

  # 1B.b Set up colors
  LC.l <- topo.colors(4)

  # 1B.c Set up which vertical lines to include
  vertList <- c(temp.per05, temp.per95)

  # 1B.c Create modelName
  modelName <- paste(sensitivity, subSetVar, subSet,
    ERConstraint, LRConstraint,
    sep = "_"
  )

  # 1B.d Create the lag-specific exposure-response plot for each lag
  ER.plot <- est.table %>%
    ggplot(aes(counterfactual_temp)) +
    geom_hline(yintercept = 0, color = "grey") +
    geom_vline(xintercept = vertList, color = "grey", linetype = "dashed") +
    geom_ribbon(aes(ymin = lci_pc, ymax = uci_pc, fill = Lag), alpha = 0.3) +
    geom_line(aes(y = fit_pc, color = Lag)) +
    labs(x = " ", y = paste0("Change in UTI \n Diagnosis Rate (%)")) +
    xlim(c(temp.lowerLim, temp.upperLim)) +
    scale_y_continuous(
      breaks = seq(est.min, est.max, by = stepY),
      limits = c(est.min, est.max)
    ) +
    scale_color_manual(values = LC.l) +
    scale_fill_manual(values = LC.l) +
    tema +
    theme(
      legend.position = c(0.88, 0.3),
      legend.key.size = unit(0.75, "cm"),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12)
    ) +
    annotate("text", x = -2, y = 4, label = "A", size = 6)

  # 1B.e Start the PDF
  png(here::here(
    outPath, "manuscript",
    paste0("Fig1_", "ER4Lags", "_", modelName, "_", expRange, "_", lagSet, ".png")
  ))

  # 1B.f Output the two plots in one column
  print(cowplot::plot_grid(ER.plot, hist.plot,
    ncol = 1, align = "v", axis = "rl", rel_heights = c(2, 1)
  ))
  # 1B.g Once the loop is complete, finish the pdf
  dev.off()
}

####************************************************
#### 2: Create Plots of Single-Lag Associations ####
####************************************************

# 2a Create plot
plotERCurvePerLag("main", "fullSet", "fullSet", "selected", "selected", "00_100", "EstInd", "per05", "evenLags")
