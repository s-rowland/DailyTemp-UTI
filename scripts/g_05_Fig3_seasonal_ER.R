# Plot Seasonal Associations
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
                              indCumul, refT, examineThreshold = "") {
  # sensitivity <- 'main' ; subSetVar <- 'season'; subSet <- 'fal'
  # ERConstraint <- 'selectedMain'; LRConstraint <- 'selectedMain';expRange <- '0_100'
  # indCumul <- 'estInd'; refT <- 'mean'; examineThreshold <- ''

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

  # 1A.c Process table
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

  # 1A.d Restrict to lags of interest
  est.table <- est.table %>%
    filter(lagNum %in% c(0)) %>%
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

  # 1A.g Restrict to 5th and 95th percentile of observations
  est.table <- est.table %>%
    filter(counterfactual_temp > quantile(tempObs.sub$temp, 0.05, type = 1)) %>%
    filter(counterfactual_temp < quantile(tempObs.sub$temp, 0.95, type = 1))

  # 1A.h Set range for y-axis
  est.min <- -5
  est.max <- 3

  ####*******************
  #### 1B: Make Plot ####
  ####*******************

  # 1B.a Set up colors
  LC.l <- topo.colors(4)

  # 1B.b Set up which vertical lines to include
  vertList <- c(temp.per05, temp.per95)
  if (examineThreshold == "examineThreshold") {
    vertList <- c(vertList, 11:19)
  }

  # 1B.c Create modelName
  modelName <- paste(sensitivity, subSetVar, subSet,
    ERConstraint, LRConstraint,
    sep = "_"
  )

  # 1B.d Create the season-specific exposure-response plots
  ER.plot <- est.table %>%
    ggplot(aes(counterfactual_temp)) +
    geom_hline(yintercept = 0, color = "grey") +
    geom_vline(xintercept = vertList, color = "grey", linetype = "dashed") +
    geom_ribbon(aes(ymin = lci_pc, ymax = uci_pc, fill = Lag), alpha = 0.3) +
    geom_line(aes(y = fit_pc, color = Lag)) +
    labs(
      x = expression(Lag ~ 0 ~ Temperature ~ (degree * C)),
      y = paste0("Change in UTI \n Diagnosis Rate (%)")
    ) +
    xlim(c(temp.lowerLim, temp.upperLim)) +
    scale_y_continuous(
      breaks = seq(est.min, est.max, by = 1),
      limits = c(est.min, est.max)
    ) +
    scale_color_manual(values = LC.l) +
    scale_fill_manual(values = LC.l) +
    tema +
    theme(legend.position = "none")
}

####************************************************
#### 2: Create Plots of Single-Lag Associations ####
####************************************************

# 2a Create plot for each season
TP.fal <- plotERCurvePerLag(
  "main", "season", "fal", "selectedMain",
  "selectedMain", "05_95", "EstInd", "mean", "smallY"
)
TP.win <- plotERCurvePerLag(
  "main", "season", "win", "selectedMain",
  "selectedMain", "05_95", "EstInd", "mean", "smallY"
)
TP.spr <- plotERCurvePerLag(
  "main", "season", "spr", "selectedMain",
  "selectedMain", "05_95", "EstInd", "mean", "smallY"
)
TP.sum <- plotERCurvePerLag(
  "main", "season", "sum", "selectedMain",
  "selectedMain", "05_95", "EstInd", "mean", "smallY"
)

# 2b Combine plots into a png
png(here::here(
  outPath, "manuscript",
  paste0("Fig3_ER_Lag0_seasonal", ".png")
),
width = ww.fig * 2, height = hh.fig * 1.5, res = rr.fig * 1
)

print(cowplot::plot_grid(TP.fal, TP.win, TP.spr, TP.sum,
  ncol = 2, align = "hv", axis = "tb",
  labels = c("Fall", "Winter", "Spring", "Summer"),
  label_x = 0.2, label_y = 0.95, label_size = 20
))

dev.off()
