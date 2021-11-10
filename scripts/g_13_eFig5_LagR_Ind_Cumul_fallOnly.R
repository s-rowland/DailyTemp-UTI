# Plot Lag-Response Curve for a Given Exposure Contrast
# Present Results
# Daily Temperature-UTI Project
# Joan Casey & Sebastian T. Rowland
# Updated 11/01/2021

####***********************
#### Table of Contents ####
####***********************

# N: Notes
# 0: Preparation
# 1: Create Plotting Function
  # 1A: Wrangle Model  Estimates
  # 1B: Make Plot
# 2: Create Plots of Single-Lag Associations
# 3: Create Plots of Cumulative Associations


####********************
#### 0: Preparation ####
####********************

# Na: Smooth plot?
# In our data setup, time is not smooth, but a step function - either you are in
# Day 1 or Day 2, there is no such thing in our data as Day 1.5.
# Day 1.5 would roughly correspond to exposure over the period of noon of Day 1
# to noon of Day 2.
# Since the crossbasis uses smooth terms like splines, we can actually estimate
# the coefficients for exposure on Day 1.5.
# Some researchers prefer to show point estimates for the individual lags,
# others prefer to show smooth curves across all of the lags.
# I don't have a strong preference; there is a tradeoff
# the individual point estimates best represents what we measured,
# but the smooth curve better illustrates the constraint we applied.
# It also depends on the number of lags you are showing.

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
plot_LRCurve_perExposure <- function(sensitivity, subSetVar, subSet,
                                     ERConstraint, LRConstraint, indCumul, contrast) {
  # sensitivity <- 'main' ; subSetVar <- 'season'; subSet <- 'fal'
  # ERConstraint <- 'selectedMain'; LRConstraint <- 'selectedMain'
  # contrast <-  c('per05', 'mean'); indCumul <- 'EstInd'

  ####**************************************
  #### 1A: Wrangle Main Model Estimates ####
  ####**************************************

  # 1A.a Read model estimates
  est.table <- readEstimates(
    sensitivity, subSetVar, subSet,
    ERConstraint, LRConstraint, indCumul
  )

  # 1A.b Isolate the relevant exposure contrasts
  est.table <- est.table %>%
    filter(refT == contrast[1][[1]]) %>%
    filter(label == contrast[2][[1]])

  # 1A.c Wrangle estimates
  est.table <- est.table %>%
    dplyr::select(-sensitivity, -subSetVar, -subSet, -indCumul, -label, -refT) %>%
    gather("lag_name", "estimate", -counterfactual_temp) %>%
    mutate(var_name = str_sub(lag_name, 1, 3), lag_day = as.numeric(str_sub(lag_name, 11))) %>%
    dplyr::select(-lag_name) %>%
    spread(var_name, estimate) %>%
    mutate(
      fit_pc = convertToPercent(fit),
      lci_pc = convertToPercent(lci), uci_pc = convertToPercent(uci)
    )

  # 1A.d Keep only the lags of interest
  est.table <- est.table %>% filter(lag_day %in% 0:14)

  # 1A.e set range of y-axis
  if (indCumul == "EstInd") {
    est.min <- -5
    est.max <- 10
    y.step <- 3
  }
  if (indCumul == "EstCumul") {
    est.min <- -10
    est.max <- 35
    y.step <- 3
  }

  ####*******************
  #### 1B: Make Plot ####
  ####*******************

  # 1B.a Create X-axis label
  if (indCumul == "EstInd") {
    XAxisLabel <- "Lag of Exposure"
  }
  if (indCumul == "EstCumul") {
    XAxisLabel <- "Cumulative Lags of Exposure"
  }

  # 1B.b Create plot
  TP.a <- est.table %>%
    ggplot(aes(lag_day)) +
    geom_hline(yintercept = 0, color = "grey") +
    geom_point(aes(y = fit_pc), fill = col.cold, color = col.cold, size = 3) +
    geom_errorbar(aes(ymin = lci_pc, ymax = uci_pc), color = col.cold, size = 0.75) +
    # scale_fill_manual(values = colorArray$tempContrast) +
    # scale_color_manual(values = colorArray$tempContrast) +
    labs(
      y = paste0("Change in UTI \n Diagnosis Rate (%)"),
      x = paste0(XAxisLabel)
    ) +
    scale_x_continuous(breaks = c(0, 3, 7, 10, 13)) +
    scale_y_continuous(
      breaks = seq(est.min, est.max, by = y.step),
      limits = c(est.min, est.max)
    ) +
    tema +
    theme(
      strip.background = element_blank(),
      strip.text = element_blank()
    ) +
    theme(panel.background = element_rect(fill = NA, color = "black")) +
    theme(
      legend.title = element_text(size = 20),
      legend.text = element_text(size = 16),
      legend.position = c(0.8, 0.84),
      legend.key.size = unit(0.4, "cm")
    ) +
    theme(
      axis.title.y = element_text(size = 20),
      axis.text.x = element_text(size = 16),
      axis.text.y = element_text(size = 16)
    )
}

####************************************************
#### 2: Create Plots of Single-Lag Associations ####
####************************************************

# 2a Creat plot objects
TP.ind <- plot_LRCurve_perExposure(
  "main", "season", "fal", "selectedMain",
  "selectedMain", "EstInd", c("per05", "mean")
)
TP.cumul <- plot_LRCurve_perExposure(
  "main", "season", "fal", "selectedMain",
  "selectedMain", "EstCumul", c("per05", "mean")
)

# 2b print the plots in a png
png(here::here(
  outPath, "manuscript",
  paste0("eFig5_LagR_singleLag_cumulative", "_Fall_Only.png")
),
width = ww.fig * 2, height = hh.fig * 0.75, res = rr.fig * 1
)

print(cowplot::plot_grid(TP.ind, TP.cumul,
  ncol = 2, align = "h", axis = "tb", rel_heights = c(2, 1),
  labels = "AUTO", label_x = 0.2, label_y = 0.95, label_size = 20
))

dev.off()
