# Plot Results of Sensitivity Analyses as Forect Plots 
# Present Results
# Temperature-UTI Project
# Joan Casey & Sebastian T. Rowland
# Updated 11/01/2021

####***********************
#### Table of Contents ####
####***********************

# N: Notes
# 0: Preparation
# 1: Create Plotting Function
  # 1A: Wrangle Model Independent Estimates
  # 1B: Make Plot
# 2: Create Plots

####********************
#### 0: Preparation ####
####********************

####********************
#### 0: Preparation ####
####********************

# 0a Create the folder structure, if you haven't already
if (!exists("ran_0_01")) {
  here::i_am("README.md")
  source(here::here("scripts", "0_01_setUp_for_Analysis.R"))
}

# 0b Create the plotting objects, if you haven't already
if (!exists("ran_g_01")) {
  source(here::here("scripts", "g_01_set_plottingObjectsManuscript.R"))
}

####*********************************
#### 1: Create Plotting Function ####
####*********************************

# 1a Declare the exposure contrast of interest
contrast <- c("per05", "mean")

# 1b Setup table of models to tabulate
sensitivityList <- rep(nameArray$sensitivityCode, 2)
subSetVarList <- rep("fullSet", 10)
subSetList <- rep("fullSet", 10)
ERConstraintList <- rep(c(
  "selected", "selectedMain", "selectedMain",
  "selectedMain", "selected"
), 2)
LRConstraintList <- rep(c(
  "selected", "selectedMain", "selectedMain",
  "selectedMain", "selected"
), 2)
indCumulList <- c(rep("estInd", 5), rep("estCumul", 5))

# 1c Readin estimates
est.list <- purrr::pmap(
  list(
    sensitivityList, subSetVarList, subSetList,
    ERConstraintList, LRConstraintList, indCumulList
  ),
  readEstimates
)
est.table <- bind_rows(est.list)

# 1d Keep only the exposure contrasts of interest
est.table <- est.table %>%
  filter(refT == contrast[1][[1]]) %>%
  filter(label == contrast[2][[1]])

# 1e Round counterfactual_temp so that it is presentable
est.table <- est.table %>%
  mutate(counterfactual_temp = round(counterfactual_temp, 2))

# 1f As usual, wrangle the estimates
est.table <- est.table %>%
  dplyr::select(-counterfactual_temp, -subSetVar, -subSet, -label, -refT) %>%
  gather("lag_name", "estimate", -sensitivity, -indCumul) %>%
  mutate(var_name = str_sub(lag_name, 1, 3), lag_day = as.numeric(str_sub(lag_name, 11))) %>%
  dplyr::select(-lag_name) %>%
  filter(!str_detect(lag_day, "\\.")) %>%
  spread(var_name, estimate) %>%
  mutate(lag_day = as.character(str_pad(lag_day, 2, "left", "0"))) %>%
  mutate(
    fit_pc = convertToPercent(fit),
    lci_pc = convertToPercent(lci),
    uci_pc = convertToPercent(uci)
  ) %>%
  mutate(se_pc = (uci_pc - lci_pc) / 1.96)

# 1g Keep only the effect estimates of interest
est.table <- est.table %>%
  filter((indCumul == "estInd" & lag_day == "00") |
    (indCumul == "estCumul" & lag_day == "13"))

# 1h Make the sensitivity names in proper format
est.table <- est.table %>%
  inner_join(nameArray, by = c("sensitivity" = "sensitivityCode")) %>%
  mutate(Sensitivity = factor(sensitivityManu, levels = rev(nameArray$sensitivityManu)))

####*********************************
#### 2: Define Plotting Function ####
####*********************************

# 2a Name function
make_sensitivity_forest <- function(activeIndCumul) {
  main <- est.table %>%
    filter(Sensitivity == "Main Model" & indCumul == activeIndCumul)

  # 2b Create plot 
  TP <- est.table %>%
    filter(indCumul == activeIndCumul) %>%
    ggplot(aes(Sensitivity)) +
    geom_blank() +
    geom_rect(aes(xmin = 0, xmax = 0.5 + nrow(nameArray)),
      ymin = main$lci_pc[1], ymax = main$uci_pc[1],
      alpha = 0.05, fill = col.cold
    ) +
    geom_hline(yintercept = 0, color = "grey") +
    geom_point(aes(y = fit_pc), color = col.cold, size = 3, shape = 18) +
    geom_errorbar(aes(ymin = lci_pc, ymax = uci_pc), color = col.cold) +
    tema +
    theme(legend.title = element_blank()) +
    coord_flip() +
    labs(
      y = expression(atop("Change in UTI", "Diagnosis Rate (%)")),
      x = paste0(" ")
    ) +
    theme(
      strip.background = element_blank(),
      strip.text.x = element_blank()
    ) +
    theme(
      axis.text.x = element_text(size = 13),
      axis.text.y = element_text(size = 13),
      axis.title.y = element_text(size = 14)
    )

  # 2c Return plot
  return(TP)
}

####*******************
#### 3: Make Plots ####
####*******************

# 3a Begin png
png(here::here(
  outPath, "manuscript",
  paste0("eFig3_sensitivity_forest", ".png")
),
width = ww.fig * 1.5, height = hh.fig * 1, res = rr.fig * 1
)

# 3b Arrange plots
print(cowplot::plot_grid(make_sensitivity_forest("estInd"),
  make_sensitivity_forest("estCumul"),
  ncol = 2, align = "hv", axis = "tb",
  # labels = 'AUTO',
  labels = c("Same-Day (Lag 0)", "Cumulative Lag 13"),
  label_x = -0.17, label_y = 0.97, label_size = 15
))

# 3c Finish
dev.off()
