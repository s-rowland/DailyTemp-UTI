# Create TableOne
# Present Results
# Temperature-UTI Project
# Joan Casey & Sebastian T. Rowland
# Updated 05/18/2021

####***********************
#### Table of Contents ####
####***********************

# D: Description
# 0: Preparation
# 1: Organize Case Data
# 2: Create Table One

####********************
#### D: Description ####
####********************

# Table one of description of demographics, ect, of study population

# Placeholder for actual TableOne code

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

# 1a Name function
plot_LRCurve_perExposure_2Mods <- function(sensitivityA, subSetVarA, subSetA, ERConstraintA, LRConstraintA,
                                           sensitivityB, subSetVarB, subSetB, ERConstraintB, LRConstraintB,
                                           contrast, curve, filePrefix) {
  # sensitivityA <- 'main' ; subSetVarA <- 'fullSet'; subSetA <- 'fullSet'
  # ERConstraintA <- 'selected'; LRConstraintA <- 'selected'
  # sensitivityB <- '21DayLag' ; subSetVarB <- 'fullSet'; subSetB <- 'fullSet'
  # ERConstraintB <- 'selected'; LRConstraintB <- 'selected';
  # curve = 'Point'; contrast <- 'per10'; filePrefix <- 'eFig4'


  ####**************************************
  #### 1A: Wrangle Main Model Estimates ####
  ####**************************************

  # 1A.a Read model estimates
  est.table <- bind_rows(
    readEstimates(
      sensitivityA, subSetVarA, subSetA,
      ERConstraintA, LRConstraintA, "EstInd"
    ),
    readEstimates(
      sensitivityB, subSetVarB, subSetB,
      ERConstraintB, LRConstraintB, "EstInd"
    )
  )

  # 1A.b Keep only relevant exposure contrast
  est.table <- est.table %>%
    filter(refT == contrast[1][[1]]) %>%
    filter(label == contrast[2][[1]])

  # 1A.c Determine type of contrast
  if (sensitivityA != sensitivityB) {
    modelComparison <- paste0(
      subSetVarA, "_", subSetA, "_",
      ERConstraintA, "_", LRConstraintA, "_",
      "sensitivity", "_", sensitivityA, "_Vs_", sensitivityB
    )
    est.table <- est.table %>%
      mutate(mod_comp = sensitivity)
  } else if (subSetA != subSetB) {
    modelComparison <- paste0(
      sensitivityA, "_", ERConstraintA, "_", LRConstraintA, "_",
      "subSet", "_", subSetA, "_Vs_", subSetB
    )
    est.table <- est.table %>%
      mutate(mod_comp = subSet)
  } else if (ERConstraintA != ERConstraintB) {
    modelComparison <- paste0(
      sensitivityA, "_", subSetVarA, "_", subSetA, "_",
      "ERConstraint", "_", ERConstraintA, "_Vs_", ERConstraintB
    )
    est.table <- est.table %>%
      mutate(mod_comp = ERConstraint)
  } else if (LRConstraintA != LRConstraintB) {
    modelComparison <- paste0(
      sensitivityA, "_", subSetVarA, "_", subSetA, "_",
      "LRConstraint", "_", LRConstraintA, "_Vs_", LRConstraintB
    )
    est.table <- est.table %>%
      mutate(mod_comp = LRConstraint)
  }

  # 1A.d Wrangle estimates
  est.table <- est.table %>%
    dplyr::select(-sensitivity, -subSetVar, -subSet, -indCumul, -label, -refT) %>%
    gather("lag_name", "estimate", -counterfactual_temp, -mod_comp) %>%
    mutate(var_name = str_sub(lag_name, 1, 3), lag_day = as.numeric(str_sub(lag_name, 11))) %>%
    dplyr::select(-lag_name) %>%
    spread(var_name, estimate) %>%
    mutate(
      fit_pc = convertToPercent(fit),
      lci_pc = convertToPercent(lci), uci_pc = convertToPercent(uci)
    )

  # 1A.e Convert counterfactual_temp to character so that you can use discrete color scale
  est.table <- est.table %>%
    mutate(counterfactual_temp = as.character(round(counterfactual_temp, 2)))


  # 1A.f Isolate to whole-number lags
  # only for the point estimate plot
  # 1A.f.i Determine the number of lags
  if (str_detect(paste0(sensitivityA, sensitivityB), "DayLag")) {
    numLag <- max(c(
      as.numeric(str_remove_all(sensitivityA, "[A-z]")),
      as.numeric(str_remove_all(sensitivityB, "[A-z]"))
    ),
    na.rm = TRUE
    )
  } else {
    numLag <- 13
  }
  # 1A.f.ii restrict to whole-number lags
  if (curve == "Point") {
    est.table <- est.table %>%
      filter(lag_day %in% 0:numLag) %>%
      mutate(lag_day = if_else(mod_comp == est.table$mod_comp[1], lag_day, lag_day + 0.3))
  }
  
  # 1A.g Re-order sensitivity
  if (sensitivityA != sensitivityB) {
    est.table <- est.table %>%
      inner_join(nameArray, by = c("mod_comp" = "sensitivityCode")) %>%
      mutate(mod_comp = factor(sensitivityManu, levels = nameArray$sensitivityManu)) %>%
      rename(Model = mod_comp)
  }

  # 1A.h Set range of y-axis
  if (contrast == c("per05", "mean")) {
    est.min <- -1.5
    est.max <- 4
  }
  # if(contrast == '05_95'){est.min <- -1; est.max <- 10}
  # if(contrast == '05_95'){est.min <- -1; est.max <- 6}
  # if(contrast == '10_90'){est.min <- -1; est.max <- 4}
  # if(contrast == 'per10'){est.min <- --1; est.max <- 2}

  ####*******************
  #### 1B: Make Plot ####
  ####*******************

  # 1B.a Set the colors
  if (sensitivityB == "main") {
    colSet <- colorArray$modContrast
  }
  if (sensitivityB != "main") {
    colSet <- colorArray$modContrastGhost
  }

  # 1B.b Determine type of curve
  if (curve == "Smooth") {
    ggLR1 <- geom_ribbon(aes(ymin = lci_pc, ymax = uci_pc, fill = Model),
      alpha = 0.35, col = NA
    )
    ggLR2 <- geom_line(aes(y = fit_pc, col = Model), size = 2)
  }
  if (curve == "Point") {
    ggLR1 <- geom_point(aes(y = fit_pc, fill = Model, color = Model, shape = Model), size = 3)
    ggLR2 <- geom_errorbar(aes(ymin = lci_pc, ymax = uci_pc, color = Model), size = 0.75)
  }

  # 1B.c Create plot
  TP.a <- est.table %>%
    ggplot(aes(lag_day)) +
    geom_hline(yintercept = 0, color = "grey") +
    ggLR1 +
    ggLR2 +
    scale_fill_manual(values = colSet) +
    scale_color_manual(values = colSet) +
    labs(
      y = paste0("Change in UTI \n Diagnosis Rate (%)"),
      x = paste0("Lag of Exposure")
    ) +
    scale_x_continuous(breaks = seq(0, numLag, by = 2)) +
    scale_y_continuous(
      breaks = seq(est.min, est.max, by = 0.5),
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
      legend.position = c(0.75, 0.75),
      legend.key.size = unit(0.4, "cm")
    ) +
    theme(
      axis.title.y = element_text(size = 20),
      axis.text.x = element_text(size = 16),
      axis.text.y = element_text(size = 16)
    )

  # 1B.c Return plot
  return(TP.a)
}

####*********************
#### 2: Create Plots ####
####*********************

# 2a Plot Sensitivity Models
TP.FandM <- plot_LRCurve_perExposure_2Mods(
  "main", "fullSet", "fullSet", "selected", "selected",
  "FandM", "fullSet", "fullSet", "selectedMain", "selectedMain",
  c("per05", "mean"), "Point"
)
TP.21DayLag <- plot_LRCurve_perExposure_2Mods(
  "main", "fullSet", "fullSet", "selected", "selected",
  "21DayLag", "fullSet", "fullSet", "selected", "selected",
  c("per05", "mean"), "Point"
)

# 2b Save plots as png
png(here::here(
  outPath, "manuscript",
  paste0("eFig4_LagR_sensitivity_ghost", ".png")
),
width = ww.fig * 2, height = hh.fig * 0.75, res = rr.fig * 1
)

print(cowplot::plot_grid(TP.FandM, TP.21DayLag,
  ncol = 2, align = "h", axis = "tb", rel_heights = c(2, 1),
  labels = "AUTO", label_x = 0.2, label_y = 0.95, label_size = 20
))

dev.off()
