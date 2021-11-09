# Plot Stratified Associations
# Present Results
# Daily Temperature-UTI Project
# Joan Casey & Sebastian T. Rowland
# Updated 11/01/2021

####***********************
#### Table of Contents ####
####***********************

# N: Notes
# 0: Preparation
# 1: Create ER Plotting Function
  # 1A: Wrangle Model Estimates
  # 1B: Make Plot
# 2: Create LR Plotting Function
  # 2A: Wrangle Model Estimates
  # 2B: Make Plot
# 3: Create Plots

####****************
#### # N: Notes ####
####****************

# N: The function is based on very flexible function that can compare any two models, 
# so it include options not utiltized in this script

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

####************************************
#### 1: Create ER Plotting Function ####
####************************************

# 1a Name function 
plot_ERCurve_perLag_2Mods <- function(sensitivityA, subSetVarA, subSetA, ERConstraintA, LRConstraintA,
                                      sensitivityB, subSetVarB, subSetB, ERConstraintB, LRConstraintB,
                                      expRange, activeLag, refT) {
  # sensitivityA <- 'main' ; subSetVarA <- 'fullSet'; subSetA <- 'fullSet'
  # ERConstraintA <- 'selected'; LRConstraintA <- 'selected'
  # sensitivityB <- 'FandM' ; subSetVarB <- 'fullSet'; subSetB <- 'fullSet'
  # ERConstraintB <- 'selectedMain'; LRConstraintB <- 'selectedMain';expRange <- '0_100'
  # refT <- 'per05'
  
  
  ####*********************************
  #### 1A: Wrangle Model Estimates ####
  ####*********************************
  
  # 1A.a Determine exposure vector
  expRangeVec <- str_split_fixed(expRange, "_", 2)[1, ]
  
  # 1A.b Read Table
  est.table <- bind_rows(
    readEstimates(
      sensitivityA, subSetVarA, subSetA,
      ERConstraintA, LRConstraintA, "EstInd", expRange
    ),
    readEstimates(
      sensitivityB, subSetVarB, subSetB,
      ERConstraintB, LRConstraintB, "EstInd", expRange
    )
  )
  
  # 1A.c Isolate the relevant exposure constrats
  est.table <- est.table %>%
    filter(refT == !!refT)
  
  # 1A.d Determine type of contrast
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
  
  # 1A.e Wrangle estimate table
  est.table <- est.table %>%
    dplyr::select(-"sensitivity", -subSetVar, -subSet, -indCumul, -label, -refT) %>%
    gather("lag_name", "estimate", -counterfactual_temp, -mod_comp) %>%
    mutate(var_name = str_sub(lag_name, 1, 3), lag_day = as.numeric(str_sub(lag_name, 11))) %>%
    dplyr::select(-lag_name) %>%
    spread(var_name, estimate) %>%
    mutate(
      fit_pc = convertToPercent(fit),
      lci_pc = convertToPercent(lci), uci_pc = convertToPercent(uci)
    )
  
  # 1A.f Determine temperature values used in all plots
  # the principle is that the we the x-axis will cover the whole range of observed
  # temperatures,
  # but the actual meat of the plot will only cover the relevant range for that subset
  # 1A.f.i Parse the upper and lower percentiles
  expRangeVec <- str_split_fixed(expRange, "_", 2)[1, ]
  # 1A.f.ii Calculate values
  temp.per05 <- quantile(tempObs$temp, 0.05, type = 1)
  temp.per95 <- quantile(tempObs$temp, 0.95, type = 1)
  temp.lowerLim <- quantile(tempObs$temp, as.numeric(expRangeVec[1]) / 100, type = 1)
  temp.upperLim <- quantile(tempObs$temp, as.numeric(expRangeVec[2]) / 100, type = 1)
  
  # A.g Re-order sensitivity
  if (sensitivityA != sensitivityB) {
    est.table <- est.table %>%
      inner_join(nameArray, by = c("mod_comp" = "sensitivityCode")) %>%
      mutate(mod_comp = factor(sensitivityManu, levels = nameArray$sensitivityManu))
  }
  
  # 1A.h Identify highest and lowest estimates
  if (expRange == "0_100") {
    est.min <- -25
    est.max <- 15
  }
  if (expRange == "05_95") {
    est.min <- -25
    est.max <- 15
  }

  ####*******************
  #### 1B: Make Plot ####
  ####*******************
  
  # 1B.a Set the colors
  if (subSetVarA == 'catchmentArea' & subSetVarB == 'catchmentArea') {
    colSet <- colorArray$catchmentArea
  } else if (subSetVarA == 'ice' & subSetVarB == 'ice') {
    colSet <- colorArray$ICE
  } else if (sensitivityB == "main") {
    colSet <- colorArray$modContrast
  } else if (sensitivityB != "main") {
    colSet <- colorArray$modContrastGhost
  }
  
  # 1B.b Rename subsets
  if (subSetVarA == "catchmentArea") {
    legendTitle <- "Catchment Area"
    est.table <- est.table %>%
      mutate(mod_comp = if_else(mod_comp == "kpsc", "Kaiser Permanente", "Sutter Health"))
    textLabel <- "A"
  }
  if (subSetVarA == "ice") {
    legendTitle <- "Tract-Level \nPoverty"
    est.table <- est.table %>%
      mutate(mod_comp = if_else(mod_comp == "iceQ1", "ICE-I Q1", "ICE-I Q2-4"))
    textLabel <- "B"
  }
  
  # 2B.c Create the subset-specific exposure-response plot 
  ER.plot <- est.table %>%
    filter(lag_day == activeLag) %>%
    ggplot(aes(counterfactual_temp)) +
    geom_hline(yintercept = 0, color = "grey") +
    geom_vline(xintercept = temp.per05, color = "grey", linetype = "dashed") +
    geom_vline(xintercept = temp.per95, color = "grey", linetype = "dashed") +
    geom_ribbon(aes(ymin = lci_pc, ymax = uci_pc, fill = mod_comp), alpha = 0.2) +
    geom_line(aes(y = fit_pc, color = mod_comp)) +
    labs(
      x = expression(Observed ~ Daily ~ Temperature ~ (degree * C)),
      y = paste0("Change in UTI \n Diagnosis Rate (%)"),
      fill = legendTitle, color = legendTitle
    ) +
    scale_fill_manual(values = colSet) +
    scale_color_manual(values = colSet) +
    xlim(c(temp.lowerLim, temp.upperLim)) +
    scale_y_continuous(
      breaks = seq(est.min, est.max, by = 2),
      limits = c(est.min, est.max)
    ) +
    tema +
    theme(
      legend.title = element_text(size = 20),
      legend.text = element_text(size = 16),
      legend.position = c(0.46, 0.2),
      legend.key.size = unit(0.4, "cm")
    ) +
    theme(
      axis.title.y = element_text(size = 20),
      axis.text.x = element_text(size = 16),
      axis.text.y = element_text(size = 16)
    )
  # annotate(geom ='text', x = -2, y = 4, label = textLabel, size = 10)
}

####************************************
#### 2: Create LR Plotting Function ####
####************************************

# 2a Name function
plot_LRCurve_perExposure_2Mods <- function(sensitivityA, subSetVarA, subSetA, ERConstraintA, LRConstraintA,
                                           sensitivityB, subSetVarB, subSetB, ERConstraintB, LRConstraintB,
                                           contrast, curve) {
  # sensitivityA <- 'Main' ; subSetVarA <- 'fullSet'; subSetA <- 'fullSet'
  # ERConstraintA <- 'selected'; LRConstraintA <- 'selected'
  # sensitivityB <- '14DayLag' ; subSetVarB <- 'fullSet'; subSetB <- 'fullSet'
  # ERConstraintB <- 'selected'; LRConstraintB <- 'selected';
  # curve = 'Point'; contrast <- 'per10'
  # sensitivityA <- 'fallOnly' ; subSetVarA <- 'ice'; subSetA <- 'iceLow'
  # ERConstraintA <- 'selectedMain'; LRConstraintA <- 'selectedMain'
  # sensitivityB <- 'fallOnly' ; subSetVarB <- 'ice'; subSetB <- 'iceHigh'
  # ERConstraintB <- 'selectedMain'; LRConstraintB <- 'selectedMain';
  # curve = 'Point'; contrast <- c("per05", "mean")

  ####*********************************
  #### 2A: Wrangle Model Estimates ####
  ####*********************************

  # 2A.a Read model estimates
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

  # 2A.b Isolate the relevant exposure constrats
  est.table <- est.table %>%
    filter(refT == contrast[1][[1]]) %>%
    filter(label == contrast[2][[1]])

  # 2A.c Determine type of contrast
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

  # 2A.d Wrangle estimates
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

  # 2A.e Re-order sensitivity
  if (sensitivityA != sensitivityB) {
    est.table <- est.table %>%
      inner_join(nameArray, by = c("mod_comp" = "sensitivityCode")) %>%
      mutate(mod_comp = factor(sensitivityManu, levels = nameArray$sensitivityManu))
  }

  # 2A.f Rename subset name
  if (subSetVarA == "catchmentArea") {
    legendTitle <- "Catchment Area"
    est.table <- est.table %>%
      mutate(mod_comp = if_else(mod_comp == "kpsc", "Kaiser Permanente", "Sutter Health"))
  }
  if (subSetVarA == "ice") {
    legendTitle <- "ICE-Income"
    est.table <- est.table %>%
      mutate(mod_comp = if_else(mod_comp == "iceQ1", "ICE-I Q1", "ICE-I Q2-4"))
  }

  # 2A.g Isolate to whole-number lags
  # only used for the point estimate plot
  # 2A.g.i Determine the number of lags
  if (str_detect(paste0(sensitivityA, sensitivityB), "DayLag")) {
    numLag <- max(c(
      as.numeric(str_remove_all(sensitivityA, "[A-z]")),
      as.numeric(str_remove_all(sensitivityB, "[A-z]"))
    ),
    na.rm = TRUE
    )
  } else {
    numLag <- 7
  }
  # 2A.g.ii restrict to whole-number lags
  # note that we then add 0.3 to one of the lags so that the stratified effect 
  # estimates are adjacent in the plot
  if (curve == "Point") {
    est.table <- est.table %>%
      filter(lag_day %in% 0:numLag) %>%
      mutate(lag_day = if_else(mod_comp == est.table$mod_comp[1], lag_day, lag_day + 0.3))
  }

  # 2A.h Set range of y-axis
  est.min <- -7
  est.max <- 12

  ####*******************
  #### 2B: Make Plot ####
  ####*******************

  # 2B.a Set the colors
  if (subSetVarA == 'catchmentArea' & subSetVarB == 'catchmentArea') {
    colSet <- colorArray$catchmentArea
  } else if (subSetVarA == 'ice' & subSetVarB == 'ice') {
    colSet <- colorArray$ICE
  } else if (sensitivityB == "main") {
    colSet <- colorArray$modContrast
  } else if (sensitivityB != "main") {
    colSet <- colorArray$modContrastGhost
  }

  # 2B.b Determine type of curve
  if (curve == "Smooth") {
    ggLR1 <- geom_ribbon(aes(ymin = lci_pc, ymax = uci_pc, fill = mod_comp),
      alpha = 0.35, col = NA
    )
    ggLR2 <- geom_line(aes(y = fit_pc, col = mod_comp), size = 2)
  }
  if (curve == "Point") {
    ggLR1 <- geom_point(aes(y = fit_pc, fill = mod_comp, color = mod_comp, shape = mod_comp), size = 3)
    ggLR2 <- geom_errorbar(aes(ymin = lci_pc, ymax = uci_pc, color = mod_comp), size = 0.75)
  }

  # 2B.c Create plot
  TP.a <- est.table %>%
    ggplot(aes(lag_day)) +
    geom_hline(yintercept = 0, color = "grey") +
    ggLR1 +
    ggLR2 +
    scale_fill_manual(values = colSet) +
    scale_color_manual(values = colSet) +
    labs(
      y = paste0("Change in UTI \n Diagnosis Rate (%)"),
      x = paste0("Lag of Exposure"),
      fill = legendTitle,
      color = legendTitle, shape = legendTitle
    ) +
    scale_x_continuous(breaks = c(0, 3, 7, 10, 13)) +
    scale_y_continuous(
      breaks = seq(est.min, est.max, by = 2),
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
}


####*********************
#### 3: Create Plots ####
####*********************

# 3a Create plots models stratified by catchment area 
TP.ER.catchment <- plot_ERCurve_perLag_2Mods(
  "fallOnly", "catchmentArea", "sutter", "selectedMain", "selectedMain",
  "fallOnly", "catchmentArea", "kpsc", "selectedMain", "selectedMain",
  "0_100", 0, "per05"
)
TP.LR.catchment <- plot_LRCurve_perExposure_2Mods(
  "fallOnly", "catchmentArea", "sutter", "selectedMain", "selectedMain",
  "fallOnly", "catchmentArea", "kpsc", "selectedMain", "selectedMain",
  c("per05", "mean"), "Point"
)

# 3b Print plots 
png(here::here(
  outPath, "plots",
  paste0("Fig4_compareLagR_", "catchmentArea", "_Fall_Only.png")
),
width = ww.fig * 2, height = hh.fig * 0.75, res = rr.fig * 1
)
print(cowplot::plot_grid(TP.ER.catchment, TP.LR.catchment,
  ncol = 2, align = "hv", axis = "tb",
  labels = "AUTO",
  label_x = 0.22, label_y = 0.97, label_size = 20
))
dev.off()

# 3c Create plots models stratified by ICE category 
TP.ER.ice <- plot_ERCurve_perLag_2Mods(
  "fallOnly", "ice", "iceQ1", "selectedMain", "selectedMain",
  "fallOnly", "ice", "iceQ234", "selectedMain", "selectedMain",
  "0_100", 0, "per05"
)

TP.LR.ice <- plot_LRCurve_perExposure_2Mods(
  "fallOnly", "ice", "iceQ1", "selectedMain", "selectedMain",
  "fallOnly", "ice", "iceQ234", "selectedMain", "selectedMain",
  c("per05", "mean"), "Point"
)

# 3d Print plots
png(here::here(
  outPath, "plots",
  paste0("Fig5_compareLagR_", "ice", "_Fall_Only.png")
),
width = ww.fig * 2, height = hh.fig * 0.75, res = rr.fig * 1
)
print(cowplot::plot_grid(TP.ER.ice, TP.LR.ice,
  ncol = 2, align = "hv", axis = "tb",
  labels = "AUTO",
  label_x = 0.22, label_y = 0.97, label_size = 20
))
dev.off()
