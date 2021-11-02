# Tabulate Results of Stratified Models
# Present Results
# Temperature-UTI Project
# Joan Casey & Sebastian T. Rowland
# Updated 11/01/2021

####***********************
#### Table of Contents ####
####***********************

# 0: Preparation
# 1: Create Table

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
  source(here::here("scripts", "G_01_set_plottingObjects.R"))
}

####*********************
#### 1: Create Table ####
####*********************

# 1a Declare the exposure contrast of interest
contrast <- c("per05", "mean")

# 1b Setup table of models to tabulat
sensitivityList <- rep("main", 2)
subSetVarList <- rep("fullSet", 2)
subSetList <- rep("fullSet", 2)
ERConstraintList <- rep("selected", 2)
LRConstraintList <- rep("selected", 2)
indCumulList <- c("estInd", "estCumul")

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
  dplyr::select(-sensitivity, -subSetVar, -subSet, -label, -refT) %>%
  gather("lag_name", "estimate", -counterfactual_temp, -indCumul) %>%
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

# 1g Save one version of table
est.table %>%
  write_csv(here::here(outPath, "tables", "EE_FullSetAnalysis.csv"))

# 1h Put table in a tidy format, with 3 columns
est.table <- est.table %>%
  mutate(EE = paste0(
    format(round(fit_pc, 1), nsmall = 1), " (",
    format(round(lci_pc, 1), nsmall = 1), ", ",
    format(round(uci_pc, 1), nsmall = 1), ")"
  ))

# 1i Spread out effect estimates into 4 columns
est.table <- est.table %>%
  mutate(EXPName = paste0(indCumul, "_", counterfactual_temp)) %>%
  dplyr::select(EXPName, lag_day, EE) %>%
  spread(EXPName, EE) %>%
  dplyr::select(lag_day, contains("Ind"), contains("Cumul")) %>%
  rename(
    Lag = lag_day, "Lag-Specific Association" = estInd_16.4,
    "Cumulative Association" = estCumul_16.4
  )

# 1j Save table
est.table %>%
  write_csv(here::here(
    outPath, "manuscript",
    "eTable3_mainModel_effectEstimates.csv"
  ))
