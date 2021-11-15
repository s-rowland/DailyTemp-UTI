# Generate Numeric Results for Results Section
# Present Results
# Temperature-UTI Project
# Joan Casey & Sebastian T. Rowland
# Updated 11/01/2021

####***********************
#### Table of Contents ####
####***********************

# N: Notes
# 0: Preparation
# 1: Numeric Results for Methods Section
# 2: Numeric Results for Main Model
# 3: Numeric Results for Effect Modification Models

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

####********************************************
#### 1: Numeric Results for Methods Section ####
####********************************************

# str: added code to determine the percentage of female cases
# 1a Percent cases that are female 
cases <- read_fst(here::here(
  "data", "prepared",
  paste0("cases_assignedTemp_", outcomeName, ".fst")
))

a.1.percentfemale <- sum(cases$case_count_sex_f) / sum(cases$case_count)

# 1b QAIC score 
tab.qaic <- read_csv(here::here(outPath, 'tables', 'model_QAIC.csv'))
tab.qaic <- tab.qaic %>% 
  filter(sensitivity == 'main')
a.2.winningQAIC <- max(tab.qaic$AkaikeWeight)

####***************************************
#### 2: Numeric Results for Main Model ####
####***************************************

# 2a Declare the exposure contrast of interest
contrastA <- c("per05", "mean")
contrastB <- c("mean", "per95")

# 2b Setup table of models to tabulat
sensitivityList <- rep("main", 2)
subSetVarList <- rep("fullSet", 2)
subSetList <- rep("fullSet", 2)
ERConstraintList <- rep("selected", 2)
LRConstraintList <- rep("selected", 2)
indCumulList <- c("estInd", "estCumul")

# 2c Readin estimates
est.table <- purrr::pmap(
  list(
    sensitivityList, subSetVarList, subSetList,
    ERConstraintList, LRConstraintList, indCumulList
  ),
  readEstimates
) %>% 
  bind_rows()

# 2d Keep only the exposure contrasts of interest
est.table <- est.table %>%
  filter(refT == contrastA[1][[1]] & label == contrastA[2][[1]] |
    refT == contrastB[1][[1]] & label == contrastB[2][[1]])

# 2e Round counterfactual_temp so that it is presentable
est.table <- est.table %>%
  mutate(counterfactual_temp = round(counterfactual_temp, 2))

# 2f As usual, wrangle the estimates
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

# 2g Put table in a tidy format, with 3 columns
est.table <- est.table %>%
  mutate(EE = paste0(
    format(round(fit_pc, 1), nsmall = 1), " (",
    format(round(lci_pc, 1), nsmall = 1), ", ",
    format(round(uci_pc, 1), nsmall = 1), ")"
  ))

# 2h Arrange the effect estimates so we have one exposure contrast per column
est.table <- est.table %>%
  mutate(EXPName = paste0(indCumul, "_", counterfactual_temp)) %>%
  dplyr::select(EXPName, lag_day, EE) %>%
  spread(EXPName, EE) %>%
  dplyr::select(lag_day, contains("Ind"), contains("Cumul"))

# 2i Get the values of interest
# note that the column names will be different for the synthetic data because the 
# fake temperature has a different distribution
b.1.EEper05toMeanInd0 <- est.table[1, 2]
b.2.EEmeanto95Ind0 <- est.table[1, 3]
b.3.EEper05toMeanCumul14 <- est.table[14, 4]
b.4.EEmeanto95Cumul14 <- est.table[14, 5]
b.5.EEfig2caption1 <- est.table[2, 2]
b.6.EEfig2caption2 <- est.table[2, 3]

####*******************************************************
#### 3: Numeric Results for Effect Modification Models ####
####*******************************************************

# 3a Setup table of models to tabulate
sensitivityList <- rep("main", 8)
subSetVarList <- c(rep("catchmentArea", 4), rep("ice", 4))
subSetList <- c(rep("kpsc", 2), rep("sutter", 2), rep("iceQ1", 2), rep("iceQ234", 2))
ERConstraintList <- rep("selectedMain", 8)
LRConstraintList <- rep("selectedMain", 8)
indCumulList <- rep(c("estInd", "estCumul"), 4)

# 3b Readin estimates
est.table <- purrr::pmap(
  list(
    sensitivityList, subSetVarList, subSetList,
    ERConstraintList, LRConstraintList, indCumulList
  ),
  readEstimates
) %>% 
  bind_rows()

# 3c Keep only the exposure contrasts of interest
est.table <- est.table %>%
  filter(refT == contrastA[1][[1]] & label == contrastA[2][[1]] |
    refT == contrastB[1][[1]] & label == contrastB[2][[1]])

# 3d Round counterfactual_temp so that it is presentable
est.table <- est.table %>%
  mutate(counterfactual_temp = round(counterfactual_temp, 2))

# 3e As usual, wrangle the estimates
est.table <- est.table %>%
  dplyr::select(-sensitivity, -subSetVar, -label, -refT) %>%
  gather("lag_name", "estimate", -counterfactual_temp, -subSet, -indCumul) %>%
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

# 3f Put table in a tidy format, with 3 columns
est.table <- est.table %>%
  mutate(EE = paste0(
    format(round(fit_pc, 1), nsmall = 1), " (",
    format(round(lci_pc, 1), nsmall = 1), ", ",
    format(round(uci_pc, 1), nsmall = 1), ")"
  ))

# 3g Arrange the effect estimates so we have one exposure contrast per column
est.table <- est.table %>%
  mutate(EXPName = paste0(indCumul, "_", counterfactual_temp, "_", subSet)) %>%
  dplyr::select(EXPName, lag_day, EE) %>%
  spread(EXPName, EE) %>%
  dplyr::select(lag_day, contains("Ind"), contains("Cumul"))

# 3h Extract values of interest
est.table.cumul <- est.table %>% 
  dplyr::select(contains('estCumul'))
c.1.EEper05toMeanInd14kpsc <- est.table.cumul[14, 3]
c.2.EEper05toMeanInd14Sutter <- est.table.cumul[14, 4]
c.3.EEper05toMeanInd14iceQ1 <- est.table.cumul[14, 1]
c.4.EEper05toMeanInd14iceQ234 <- est.table.cumul[14, 2]

# older version (perseved in case column names change with additional EMM analysis)
#c.1.EEper05toMeanInd14kpsc <- est.table$estCumul_16.4_kpsc[14]
#c.2.EEper05toMeanInd14Sutter <- est.table$estCumul_16.4_sutter[14]
#c.3.EEper05toMeanInd14iceQ1 <- est.table$estCumul_16.4_iceQ1[14]
#c.4.EEper05toMeanInd14iceQ234 <- est.table$estCumul_16.4_iceQ234[14]


####*************************************
#### 4: Numeric Results for Abstract ####
####*************************************

# 4a Setup table of models to tabulate
sensitivityList <- rep("main", 8)
subSetVarList <- rep("season", 8)
subSetList <- c(rep("fal", 2), rep("win", 2), rep("spr", 2), rep("sum", 2))
ERConstraintList <- rep("selectedMain", 8)
LRConstraintList <- rep("selectedMain", 8)
indCumulList <- rep(c("estInd", "estCumul"), 4)

# 4b Readin estimates
est.table <- purrr::pmap(
  list(
    sensitivityList, subSetVarList, subSetList,
    ERConstraintList, LRConstraintList, indCumulList
  ),
  readEstimates
) %>% 
  bind_rows()

# 4c Keep only the exposure contrasts of interest
est.table <- est.table %>%
  filter(refT == contrastB[1][[1]] & label == contrastB[2][[1]])

# 4d Round counterfactual_temp so that it is presentable
est.table <- est.table %>%
  mutate(counterfactual_temp = round(counterfactual_temp, 2))

# 4e As usual, wrangle the estimates
est.table <- est.table %>%
  dplyr::select(-sensitivity, -subSetVar, -label, -refT) %>%
  gather("lag_name", "estimate", -counterfactual_temp, -subSet, -indCumul) %>%
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

# 4f Put table in a tidy format, with 3 columns
est.table <- est.table %>%
  mutate(EE = paste0(
    format(round(fit_pc, 1), nsmall = 1), " (",
    format(round(lci_pc, 1), nsmall = 1), ", ",
    format(round(uci_pc, 1), nsmall = 1), ")"
  ))

# 4g Arrange the effect estimates so we have one exposure contrast per column
est.table <- est.table %>%
  mutate(EXPName = paste0(indCumul, "_", counterfactual_temp, "_", subSet)) %>%
  dplyr::select(EXPName, lag_day, EE) %>%
  spread(EXPName, EE) %>%
  dplyr::select(lag_day, contains("Ind"), contains("Cumul"))

# 4h Extract effect estimates of interest 
eeFall <- est.table %>% 
  dplyr::select(contains('Ind')) %>% 
  dplyr::select(contains('fal'))
d.a.eeFall <- eeFall[1,1] 
eeSpring <- est.table %>% 
  dplyr::select(contains('Ind')) %>% 
  dplyr::select(contains('spr'))
d.b.eeSpring <- eeSpring[1,1] 

####*************
#### 5: Save ####
####*************

# 5 Save numeric results 
data.frame(quantity = c(
  'a.1.percentfemale', 'a.2.winningQAIC',
  'b.1.EEper05toMeanInd0', 'b.2.EEmeanto95Ind0', 'b.3.EEper05toMeanCumul14',
  'b.4.EEmeanto95Cumul14', 'b.5.EEfig2caption1', 'b.6.EEfig2caption2', 
  'c.1.EEper05toMeanInd14kpsc', 'c.2.EEper05toMeanInd14Sutter', 
  'c.3.EEper05toMeanInd14iceQ1', 'c.4.EEper05toMeanInd14iceQ234', 
  'd.a.eeFall', 'd.b.eeSpring'), 
           value = c(
             a.1.percentfemale, a.2.winningQAIC,
             b.1.EEper05toMeanInd0, b.2.EEmeanto95Ind0, b.3.EEper05toMeanCumul14, 
             b.4.EEmeanto95Cumul14, b.5.EEfig2caption1, b.6.EEfig2caption2, 
             c.1.EEper05toMeanInd14kpsc, c.2.EEper05toMeanInd14Sutter, 
             c.3.EEper05toMeanInd14iceQ1, c.4.EEper05toMeanInd14iceQ234, 
             d.a.eeFall, d.b.eeSpring)) %>% 
  write_csv(here::here(outPath, "manuscript", 'numeric_results.csv'))
  

