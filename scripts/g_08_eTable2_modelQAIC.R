# Tabulate AIC of Models
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

# 1a Read in the AICs of the models 
aic.table <- read_csv(here::here("outputs", "tables", "model_AIC.csv"))

# 1b Keep only the AIC of the main model
aic.table <- aic.table %>%
  filter(sensitivity == "main" & subSetVar == "fullSet") %>%
  dplyr::select(ERConstraint, LRConstraint, AIC, AkaikeWeight)

# 1c Save table
aic.table %>%
  write_csv(here::here(
    outPath, "manuscript",
    "eTable2_AIC_table.csv"
  ))
