# Prepare Data, Run Analysis, Present Results
# (Conduct entire project)
# Daily Temperature-UTI Project
# Joan Casey & Sebastian T. Rowland
# Updated 11/01/2021

####***********************
#### Table of Contents ####
####***********************

# N: Notes
# 0: Preparation
# 1: Prepare Data
# 2: Run Analysis
# 3: Present Results

####**************
#### N: Notes ####
####**************

# This is a top-level script, to run everything.

####********************
#### 0: Preparation ####
####********************

# 0a Create the folder structure, load packages, etc, if you haven't already
# here:here is helpful for file directories
if (!exists("ran_0_01")) {
  here::i_am("README.md") 
  source(here::here("scripts", "0_01_setUp_for_analysis.R"))
}

# 0b Begin timer
tic("Analysis and plotting completed.")

####*********************
#### 1: Prepare Data ####
####*********************

# 1a Run scripts to prepare data
# We run generate fake data if we are doing a code review
# the fake data means that any reviewer would get the same effect estimates
# and the reviewer can run the code on a computer outside of the Sutter system
if (outcomeName == "UTI") {
  source(here::here("scripts", "a_01a_prepare_tempData.R"))
  source(here::here("scripts", "a_02a_identifyCases.R"))
}
if (outcomeName == "fake") {
  source(here::here("scripts", "a_01b_generate_fakeTemp_forCodePrep.R"))
  source(here::here("scripts", "a_02b_generate_fakeCases_forCodePrep.R"))
}
# we assign exposure the same way regardless of real or fake data
source(here::here("scripts", "a_03_assignExposure_matchDays.R"))

####*********************
#### 2: Run Analysis ####
####*********************

# 2a Conduct main analyses
source(here::here("scripts", "c_01_fit_mainModel.R"))
source(here::here("scripts", "c_02_fit_EMMmodels.R"))

# 2b Conduct sensitivity analyses
source(here::here("scripts", "d_01_fit_sensitivityAnalyses.R"))

# 2c Conduct exploratory analyses
# source(here::here('scripts', 'e_02_fit_singleLagModels.R'))
# source(here::here('scripts', 'e_03_fit_unconstrainedDLNM.R'))
# source(here::here('scripts', 'e_04_fit_sensitivityAnalyses_setConstraints.R'))

####************************
#### 3: Present Results ####
####************************

# 3a Run scripts to present results
source(here::here("scripts", "g_01_set_plottingObjectsManuscript.R"))
# source(here::here('scripts', 'g_02_Table1_TableOne.R'))
source(here::here("scripts", "g_03_Fig1_expR_first4EvenLags.R"))
source(here::here("scripts", "g_04_Fig2_LagR_Ind_Cumul.R"))
source(here::here("scripts", "g_05_Fig3_seasonal_ER.R"))
source(here::here("scripts", "g_06_Fig4and5_EMM.R"))
source(here::here("scripts", "g_07_eTable1_TempDistribution.R"))
source(here::here("scripts", "g_08_eTable2_modelAIC.R"))
source(here::here("scripts", "g_09_eTable3_mainEffectEstimates.R"))
source(here::here("scripts", "g_10_eFig2_UTI_timingDistribution.R"))
source(here::here("scripts", "g_11_eFig3_sensitivity_forestPlot.R"))
source(here::here("scripts", "g_12_eFig4and5_sensitivity_lagRwithGhost.R"))
source(here::here("scripts", "g_13_numbers_for_Results.R"))

# 3b Tell the analyst that the analysis is done
toc()
