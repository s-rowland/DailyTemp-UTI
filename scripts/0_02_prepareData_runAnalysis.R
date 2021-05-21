# Prepare Data, Run Analysis, Present Results 
# (Conduct entire project)
# Daily Temperature-UTI Project 
# Joan Casey & Sebastian T. Rowland
# Updated 05/19/2021

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

# This is a master script, to run everything. 

####********************
#### 0: Preparation #### 
####********************

# 0a Tell the analyst that the analysis is beginning 
StartTime_0_02 <- Sys.time()
print(paste('begin 0_02 at', StartTime_0_02))

# 0b Create the folder structure, load packages, etc, if you haven't already
if (!exists('ran_0_01')){
  here::i_am('README.md')
  source(here::here('scripts', '0_01_setUp_for_Analysis.R'))
}

####*********************
#### 1: Prepare Data #### 
####*********************

# 1a Run scripts to prepare data 
# you might choose to split up the data preparation into multiple scripts
#source(here::here('scripts', 'a_01_prepare_tempData.R'))
source(here::here('scripts', 'a_01b_generate_fakeTemp_forCodePrepR.R'))
#source(here::here('scripts', 'a_02a_identifyCases.R'))
source(here::here('scripts', 'a_02b_generate_fakeCases_forCodeReview.R'))
source(here::here('scripts', 'a_03_assign_exposure.R'))

####*********************
#### 2: Run Analysis #### 
####*********************

# 2a Conduct main analyses 
source(here::here('scripts', 'c_01_fit_mainModel.R'))

# 2b Conduct sensitivity analyses 

# 2c Conduct exploratory analyses

####************************
#### 3: Present Results #### 
####************************

# 3a Run scripts to present results 
source(here::here('scripts', 'g_01_set_plottingObjects.R'))
#source(here::here('scripts', 'g_02_Table1_TableOne.R'))
source(here::here('scripts', 'g_03_plot_exposureResponse_allLags_stationary.R'))
source(here::here('scripts', 'g_04_plot_lagResponseCurve.R'))
source(here::here('scripts', 'g_05_tabulate_effectEstimates.R'))

# 3b Tell the analyst that the analysis is done
cat('completed 0_02 at ', paste(Sys.time()), 
    '\n total time: ', round((Sys.time() - StartTime_0_02), 1), ' min \n')

rm(StartTime_0_02)
