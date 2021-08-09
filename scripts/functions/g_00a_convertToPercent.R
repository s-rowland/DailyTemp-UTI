# Convert RR to percent change
# Functions
# Daily Temperature-UTI Project 
# Joan Casey & Sebastian T. Rowland
# Updated 05/19/2021

####***********************
#### Table of Contents #### 
####***********************

# D: Description
# 1: Create Function

####********************
#### D: Description ####
####********************

# This function is used to convert rate ratio to percent change

####************************
#### 1: Create Function ####
####************************

# 1a Declare convert to percent function 
convertToPercent <- function(x){
  100 * (x - 1)
}
