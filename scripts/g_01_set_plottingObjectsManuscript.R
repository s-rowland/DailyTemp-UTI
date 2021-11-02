# Set features for plots and create related objects
# Present Results
# Temperature-UTI Project
# Joan Casey & Sebastian T. Rowland
# Updated 11/01/2021

####***********************
#### Table of Contents ####
####***********************

# D: Description
# 0: Preparation
# 1: Set Common Plotting Features
# 2: Read Tables Required for Plotting

####********************
#### D: Description ####
####********************

# By setting common plotting features in a separate script
# We can ensure consistency across plots
# It also helps make some choices more explicit
# eg, this is the script where I set the colors for subgroups

####********************
#### 0: Preparation ####
####********************

# 0a Create the folder structure, if you haven't already
if (!exists("ran_0_01")) {
  here::i_am("README.md")
  source(here::here("scripts", "0_01_setUp_for_analysis.R"))
}

# 0b Add marker
ran_g_01 <- "ran_g_01"

####*************************************
#### 1: Set Common Plotting Features ####
####*************************************

# 1a Set common plot dimensions
# not all figures are this size, but these are common sizes
hh.fig <- 1000
ww.fig <- 1000
rr.fig <- 150

hh.efig <- 600
ww.efig <- 600
rr.efig <- 200

# 1b Create plotting theme
# this winds up not being used because some of the figures contain multiple
# plots, and thus their size needs to be uniquely chosen.
# tema <-   theme_classic() +
# theme(plot.title = element_text(size = 36, vjust = 1, hjust =0.5)) +
# theme(axis.title.x = element_text(size = 16, vjust =0, lineheight=0.25)) +
# theme(axis.title.y = element_text(size = 16, angle = 90, vjust= 0.5)) +
# theme(axis.text.x = element_text(size = 12)) +
# theme(axis.text.y = element_text(size = 12, angle = 0))

# 1c Set individual colors
col.cold <- "deepskyblue2"
col.hot <- "tomato2"
col.modA <- "darkorchid1"
col.modB <- "burlywood4"
col.modMain <- "orange"
col.modGhost <- "grey73"

# 1d Set order of colors
# this is used in the scale_manual step of the plot
# by setting the colors here, we can ensure consistent coloring scheme across
# the manuscript
# if we do any subsetting, we can include those colors in this array
colorArray <- list(
  lag = terrain.colors(7),
  tempContrast = c(col.cold, col.hot),
  modContrast = c(col.modA, col.modB),
  modContrastGhost = c(col.modGhost, col.cold)
)

# 1e Set order of sensitivity analyses
nameArray <- data.frame(
  sensitivityCode = c("main", "noRH", "RHdlnm", "FandM", "21DayLag"),
  sensitivityManu = c(
    "Main Model", "No RH Adjustment", "DLNM for RH",
    "Females and Males", "21 Lag Days"
  )
)

# 1f Create plotting theme
tema <- theme_classic() +
  theme(plot.title = element_text(size = 26, vjust = 1, hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 19, vjust = 0, lineheight = 0.25)) +
  theme(axis.title.y = element_text(size = 19, angle = 90, vjust = 0.5)) +
  theme(axis.text.x = element_text(size = 16)) +
  theme(axis.text.y = element_text(size = 16, angle = 0))

####******************************************
#### 2: Read Tables Required for Plotting ####
####******************************************

# 2a Read case-temperature data set
dta <- read_fst(here::here(
  "data", "prepared",
  paste0("cases_assignedTemp_", outcomeName, ".fst")
))
dta <- dta %>%
  mutate(MM = month(adate)) %>%
  mutate(season = case_when(
    MM %in% c(12, 1, 2) ~ "win",
    MM %in% c(3, 4, 5) ~ "spr",
    MM %in% c(6, 7, 8) ~ "sum",
    MM %in% c(9, 10, 11) ~ "fal"
  ))

# 2b Make temperature dataset
tempObs <- dta %>%
  group_by(adate, fips, catchmentArea, season) %>%
  summarize(temp = mean(tLag00))
# tempSeq <- tempSeq$temp

# 2c Read selected models table
selectedConstraints <- read_csv(here::here(
  outPath, "tables",
  "selected_constraints.csv"
))
