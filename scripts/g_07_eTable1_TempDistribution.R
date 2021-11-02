# Examine Temperature Distribution Across Catchment Area
# Present Results
# Temperature-UTI Project
# Joan Casey & Sebastian T. Rowland
# Updated 11/01/2021

####***********************
#### Table of Contents ####
####***********************

# N: Notes
# 0: Preparation
# 1: Read Data
# 2: Plot Temperature Distribution
# 3: Plot Case Distribution

####**************
#### N: Notes ####
####**************

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

####*********************
#### 1: Create Table ####
####*********************

# 1a Read outcome data
cases <- read_fst(here::here(
  "data", "prepared",
  paste0("cases_assignedTemp_", outcomeName, ".fst")
))

# 1b Generate set of observed temperatures
tempObs <- cases %>%
  group_by(adate, fips, catchmentArea) %>%
  summarize(temp = mean(tLag00)) %>%
  mutate(catchmentArea = case_when(
    catchmentArea == "kpsc" ~ "Kaiser Permanente",
    catchmentArea == "sutter" ~ "Sutter Health",
    catchmentArea == "Total" ~ "Combined"
  ))

# 1c Create table
tempObs %>%
  bind_rows(mutate(tempObs, catchmentArea = "Total")) %>%
  group_by(catchmentArea) %>%
  summarize(
    mean = round(mean(temp), 2),
    SD = round(sd(temp), 2),
    Min = round(min(temp), 2),
    tenth = round(quantile(temp, 0.1), 2),
    Q1 = round(quantile(temp, 0.25), 2),
    Median = round(median(temp), 2),
    Q3 = round(quantile(temp, 0.75), 2),
    ninetith = round(quantile(temp, 0.9), 2),
    Max = round(max(temp), 2)
  ) %>%
  write_csv(here::here(outPath, "manuscript", "eTable1_temp_distribution.csv"))

# 1d Create violin plot (currently not included in manuscript)
png(here::here(
  outPath, "manuscript",
  paste0("temp_distribution_violin", ".png")
),
width = ww.fig * 1, height = hh.fig * 1.5, res = rr.fig * 1
)

ggplot(tempObs) +
  geom_violin(aes(
    x = catchmentArea, y = temp,
    color = catchmentArea, fill = catchmentArea
  )) +
  labs(
    x = "Catchement Area",
    y = expression(Observed ~ Daily ~ Temperature ~ (degree * C))
  ) +
  tema +
  theme(legend.position = "none")

dev.off()
