# Plot Distribution of UTI Timing
# Present Results
# Temperature-UTI Project
# Joan Casey & Sebastian T. Rowland
# Updated 11/01/2021

####***********************
#### Table of Contents ####
####***********************

# D: Description
# 0: Preparation
# 1: Organize Case Data
# 2: Create Plots

####********************
#### D: Description ####
####********************

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

####***************************
#### 1: Organize Case Data ####
####***************************

# 1a Create month and DoW variables
cases <- dta %>%
  mutate(
    MM = month(adate),
    DoW = wday(adate)
  )

# 1b Wrangle dates into nice format
cases <- cases %>%
  mutate(DoW_name = case_when(
    DoW == 1 ~ "Sun",
    DoW == 2 ~ "Mon",
    DoW == 3 ~ "Tues",
    DoW == 4 ~ "Weds",
    DoW == 5 ~ "Thurs",
    DoW == 6 ~ "Fri",
    DoW == 7 ~ "Sat"
  )) %>%
  mutate(MM_name = case_when(
    MM == 1 ~ "Jan",
    MM == 2 ~ "Feb",
    MM == 3 ~ "Mar",
    MM == 4 ~ "Apr",
    MM == 5 ~ "May",
    MM == 6 ~ "June",
    MM == 7 ~ "July",
    MM == 8 ~ "Aug",
    MM == 9 ~ "Sept",
    MM == 10 ~ "Oct",
    MM == 11 ~ "Nov",
    MM == 12 ~ "Dec"
  ))

# 1c Make date variables factors so they order properly in the plot
cases$DoW_name <- factor(cases$DoW_name, c("Sun", "Mon", "Tues", "Weds", "Thurs", "Fri", "Sat"))
cases$MM_name <- factor(cases$MM_name, c(
  "Jan", "Feb", "Mar", "Apr", "May", "June",
  "July", "Aug", "Sept", "Oct", "Nov", "Dec"
))

####*********************
#### 2: Create Plots ####
####*********************

# 2a Set the title for the y-axis
YAxisTitle <- "Proportion of Cases (%)"

# 2b Plot the distribution by month
TP.month <- cases %>%
  group_by(MM_name) %>%
  summarize(Count = sum(case_count_sex_f)) %>%
  mutate(Proportion = 100 * Count / sum(cases$case_count_sex_f)) %>%
  ggplot() +
  geom_bar(aes(MM_name, y = Proportion), stat = "identity") + # ,  color = 'lightblue') +
  labs(y = YAxisTitle, x = "Month") +
  coord_cartesian(
    ylim = c(0, 11), expand = TRUE,
    default = FALSE, clip = "off"
  ) +
  tema

# 2c Plot the distribution by day of the week
TP.day <- cases %>%
  group_by(DoW_name) %>%
  summarize(Count = sum(case_count_sex_f)) %>%
  mutate(Proportion = 100 * Count / sum(cases$case_count_sex_f)) %>%
  ggplot() +
  geom_bar(aes(DoW_name, y = Proportion), stat = "identity") + # ,  color = 'lightblue') +
  labs(y = YAxisTitle, x = "Day of Week") +
  tema

# 2d Print plot
png(here::here(
  outPath, "manuscript",
  paste0("eFig2_UTI_timingDistribution", ".png")
),
width = ww.fig * 1, height = hh.fig * 1.5, res = rr.fig * 1
)

print(cowplot::plot_grid(TP.month, TP.day,
  ncol = 1, align = "hv", axis = "tb",
  labels = c("A", "B"),
  label_x = 0.12, label_y = 0.95, label_size = 20
))

dev.off()
