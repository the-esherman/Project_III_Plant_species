# Analysis
# By Emil A.S. Andersen
# 
#=======  ♣   Libraries     ♣ =======
library(plyr)
library(tidyverse)
library(car)
library(nlme)
library(gridExtra)
library(cowplot)
#
#
#
#=======  ♠   Load data     ♠ =======
#
# Biomass
Biomass <- read_csv("clean_data/GardenExperiment1_EA_DryWeights_202204-202308.csv", col_names = TRUE)
#
# 
SoilAirT <- read_csv("clean_data/GardenExperiment1_EA_SoilairtemperatureVWC_202109-202306.csv", col_names = TRUE)


#
#
#
#=======  ►   Functions     ◄ =======

#
#
#
#=======  ♦   Main data     ♦ =======

#
#
#
#=======  §§  Statistics    §§ =======
#-------  »   Contrasts     « -------

#
#
#
#-------  »   Q1            « -------

#
#
#
#-------  »   Q2            « -------

#
#
#
#=======  ♫♫  Graphs        ♫♫ =======
#-------  ♪   Environmental ♪ -------
# Soil temperature
airT_plot <- SoilAirT %>% 
  ggplot() +
  geom_hline(yintercept = 0, color = "#999999") +
  geom_line(aes(x = date, y = daily_mean_airtemp, lty = "Garden temperature"), na.rm = TRUE) + 
  #scale_y_continuous(breaks = c(-10, 0, 10, 20), minor_breaks = c(-15, -5, 5, 15)) +
  scale_x_date(date_breaks = "30 day", date_minor_breaks = "5 day") +
  #coord_cartesian(xlim = c(as.Date("2019-08-06"),as.Date("2020-09-16"))) +
  labs(x = NULL, y = "Air temperature (°C)") + # x = "Time of year",  , title = "Air temperature" 
  guides(lty = guide_legend(title = "Mean diel temperature"))+ #lty = guide_legend(title = "Mean diel temperature")) +
  theme_bw(base_size = 20) +
  theme(legend.position = "top", axis.text.x = element_blank(), axis.text.y = element_text(size = 15))
#
# Get legend
airT_legend <- get_legend(airT_plot)
airT_plot.2 <- airT_plot + theme_bw(base_size = 17) + theme(legend.position = "none", axis.text.x = element_blank(), axis.text.y = element_text(size = 15))
#
# Soil temperatures - all
soilT_plot <- SoilAirT %>% 
  ggplot() +
  geom_hline(yintercept = 0, color = "#999999") +
  geom_line(aes(x = date, y = daily_mean_soiltemp, lty = "Garden temperature"), na.rm = TRUE) + 
  #scale_y_continuous(breaks = c(-10, 0, 10, 20), minor_breaks = c(-15, -5, 5, 15)) +
  scale_x_date(date_breaks = "30 day", date_minor_breaks = "5 day") +
  #coord_cartesian(xlim = c(as.Date("2019-08-06"),as.Date("2020-09-16"))) +
  labs(x = NULL, y = "Soil temperature (°C)") + # x = "Time of year", , title = "Soil temperature"
  guides(lty = "none") +
  theme_bw(base_size = 17) +
  theme(legend.position = "top")
#
grid.arrange(airT_legend, airT_plot.2, soilT_plot, ncol = 1, widths = c(2.7), heights = c(0.5, 3, 3))

#
VWC_plot <- SoilAirT %>% 
  ggplot() +
  geom_hline(yintercept = 0, color = "#999999") +
  geom_line(aes(x = date, y = daily_mean_VWC, lty = "Garden VWC"), na.rm = TRUE) + 
  #scale_y_continuous(breaks = c(-10, 0, 10, 20), minor_breaks = c(-15, -5, 5, 15)) +
  scale_x_date(date_breaks = "30 day", date_minor_breaks = "5 day") +
  #coord_cartesian(xlim = c(as.Date("2019-08-06"),as.Date("2020-09-16"))) +
  labs(x = "Time of year", y = "Soil VWC") + # x = "Time of year", , title = "Soil temperature"
  guides(fill = guide_legend(title = "VWC")) +
  theme_bw(base_size = 20) +
  theme(legend.position = "bottom")


#
#
#
#-------  ♪   Biomass       ♪ -------

#
#
#
#-------  ♪   Recovery      ♪ -------

#
#
#
#=======  ■  { The End }    ■ =======