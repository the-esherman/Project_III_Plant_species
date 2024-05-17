# Garden Experiment 1
# Script author: Emil A.S. Andersen
#
# Main analysis of 15N data and statistics
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
Biomass <- Biomass %>%
  rename("species" = Species,
         "measuringPeriod" = Measurementperiod,
         "replicate" = Replicate,
         "AB" = DWAbovegroundBiomass_g,
         "CR" = DWCoarseRoots_g,
         "FR" = DWFineRoots_g,
         "LR" = DWtLargeRoots_g) %>%
  mutate(species = if_else(species == "SOIL", "SOI", species),
         across(c(measuringPeriod, replicate), ~ as.character(.x))) %>%
  filter(!is.na(species)) %>%
  select(!Comments) %>%
  pivot_longer(4:7, names_to = "organ", values_to = "biomass")
#
# Environmental data
# Air and soil temperature and soil moisture
SoilAirT <- read_csv("clean_data/GardenExperiment1_EA_SoilairtemperatureVWC_202109-202306.csv", col_names = TRUE)
#
#
# 15N data
IRMS <- read_csv("clean_data/GardenExperiment1_EA_IRMS.csv", col_names = TRUE)
# Separate control and labelled
# Control or natural abundance
IRMS_control <- IRMS %>%
  filter(measuringPeriod == "C")
#
# Labelled
IRMS_15N <- IRMS %>%
  filter(measuringPeriod != "C")


# Constants
#
# Extraction correction factor
K_EN = 0.4
# See https://climexhandbook.w.uib.no/2019/11/06/soil-microbial-biomass-c-n-and-p/ and UCPH bio lab protocol (where K_EN = 0.4)
#
#
#
#=======  ►   Functions     ◄ =======
# From http://www.cookbook-r.com/Manipulating_data/Summarizing_data/
## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- plyr::ddply(data, groupvars, .drop=.drop,
                       .fun = function(xx, col) {
                         c(N    = length2(xx[[col]], na.rm=na.rm),
                           mean = mean   (xx[[col]], na.rm=na.rm),
                           sd   = sd     (xx[[col]], na.rm=na.rm),
                           max  = max    (xx[[col]], na.rm=na.rm),
                           min  = min    (xx[[col]], na.rm=na.rm)
                         )
                       },
                       measurevar
  )
  # Rename the "mean" column    
  datac <- plyr::rename(datac, c("mean" = measurevar))
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  return(datac)
}
#
#
#
#=======  ♦   Main data     ♦ =======
#
# Label atomic data
# Added 15N; mg 15N pr pot
# This is from WinterEcology I, and matches almost the 15N load on the pots, but will need to be adjusted per pot per measuring period
# Added 15N; mg 15N pr patch
N_add <- 1.084
Label_15F <- 0.987
Label_atom_pc <- Label_15F*100
#
Label_atom_pc <- 0.987 # 98.7% double labelled 15N-NH4NO3
Atom_mass_14N_NH4NO3 <- 2*14.007+4*1.008+3*15.999 # The atomic mass of 14N NH4NO3
Atom_mass_15N_NH4NO3 <- 2*15+4*1.008+3*15.999 # The atomic mass of double 15N NH4NO3

#(2*15*Label_atom_pc)/(2*14.007*(1-Label_atom_pc)+2*15*Label_atom_pc+4*1.008+3*15.999)

Label_15N_frac <- (2*15*Label_atom_pc)/(Atom_mass_14N_NH4NO3*(1-Label_atom_pc)+Atom_mass_15N_NH4NO3*Label_atom_pc) # The atom mass of the 15N to the total label NH4NO3
Label_N_frac <- (2*15*Label_atom_pc+2*14*(1-Label_atom_pc))/(Atom_mass_14N_NH4NO3*(1-Label_atom_pc)+Atom_mass_15N_NH4NO3*Label_atom_pc)
# In the numerator: 2 15N per molecule, but only 98.7%
# In the denominator: The molecule's average mass, as 14N NH4NO3 is 1.3% and 15N NH4NO3 is 98.7%
#
#
# Calculate natural abundance from control samples
IRMS_control_sum <- IRMS_control %>%
  group_by(species) %>%
  summarise(#delta_nitrogen_15 = mean(delta_nitrogen_15, na.rm = TRUE), # Unnecessary, used to calculate average of atom%
            natAb_atom_nitrogen_15 = mean(atom_nitrogen_15, na.rm = TRUE)) #%>%
  #mutate(natAb_atom_pc = 100/(1+272/(1+delta_nitrogen_15/1000))) %>% # gives almost the same as simply averaging the atom%
#
# Combine biomass data with 15N data to calculate recovery
vegroot15N <- left_join(IRMS_15N, Biomass, by = join_by(species, measuringPeriod, replicate, organ)) %>%
  relocate(biomass, .after = organ) %>%
  # Add natural abundance data per species
  left_join(IRMS_control_sum, by = join_by(species)) %>%
  # Calculate recovery
  mutate(recovery = ((atom_nitrogen_15 - natAb_atom_nitrogen_15)/100 * nitrogen_content/100 * biomass)/(N_add/1000) * 100) %>%
  mutate(recovery = if_else(recovery < 0, 0, recovery))
#
# Calculate recovery for entire plant
vegroot15N_total_Plant <- vegroot15N %>%
  group_by(across(c("species", "measuringPeriod", "replicate"))) %>%
  summarise(plantRecovery = sum(recovery, na.rm = TRUE), .groups = "keep") %>%
  ungroup()



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