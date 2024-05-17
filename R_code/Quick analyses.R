# Garden Experiment 1
# Script author: Emil A.S. Andersen
#
# Quick analysis of data
#
#------- ### Libraries ### -------
library(tidyverse)
library(readxl)
library(gridExtra)
library(viridis)
library(ggpubr)
library(rstatix)
library(car)
library(pastecs)
library(nlme)
library(multcomp)
library(WRS)




# Quick test
proj3 <- read_xlsx("raw_data/proj3_raw_v0.2.xlsx", sheet = "IRMS", skip = 1, col_names = TRUE)
# ωN, d15N, FN (atom%)

# Quick plot

proj3 <- proj3 %>%
  mutate(across(c("Species","MP", "Replicate", "Type"), as.character)) %>%
  mutate(across(c("ωN", "d15N", "FN"), as.numeric))

# 
proj3 %>%
  filter(Species == "LOI") %>%
  group_by(across(c("Species", "MP"))) %>%
  summarise(avgFN = mean(FN, na.rm = TRUE), se = sd(FN)/sqrt(length(FN)), .groups = "keep") %>%
  ggplot() + 
  geom_errorbar(aes(x = MP, y = avgFN, ymin=avgFN-se, ymax=avgFN+se), position=position_dodge(.9)) +
  geom_col(aes(MP, avgFN),color = "black") +
  facet_wrap( ~ Species, ncol = 2, scales = "free") + 
  labs(x = "Measuring period (MP)", y = expression("Atom% N"), title = expression("Plant Atom% N")) +
  theme_classic()

#
# N concentration
proj3 %>%
  filter(Species == "CAS") %>%
  group_by(across(c("Species", "MP"))) %>%
  summarise(avg = mean(ωN, na.rm = TRUE), se = sd(ωN)/sqrt(length(ωN)), .groups = "keep") %>%
  ggplot() + 
  geom_errorbar(aes(x = MP, y = avg, ymin=avg-se, ymax=avg+se), position=position_dodge(.9)) +
  geom_col(aes(MP, avg),color = "black") +
  facet_wrap( ~ Species, ncol = 2, scales = "free") + 
  labs(x = "Measuring period (MP)", y = expression("N"), title = expression("Plant N concentration")) +
  theme_classic()
#
#
# N conc
proj3 %>%
  filter(Type == "FR") %>%
  group_by(across(c("Species", "MP"))) %>%
  summarise(avg = mean(ωN, na.rm = TRUE), se = sd(ωN)/sqrt(length(ωN)), .groups = "keep") %>%
  ggplot() + 
  geom_errorbar(aes(x = MP, y = avg, ymin=avg-se, ymax=avg+se), position=position_dodge(.9)) +
  geom_col(aes(MP, avg),color = "black") +
  facet_wrap( ~ Species, ncol = 4, scales = "free") + 
  labs(x = "Measuring period (MP)", 
       y = expression("[N] % (g "*g^-1*" DW)"), 
       title = expression("Plant N concentration")) +
  theme_classic()
#
#
# d15N
proj3 %>%
  group_by(across(c("Species", "MP"))) %>%
  summarise(avg = mean(d15N, na.rm = TRUE), se = sd(d15N)/sqrt(length(d15N)), .groups = "keep") %>%
  ggplot() + 
  geom_errorbar(aes(x = MP, y = avg, ymin=avg-se, ymax=avg+se), position=position_dodge(.9)) +
  geom_col(aes(MP, avg),color = "black") +
  facet_wrap( ~ Species, ncol = 4, scales = "free") + 
  labs(x = "Measuring period (MP)", y = expression(delta^15*"N"), title = expression("Plant "*delta^15*"N concentration")) +
  theme_classic()
#
proj3 %>%
  dplyr::filter(Type == "AB") %>%
  #group_by(across(c("Species", "MP"))) %>%
  #summarise(avg = mean(d15N, na.rm = TRUE), se = sd(d15N)/sqrt(length(d15N)), .groups = "keep") %>%
  ggplot() + 
  #geom_errorbar(aes(x = MP, y = avg, ymin=avg-se, ymax=avg+se), position=position_dodge(.9)) +
  geom_boxplot(aes(MP, d15N),color = "black") +
  facet_wrap( ~ Species, ncol = 4, scales = "free") + 
  labs(x = "Measuring period (MP)", y = expression(delta^15*"N"), title = expression("Plant "*delta^15*"N")) +
  theme_classic(base_size = 20) +
  theme(panel.spacing = unit(1, "lines"))#,axis.text.x=element_text(angle=60, hjust=1))
