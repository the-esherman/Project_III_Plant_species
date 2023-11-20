# Cleaning 15N data
#
#------- ### Libraries ### -------
library(tidyverse)
library(readxl)
library(plotly)
#
#
#
#------- ### Load data ### -------
#
B1_230303 <- read_xlsx("raw_data/23123_EA-IRMS_EmilA_B1_230303_report.xlsx", sheet = "2. Final results", skip = 37, col_names = TRUE, na = "NA")
B2_230308 <- read_xlsx("raw_data/23123_EA-IRMS_EmilA_B2_230308_report.xlsx", sheet = "2. Final results", skip = 37, col_names = TRUE, na = "NA")
B3_230310 <- read_xlsx("raw_data/23123_EA-IRMS_EmilA_B3_230310_report.xlsx", sheet = "2. Final results", skip = 37, col_names = TRUE, na = "NA")
B1_230317 <- read_xlsx("raw_data/23150_EA-IRMS_EmilA_B1_230317_report.xlsx", sheet = "2. Final results", skip = 37, col_names = TRUE, na = "NA")
B2_230320 <- read_xlsx("raw_data/23150_EA-IRMS_EmilA_B2_230320_report2.xlsx", sheet = "2. Final results", skip = 37, col_names = TRUE, na = "NA")
B1_221110 <- read_xlsx("raw_data/22304_EA-IRMS_EmilA_Niki_B1_221110_report.xlsx", sheet = "2. Final results", skip = 37, col_names = TRUE, na = "NA")
B2_221111 <- read_xlsx("raw_data/22304_EA-IRMS_EmilA_Niki_B2_221111_report.xlsx", sheet = "2. Final results", skip = 37, col_names = TRUE, na = "NA")
B3_221114 <- read_xlsx("raw_data/22304_EA-IRMS_EmilA_Niki_B3_221114_report.xlsx", sheet = "2. Final results", skip = 37, col_names = TRUE, na = "NA")
B4_221115 <- read_xlsx("raw_data/22304_EA-IRMS_EmilA_Niki_B4_221115_report.xlsx", sheet = "2. Final results", skip = 37, col_names = TRUE, na = "NA")
B5_221117 <- read_xlsx("raw_data/22304_EA-IRMS_EmilA_Niki_B5_221117_report.xlsx", sheet = "2. Final results", skip = 37, col_names = TRUE, na = "NA")
B6_221121 <- read_xlsx("raw_data/22304_EA-IRMS_EmilA_Niki_B6_221121_report.xlsx", sheet = "2. Final results", skip = 37, col_names = TRUE, na = "NA")
B7_221123 <- read_xlsx("raw_data/22304_EA-IRMS_EmilA_Niki_B7_221123_report.xlsx", sheet = "2. Final results", skip = 37, col_names = TRUE, na = "NA")
B8_221124 <- read_xlsx("raw_data/22304_EA-IRMS_EmilA_Niki_B8_221124_report.xlsx", sheet = "2. Final results", skip = 37, col_names = TRUE, na = "NA")
B9_221129 <- read_xlsx("raw_data/22304_EA-IRMS_EmilA_Niki_B9_221129_report.xlsx", sheet = "2. Final results", skip = 37, col_names = TRUE, na = "NA")
RE1_230822 <- read_xlsx("raw_data/23225_EA-IRMS_RE1_230822_report.xlsx", sheet = "2. Final results", skip = 37, col_names = TRUE, na = "NA")
#
#
#
#------- ## Combine and clean ## -------
#
Isotope15N.0 <- B1_230303 %>%
  bind_rows(B2_230308) %>%
  bind_rows(B3_230310) %>%
  bind_rows(B1_230317) %>%
  bind_rows(B2_230320) %>%
  bind_rows(B1_221110) %>%
  bind_rows(B2_221111) %>%
  bind_rows(B3_221114) %>%
  bind_rows(B4_221115) %>%
  bind_rows(B5_221117) %>%
  bind_rows(B6_221121) %>%
  bind_rows(B7_221123) %>%
  bind_rows(B8_221124) %>%
  bind_rows(B9_221129) %>%
  mutate(across(Plate, ~as.character(.x))) %>%
  bind_rows(RE1_230822) %>%
  filter(!is.na(Identifier))
#
Isotope15N.1 <- Isotope15N.0 %>%
  separate_wider_delim(Sample, delim = " ", names = c("Type", "Organ1"), too_few = "debug", too_many = "debug") %>%
  separate_wider_delim(Type, delim = "_", names = c("Species", "MP", "Replicate", "Organ"), too_few = "debug", too_many = "debug") %>%
  filter(Species != "V",
         Species != "A") %>%
  mutate(Organ = if_else(is.na(Organ), Organ1, Organ)) %>%
  select(!c(Type_remainder, Organ1))
#
Isotope15N.2 <- Isotope15N.1 %>%
  filter(Species != "Unknown") %>%
  mutate(Species = case_when(Species == "Cass" ~ "CAS",
                             Species == "Emp" ~ "EMP",
                             Species == "Myr" ~ "MYR",
                             Species == "Sal" ~ "SAL",
                             Species == "Soil" | Species == "soil" | Species == "SOIL" ~ "SOI",
                             Species == "SAL" & MP == 4 & Replicate == 5 ~ "SOI", # Special case of mix-up. SAL_4_5 => SOI_4_5, while SAL_4_6 => SAL_4_5
                             TRUE ~ Species)) %>%
  mutate(MP = if_else(Species == "SAL" & MP == 6 & Replicate == "Ab", "4", MP)) %>%
  mutate(Replicate = case_when(Species == "SAL" & MP == 4 & Replicate == "Ab" ~ "6",
                               Species == "SOI" & MP == 3 & Replicate == "5a" ~ "5", # Or 5b?? The other 1-4 exist for CR
                               Species == "SOI" & MP == 3 & Replicate == "5b" ~ NA, # Or 5b?? The other 1-4 exist for CR
                               Species == "VIT" & MP == 3 & Replicate == "5a" ~ "5", # Or 5b?? The other 1-4 exist for CR
                               Species == "VIT" & MP == 3 & Replicate == "5b" ~ NA, # Or 2b?? The other 1-4 exist for CR
                               Species == "VIT" & MP == 4 & Replicate == "2?" ~ NA, # Already exist a 2 for AB
                               Species == "JUN" & MP == 4 & Replicate == "1a" ~ "1", # Or 1b?? Check powder against other JUN samples!
                               Species == "JUN" & MP == 4 & Replicate == "1b" ~ NA, # Or 1b?? Check powder against other JUN samples!
                               TRUE ~ Replicate)) %>%
  mutate(Organ = case_when(Organ == "veg" ~ "AB", # Species == "SAL" & MP == 4 & Replicate == 6 & 
                           Organ == "Ab" ~ "AB",
                           TRUE ~ Organ)) %>%
  mutate(Replicate = case_when(Species == "SAL" & MP == 4 & Replicate == "6" ~ "5",
                               TRUE ~ Replicate)) %>%
  filter(!is.na(Replicate)) %>%
  mutate(Weight_µg = if_else(is.na(`Weight (mg)`), `Weight (µg)`, `Weight (mg)`))
# Other cases:
# VIT_1_3 CR or SOI_1_1 CR?
#
Isotope15N.3 <- Isotope15N.2 %>%
  select(Species, MP, Replicate, Organ, Weight_µg, `ωN / %`, `d15N / ‰`, `FN / %`) %>%
  rename("Nconc_pc" = `ωN / %`,
         "d15N" = `d15N / ‰`,
         "Atom_pc" = `FN / %`)
#
write_csv(Isotope15N.3, "clean_data/Isotope.csv", na = "NA")



#
Isotope15N.3 %>%
  ggplot(aes(x = MP, y = d15N, color = Organ)) + geom_point() + facet_wrap(~Species)


















# • Outlier check ----
#
# Fine Roots
Isotope15N.3 %>%
  mutate(across(c(MP, Replicate), ~as.numeric(.x))) %>%
  filter(Organ == "FR") %>%
  count(Species, MP) %>%
  ggplot(aes(x = MP, y = n)) + geom_point() + facet_wrap(~Species)
#
# Coarse Roots
Isotope15N.3 %>%
  mutate(across(c(MP, Replicate), ~as.numeric(.x))) %>%
  filter(Organ == "CR") %>%
  count(Species, MP) %>%
  ggplot(aes(x = MP, y = n)) + geom_point() + facet_wrap(~Species)
#
# Aboveground shoots
Isotope15N.3 %>%
  mutate(across(c(MP, Replicate), ~as.numeric(.x))) %>%
  filter(Organ == "AB") %>%
  count(Species, MP) %>%
  ggplot(aes(x = MP, y = n)) + geom_point() + facet_wrap(~Species)
#
# Cleveland dot plot, all d15N values
dotchart(Isotope15N.3$d15N, 
         main="Cleveland plot - d15N", xlab = "Observed values", 
         pch = 19, color = hcl.colors(12), 
         labels = Isotope15N.3$MP, 
         groups = Isotope15N.3$Replicate,
         gpch = 12, gcolor = 1)
#
# d15N per species
Isotope15N.d15N_test <- Isotope15N.3 %>%
  select(1:4, d15N) %>%
  filter(Species != "SAL" | MP != 3 | Replicate != "1" | Organ != "AB" | d15N <= 200) %>%
  filter(Species != "COR" | MP != 1 | Replicate != "5" | Organ != "LR" | d15N >= 100) %>%
  complete(Species, MP, Replicate, Organ) %>%
  pivot_wider(names_from = Organ, values_from = d15N)
#
plot_ly(Isotope15N.d15N_test, x = ~AB, y = ~Species, name = "Aboveground", type = 'scatter', mode = "markers", marker = list(color = "#009E73")) %>% 
  add_trace(x = ~FR, y = ~Species, name = "Fine roots",type = 'scatter', mode = "markers", marker = list(color = "#E69F00")) %>% 
  add_trace(x = ~CR, y = ~Species, name = "Coarse roots",type = 'scatter', mode = "markers", marker = list(color = "#CC79A7")) %>% 
  add_trace(x = ~LR, y = ~Species, name = "Large roots",type = 'scatter', mode = "markers", marker = list(color = "#0072B2")) %>% 
  layout(title = "Species d15N", xaxis = list(title = "d15N"), margin = list(l = 100))
#
# d15N per Measuring period
plot_ly(Isotope15N.d15N_test, y = ~AB, x = ~MP, name = "Aboveground", type = 'scatter', mode = "markers", marker = list(color = "#009E73")) %>% 
  add_trace(y = ~FR, x = ~MP, name = "Fine roots",type = 'scatter', mode = "markers", marker = list(color = "#E69F00")) %>% 
  add_trace(y = ~CR, x = ~MP, name = "Coarse roots",type = 'scatter', mode = "markers", marker = list(color = "#CC79A7")) %>% 
  add_trace(y = ~LR, x = ~MP, name = "Large roots",type = 'scatter', mode = "markers", marker = list(color = "#0072B2")) %>% 
  layout(title = "MP d15N", yaxis = list(title = "d15N"), margin = list(l = 100))
#
