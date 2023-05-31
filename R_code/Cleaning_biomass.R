# Cleaning and collecting data
# Garden experiment I

#------- ### Libraries ### -------
library(tidyverse)
library(readxl)
library(writexl)

#------- ### Load data ### -------
LargeRoots <- read_excel("raw_data/Large roots.xlsx", skip = 1, col_names = TRUE, col_types = c("text", "text", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
CoarseRoots <- read_excel("raw_data/Coarse roots.xlsx", skip = 1, col_names = TRUE, col_types = c("text", "text", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text"))
FineRoots <- read_excel("raw_data/Fine roots.xlsx", skip = 1, col_names = TRUE, col_types = c("text", "text", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
Aboveground <- read_excel("raw_data/Aboveground Biomass.xlsx", skip = 1, col_names = TRUE, col_types = c("text", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text")) # add extra "numeric" when all data has been typed in

RespirationMP1 <- read_excel("raw_data/Respiration.xlsx", sheet = "MP1", skip = 1, col_names = FALSE)
RespirationMP2 <- read_excel("raw_data/Respiration.xlsx", sheet = "MP2", skip = 1, col_names = FALSE)
RespirationMP3 <- read_excel("raw_data/Respiration.xlsx", sheet = "MP3", skip = 1, col_names = FALSE)
RespirationMP4 <- read_excel("raw_data/Respiration.xlsx", sheet = "MP4", skip = 1, col_names = FALSE)
RespirationMP5 <- read_excel("raw_data/Respiration.xlsx", sheet = "MP5", skip = 1, col_names = FALSE)
RespirationMP6 <- read_excel("raw_data/Respiration.xlsx", sheet = "MP6", skip = 1, col_names = FALSE)


#------- ### Cleaning ### -------

# Remove unnecessary columns and extra headers. Pivot longer
# Large roots
LargeRoots <- LargeRoots %>%
  select(2, 4:11) %>%
  filter(!(is.na(SP))) %>%
  filter(SP != "SP") %>%
  rename("Species" = "SP",
         "1" ="MP1",
         "2" ="MP2",
         "3" ="MP3",
         "4" ="MP4",
         "5" ="MP5",
         "6" ="MP6"
         )
LargeRoots <- LargeRoots %>%
  pivot_longer(cols = 4:9, names_to = "MP", values_to = "DW_g") %>%
  mutate(Species = case_when(Species == "Emp" ~ "EMP",
                             Species == "Loi" ~ "LOI",
                             Species == "Cas" | Species == "Cass" ~ "CAS",
                             Species == "Vit" | Species == "Vac-VI" ~ "VIT",
                             Species == "Myr" | Species == "Vac-M" ~ "MYR",
                             Species == "Uli" | Species == "Vac-UL" ~ "ULI",
                             Species == "Sal" ~ "SAL",
                             Species == "Des" ~ "DES",
                             Species == "Jun" ~ "JUN",
                             Species == "Rub" ~ "RUB",
                             Species == "Cor" ~ "COR",
                             Species == "Soil" | Species == "SOIL" ~ "SOI",
                             TRUE ~ Species))

# Coarse roots
CoarseRoots <- CoarseRoots %>%
  select(2, 4:11) %>%
  filter(!(is.na(SP))) %>%
  filter(SP != "SP") %>%
  rename("Species" = "SP",
         "1" ="MP1",
         "2" ="MP2",
         "3" ="MP3",
         "4" ="MP4",
         "5" ="MP5",
         "6" ="MP6"
  )
CoarseRoots <- CoarseRoots %>%
  pivot_longer(cols = 4:9, names_to = "MP", values_to = "DW_g") %>%
  mutate(Species = case_when(Species == "Emp" ~ "EMP",
                             Species == "Loi" ~ "LOI",
                             Species == "Cas" | Species == "Cass" ~ "CAS",
                             Species == "Vit" | Species == "Vac-VI" ~ "VIT",
                             Species == "Myr" | Species == "Vac-M" ~ "MYR",
                             Species == "Uli" | Species == "Vac-UL" ~ "ULI",
                             Species == "Sal" ~ "SAL",
                             Species == "Des" ~ "DES",
                             Species == "Jun" ~ "JUN",
                             Species == "Rub" ~ "RUB",
                             Species == "Cor" ~ "COR",
                             Species == "Soil" | Species == "SOIL" ~ "SOI",
                             TRUE ~ Species))

# Fine roots
FineRoots <- FineRoots %>%
  select(2, 4:12) %>%
  filter(!(is.na(SP))) %>%
  filter(SP != "SP") %>%
  rename("five_extra" = "To add to fine roots for MP5") %>%
  rowwise() %>%
  mutate(MP5 = sum(MP5, five_extra, na.rm = TRUE)) %>%
  rename("Species" = "SP",
         "1" ="MP1",
         "2" ="MP2",
         "3" ="MP3",
         "4" ="MP4",
         "5" ="MP5",
         "6" ="MP6"
         )
FineRoots <- FineRoots %>%
  select(1:9) %>%
  pivot_longer(cols = 4:9, names_to = "MP", values_to = "DW_g") %>%
  mutate(Species = case_when(Species == "Emp" ~ "EMP",
                             Species == "Loi" ~ "LOI",
                             Species == "Cas" | Species == "Cass" ~ "CAS",
                             Species == "Vit" | Species == "Vac-VI" ~ "VIT",
                             Species == "Myr" | Species == "Vac-M" ~ "MYR",
                             Species == "Uli" | Species == "Vac-UL" ~ "ULI",
                             Species == "Sal" ~ "SAL",
                             Species == "Des" ~ "DES",
                             Species == "Jun" ~ "JUN",
                             Species == "Rub" ~ "RUB",
                             Species == "Cor" ~ "COR",
                             Species == "Soil" | Species == "SOIL" ~ "SOI",
                             TRUE ~ Species))

# Aboveground
Aboveground <- Aboveground %>%
  select(1,3:16) %>%
  slice(1:60) %>%
  rename("Species" = "SP") %>%
  mutate("1" = MP1 + ...6,
         "2" = MP2 + ...8,
         "3" = MP3 + ...10,
         "4" = MP4 + ...12,
         "5" = MP5 + ...14,
         "6" = MP6 + ...16)
Aboveground <- Aboveground %>%
  select(1:3, 16:21) %>%
  pivot_longer(cols = 4:9, names_to = "MP", values_to = "DW_g") %>%
  mutate(Species = case_when(Species == "Emp" ~ "EMP",
                             Species == "Loi" ~ "LOI",
                             Species == "Cas" | Species == "Cass" ~ "CAS",
                             Species == "Vit" | Species == "Vac-VI" ~ "VIT",
                             Species == "Myr" | Species == "Vac-M" ~ "MYR",
                             Species == "Uli" | Species == "Vac-UL" ~ "ULI",
                             Species == "Sal" ~ "SAL",
                             Species == "Des" ~ "DES",
                             Species == "Jun" ~ "JUN",
                             Species == "Rub" ~ "RUB",
                             Species == "Cor" ~ "COR",
                             Species == "Soil" | Species == "SOIL" ~ "SOI",
                             TRUE ~ Species),
         Type = case_when(Type == "AB veg" ~ "AB",
                          TRUE ~ Type))

#
# Respiration biomass (Fine roots and Above ground)
RespirationMP1 <- RespirationMP1 %>%
  select(1:12) %>%
  rename("Researcher" = ...1,
         "Species" = ...2,
         "MP" = ...3,
         "Rep" = ...4,
         "Date_FR" = ...5,
         "CO2_0h_FR_ppm" = ...6,
         "CO2_1h_FR_ppm" = ...7,
         "CO2_12h_FR_ppm" = ...8,
         "Date_AB" = ...9,
         "CO2_0h_AB_ppm" = ...10,
         "CO2_1h_AB_ppm" = ...11,
         "CO2_12h_AB_ppm" = ...12
         ) %>%
  select(2:12) %>%
  filter(!(is.na(Species))) %>%
  filter(Species != "SP") %>%
  mutate(across(c("CO2_0h_FR_ppm", "CO2_1h_FR_ppm", "CO2_12h_FR_ppm", "Date_AB", "CO2_0h_AB_ppm", "CO2_1h_AB_ppm", "CO2_12h_AB_ppm"), as.numeric)) %>%
  mutate(across("Date_AB", as.character))
#
RespirationMP2 <- RespirationMP2 %>%
  rename("Researcher" = ...1,
         "Species" = ...2,
         "MP" = ...3,
         "Rep" = ...4,
         "Date_FR" = ...5,
         "CO2_0h_FR_ppm" = ...6,
         "CO2_1h_FR_ppm" = ...7,
         "CO2_4h_FR_ppm" = ...8,
         "DW_FR_g" = ...9,
         "Date_AB" = ...10,
         "CO2_0h_AB_ppm" = ...11,
         "CO2_1h_AB_ppm" = ...12,
         "DW_AB_g" = ...13
         ) %>%
  select(2:13) %>%
  filter(!(is.na(Species))) %>%
  filter(Species != "SP") %>%
  mutate(CO2_0h_FR_ppm = na_if(CO2_0h_FR_ppm, "/"),
         CO2_1h_FR_ppm = na_if(CO2_1h_FR_ppm, "/"),
         CO2_4h_FR_ppm = na_if(CO2_4h_FR_ppm, "/"),
         DW_FR_g = na_if(DW_FR_g, "/"),
         Date_AB = na_if(Date_AB, "/"),
         CO2_0h_AB_ppm = na_if(CO2_0h_AB_ppm, "/"),
         CO2_1h_AB_ppm = na_if(CO2_1h_AB_ppm, "/"),
         DW_AB_g = na_if(DW_AB_g, "/")
         ) %>% # turn / into NA
  mutate(across(c("CO2_0h_FR_ppm", "CO2_1h_FR_ppm", "CO2_4h_FR_ppm", "DW_FR_g", "CO2_0h_AB_ppm", "CO2_1h_AB_ppm", "DW_AB_g"), as.numeric))
#
RespirationMP3 <- RespirationMP3 %>%
  rename("Researcher" = ...1,
         "Species" = ...2,
         "MP" = ...3,
         "Rep" = ...4,
         "Date_FR" = ...5,
         "CO2_0h_FR_ppm" = ...6,
         "CO2_1h_FR_ppm" = ...7,
         "CO2_4h_FR_ppm" = ...8,
         "DW_FR_g" = ...9,
         "Date_AB" = ...10,
         "CO2_0h_AB_ppm" = ...11,
         "CO2_1h_AB_ppm" = ...12,
         "DW_AB_g" = ...13
         ) %>%
  select(2:13) %>%
  filter(!(is.na(Species))) %>%
  filter(Species != "SP") %>%
  mutate(CO2_0h_FR_ppm = na_if(CO2_0h_FR_ppm, "/"),
         CO2_1h_FR_ppm = na_if(CO2_1h_FR_ppm, "/"),
         CO2_4h_FR_ppm = na_if(CO2_4h_FR_ppm, "/"),
         DW_FR_g = na_if(DW_FR_g, "/"),
         Date_AB = na_if(Date_AB, "/"),
         CO2_0h_AB_ppm = na_if(CO2_0h_AB_ppm, "/"),
         CO2_1h_AB_ppm = na_if(CO2_1h_AB_ppm, "/"),
         DW_AB_g = na_if(DW_AB_g, "/")
         ) %>% # turn / into NA
  mutate(across(c("CO2_0h_FR_ppm", "CO2_1h_FR_ppm", "CO2_4h_FR_ppm", "DW_FR_g", "CO2_0h_AB_ppm", "CO2_1h_AB_ppm", "DW_AB_g"), as.numeric))
#
RespirationMP4 <- RespirationMP4 %>%
  rename("Researcher" = ...1,
         "Species" = ...2,
         "MP" = ...3,
         "Rep" = ...4,
         "Date_FR" = ...5,
         "CO2_0h_FR_ppm" = ...6,
         "CO2_1h_FR_ppm" = ...7,
         "CO2_4h_FR_ppm" = ...8,
         "DW_FR_g" = ...9,
         "Date_AB" = ...10,
         "CO2_0h_AB_ppm" = ...11,
         "CO2_1h_AB_ppm" = ...12,
         "DW_AB_g" = ...13
  ) %>%
  select(2:13) %>%
  filter(!(is.na(Species))) %>%
  filter(Species != "SP") %>%
  slice(1:60) %>%
  mutate(CO2_0h_FR_ppm = na_if(CO2_0h_FR_ppm, "/"),
         CO2_1h_FR_ppm = na_if(CO2_1h_FR_ppm, "/"),
         CO2_4h_FR_ppm = na_if(CO2_4h_FR_ppm, "/"),
         DW_FR_g = na_if(DW_FR_g, "/"),
         Date_AB = na_if(Date_AB, "/"),
         CO2_0h_AB_ppm = na_if(CO2_0h_AB_ppm, "/"),
         CO2_1h_AB_ppm = na_if(CO2_1h_AB_ppm, "/"),
         DW_AB_g = na_if(DW_AB_g, "/")
         ) %>% # turn / into NA
  rowwise() %>%
  mutate(Species = if_else(Species == "Sal" && Rep == "5", "Soil", Species),
         Rep = if_else(Rep == "6", "5", Rep)) %>%
  mutate(across(c("CO2_0h_FR_ppm", "CO2_1h_FR_ppm", "CO2_4h_FR_ppm", "DW_FR_g", "CO2_0h_AB_ppm", "CO2_1h_AB_ppm", "DW_AB_g"), as.numeric))
#
RespirationMP5 <- RespirationMP5 %>%
  rename("Researcher" = ...1,
         "Species" = ...2,
         "MP" = ...3,
         "Rep" = ...4,
         "Date_FR" = ...5,
         "CO2_0h_FR_ppm" = ...6,
         "CO2_1h_FR_ppm" = ...7,
         "CO2_4h_FR_ppm" = ...8,
         "DW_FR_g" = ...9,
         "Date_AB" = ...10,
         "CO2_0h_AB_ppm" = ...11,
         "CO2_1h_AB_ppm" = ...12,
         "CO2_2h_AB_ppm" = ...13,
         "DW_AB_g" = ...14
  ) %>%
  select(2:14) %>%
  filter(!(is.na(Species))) %>%
  filter(Species != "SP") %>%
  mutate(CO2_0h_FR_ppm = na_if(CO2_0h_FR_ppm, "/"), CO2_0h_FR_ppm = na_if(CO2_0h_FR_ppm, "NA"),
         CO2_1h_FR_ppm = na_if(CO2_1h_FR_ppm, "/"), CO2_1h_FR_ppm = na_if(CO2_1h_FR_ppm, "NA"),
         CO2_4h_FR_ppm = na_if(CO2_4h_FR_ppm, "/"), CO2_4h_FR_ppm = na_if(CO2_4h_FR_ppm, "NA"),
         DW_FR_g = na_if(DW_FR_g, "/"), DW_FR_g = na_if(DW_FR_g, "NA"),
         Date_AB = na_if(Date_AB, "/"), Date_AB = na_if(Date_AB, "NA"),
         CO2_0h_AB_ppm = na_if(CO2_0h_AB_ppm, "/"), CO2_0h_AB_ppm = na_if(CO2_0h_AB_ppm, "NA"),
         CO2_1h_AB_ppm = na_if(CO2_1h_AB_ppm, "/"), CO2_1h_AB_ppm = na_if(CO2_1h_AB_ppm, "NA"),
         CO2_2h_AB_ppm = na_if(CO2_2h_AB_ppm, "/"), CO2_2h_AB_ppm = na_if(CO2_2h_AB_ppm, "NA"),
         DW_AB_g = na_if(DW_AB_g, "/"), DW_AB_g = na_if(DW_AB_g, "NA")
  ) %>% # turn "/" and "NA" into NA
  mutate(across(c("CO2_0h_FR_ppm", "CO2_1h_FR_ppm", "CO2_4h_FR_ppm", "DW_FR_g", "CO2_0h_AB_ppm", "CO2_1h_AB_ppm", "CO2_2h_AB_ppm", "DW_AB_g"), as.numeric))
#
RespirationMP6 <- RespirationMP6 %>%
  rename("Researcher" = ...1,
         "Species" = ...2,
         "MP" = ...3,
         "Rep" = ...4,
         "Date_FR" = ...5,
         "CO2_0h_FR_ppm" = ...6,
         "CO2_1h_FR_ppm" = ...7,
         "CO2_4h_FR_ppm" = ...8,
         "DW_FR_g" = ...9,
         "Date_AB" = ...10,
         "CO2_0h_AB_ppm" = ...11,
         "CO2_1h_AB_ppm" = ...12,
         "CO2_2h_AB_ppm" = ...13,
         "DW_AB_g_1" = ...14,
         "DW_AB_g_2" = ...15
         ) %>%
  select(2:15) %>%
  filter(!(is.na(Species))) %>%
  filter(Species != "SP") %>%
  mutate(CO2_0h_FR_ppm = na_if(CO2_0h_FR_ppm, "/"), CO2_0h_FR_ppm = na_if(CO2_0h_FR_ppm, "NA"),
         CO2_1h_FR_ppm = na_if(CO2_1h_FR_ppm, "/"), CO2_1h_FR_ppm = na_if(CO2_1h_FR_ppm, "NA"),
         CO2_4h_FR_ppm = na_if(CO2_4h_FR_ppm, "/"), CO2_4h_FR_ppm = na_if(CO2_4h_FR_ppm, "NA"),
         DW_FR_g = na_if(DW_FR_g, "/"), DW_FR_g = na_if(DW_FR_g, "NA"),
         Date_AB = na_if(Date_AB, "/"), Date_AB = na_if(Date_AB, "NA"),
         CO2_0h_AB_ppm = na_if(CO2_0h_AB_ppm, "/"), CO2_0h_AB_ppm = na_if(CO2_0h_AB_ppm, "NA"),
         CO2_1h_AB_ppm = na_if(CO2_1h_AB_ppm, "/"), CO2_1h_AB_ppm = na_if(CO2_1h_AB_ppm, "NA"),
         CO2_2h_AB_ppm = na_if(CO2_2h_AB_ppm, "/"), CO2_2h_AB_ppm = na_if(CO2_2h_AB_ppm, "NA"),
         DW_AB_g_1 = na_if(DW_AB_g_1, "/"), DW_AB_g_1 = na_if(DW_AB_g_1, "NA"),
         DW_AB_g_2 = na_if(DW_AB_g_2, "/"), DW_AB_g_2 = na_if(DW_AB_g_2, "NA")
  ) %>% # turn "/" and "NA" into NA
  mutate(across(c("CO2_0h_FR_ppm", "CO2_1h_FR_ppm", "CO2_4h_FR_ppm", "DW_FR_g", "CO2_0h_AB_ppm", "CO2_1h_AB_ppm", "CO2_2h_AB_ppm", "DW_AB_g_1", "DW_AB_g_2"), as.numeric)) %>%
  rowwise() %>%
  mutate(DW_AB_g = sum(DW_AB_g_1, DW_AB_g_2, na.rm = TRUE)) %>%
  mutate(DW_AB_g = na_if(DW_AB_g, 0))
#
Respiration <- bind_rows(RespirationMP6, RespirationMP5) %>%
  bind_rows(RespirationMP4) %>%
  bind_rows(RespirationMP3) %>%
  bind_rows(RespirationMP2) %>%
  bind_rows(RespirationMP1) %>%
  relocate(CO2_12h_FR_ppm, .after = CO2_4h_FR_ppm) %>%
  relocate(CO2_12h_AB_ppm, .after = CO2_2h_AB_ppm) %>%
  mutate(Species = case_when(Species == "Emp" ~ "EMP",
                             Species == "Loi" ~ "LOI",
                             Species == "Cas" | Species == "Cass" ~ "CAS",
                             Species == "Vit" | Species == "Vac-VI" ~ "VIT",
                             Species == "Myr" | Species == "Vac-M" ~ "MYR",
                             Species == "Uli" | Species == "Vac-UL" ~ "ULI",
                             Species == "Sal" ~ "SAL",
                             Species == "Des" ~ "DES",
                             Species == "Jun" ~ "JUN",
                             Species == "Rub" ~ "RUB",
                             Species == "Cor" ~ "COR",
                             Species == "Soil" | Species == "SOIL" ~ "SOI",
                             TRUE ~ Species))

#
FineRoots <- Respiration %>%
  select(1:3, DW_FR_g) %>%
  left_join(FineRoots, by = join_by(Species, MP, Rep)) %>%
  rowwise() %>%
  mutate(DW_g = sum(DW_FR_g, DW_g, na.rm = TRUE)) %>%
  select(1:3, Type, DW_g) %>%
  mutate(DW_g = na_if(DW_g, 0))

# Merge biomass
Biomass <- bind_rows(LargeRoots, CoarseRoots) %>%
  bind_rows(FineRoots) %>%
  bind_rows(Aboveground)

# Save as csv, comma delimited, with "." as decimal separator. For ";" as separator and "," as decimal use write_csv2
write_csv(Biomass, "clean_data/Biomass.csv")

#------- {The End} -------