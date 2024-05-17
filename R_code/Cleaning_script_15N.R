# Cleaning 15N data
#
#------- ### Libraries ### -------
library(plotly)
library(tidyverse)
library(readxl)
#
#
#
#------- ### Load data ### -------
#
# Load IRMS data in one long list and combine
IRMS_path <- "raw_data/IRMS/"
IRMS_folder <- dir(IRMS_path)
IRMS_list <- list()
#
# Loop through each file
for (file in IRMS_folder){
  
  # Load data: all IRMS data from plants
  IRMS_data <- read_xlsx(paste(IRMS_path, file, sep = ""), sheet = "2. Final results", skip = 37, col_names = TRUE, na = "NA")
  
  # Add file id to new column
  IRMS_data$id <- str_extract(file, "[BR][E0123456789]+_\\d+")
  
  IRMS_data$Plate <- as.character(IRMS_data$Plate)
  
  # Name each file uniquely, based on filename. Add to list
  IRMS_list[[paste("IRMS", str_extract(file, "[BR][E0123456789]+_\\d+"), sep = "_")]] <- IRMS_data
  
  # Remove temp file
  rm(IRMS_data)
}
#
# Remove two samples with unknown weights
IRMS_list$IRMS_B1_240214 <- IRMS_list$IRMS_B1_240214 %>%
  filter(`Weight (mg)` != "?") %>%
  mutate(across(c(`Weight (mg)`, `ωN / %`), ~as.numeric(.x)))
#
# Remove one sample with too little material
IRMS_list$IRMS_B6_231127 <- IRMS_list$IRMS_B6_231127 %>%
  filter(`Weight (mg)` >= 0.1) %>%
  mutate(across(`ωN / %`, ~as.numeric(.x)))
#
# Combine into one file
IRMS_all <- do.call(bind_rows, IRMS_list)
#
#
#
# ------- # Extractions # -------
#
# Extraction data is all in the same format
extr_header <- c("Sample_nr", "Plate", "Well", "Sample_code", "SE_or_SEF", "Comments", "c1", "c2", "c3", "c4", 
                 "c5", "c6", "c7", "H2O", "Freeze_dry_ml", "Freeze_dry2", "Soil_FW_g", "Soil_DW_g", "SW_ml", "Sample_number", 
                 "Name", "Height_N_nA", "N15", "Height_C_nA", "C13", "Weight", "PEAnA_N", "PEA15N", "PEAnA_C", "PEA13C", 
                 "gnsnSTD_N_weight", "gnsnSTD_C_weight", "1nA_to_mgN_STD", "1nA_to_mgC", "Sample_N_mg", "Sample_C_mg", "Extr_N_mg", "Extr_C_mg", "Soil_N_IRMS", "Soil_C_IRMS", 
                 "%N_peach", "%C_peach", "C_N_ratio", "d15N_korr", "AP_NatAbu_15N", "AP_15N", "APE_N", "c8", "Soil_15N", "15N_in_N", 
                 "d13C_korr")
extr_type <- c("numeric", "text", "text", "text", "text", "text", "text", "text", "text", "text", 
               "text", "text", "text", "text", "text", "text", "text", "text", "text", "numeric", 
               "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
               "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
               "numeric","numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
               "numeric")
#
# Load extraction files
Extr_32 <- read_xlsx("raw_data/Extractions/EmilExtr2023_Tray32_FINAL.xlsx", sheet = "Emil2023", col_names = extr_header, skip = 2, col_types = extr_type)
Extr_33 <- read_xlsx("raw_data/Extractions/EmilExtr2023_Tray33_FINAL.xlsx", sheet = "Emil2023", col_names = extr_header, skip = 2, col_types = extr_type)
Extr_34 <- read_xlsx("raw_data/Extractions/EmilExtr2023_Tray34_FINAL.xlsx", sheet = "Emil2023", col_names = extr_header, skip = 2, col_types = extr_type)
Extr_35 <- read_xlsx("raw_data/Extractions/EmilExtr2023_Tray35_FINAL.xlsx", sheet = "Emil2023", col_names = extr_header, skip = 2, col_types = extr_type)
Extr_36 <- read_xlsx("raw_data/Extractions/EmilExtr2023_Tray36_FINAL.xlsx", sheet = "Emil2023", col_names = extr_header, skip = 2, col_types = extr_type)
Extr_37 <- read_xlsx("raw_data/Extractions/EmilExtr2023_Tray37_FINAL.xlsx", sheet = "Emil2023", col_names = extr_header, skip = 2, col_types = extr_type)
Extr_38 <- read_xlsx("raw_data/Extractions/EmilExtr2023_Tray38_FINAL.xlsx", sheet = "Emil2023", col_names = extr_header, skip = 2, col_types = extr_type)
Extr_39_40_41 <- read_xlsx("raw_data/Extractions/EmilExtr2023_Tray39_40_41_FINAL.xlsx", sheet = "Emil2023", col_names = extr_header, skip = 2, col_types = extr_type)
#
# Combine extraction datasets
extractions <- bind_rows(list(Extr_32, Extr_33, Extr_34, Extr_35, Extr_36, Extr_37, Extr_38, Extr_39_40_41), .id = "Raw")

#
# Remove unnecessary columns and rows
extractions.1 <- extractions %>%
  select(1:6, 23:37) %>%
  filter(!is.na(Sample_nr)) %>%
  mutate(Raw = case_when(Raw == "1" ~ "extr32",
                         Raw == "2" ~ "extr33",
                         Raw == "3" ~ "extr34",
                         Raw == "4" ~ "extr35",
                         Raw == "5" ~ "extr36",
                         Raw == "6" ~ "extr37",
                         Raw == "7" ~ "extr38",
                         Raw == "9" ~ "extr39_40_41"))
#
#
extractions.2 <- extractions.1 %>%
  mutate(Sample_code = case_when(Sample_code == "NA" & Plate == "33" & Well == "E3" ~ "Vit_6_2",
                                 Sample_code == "NA" & Plate == "33" & Well == "H4" ~ "Des_6_5",
                                 TRUE ~ Sample_code)) %>%
  mutate(Sample_code = str_replace_all(Sample_code, "-|\\.", "_")) %>%
  separate_wider_delim(Sample_code, delim = "_", names = c("Species", "MP", "Replicate"), too_few = "debug", too_many = "debug") %>%
  #
  # Standardize species names and ensure the right species is given
  mutate(Species = case_when(Species == "blank" ~ "Blank",
                             Species == "Cass" | Species == "CASS" | Species == "Cas" ~ "CAS",
                             Species == "Emp" ~ "EMP",
                             Species == "Loi" ~ "LOI",
                             Species == "Vit" ~ "VIT",
                             Species == "Myr" ~ "MYR",
                             Species == "Uli" ~ "ULI",
                             Species == "Sal" ~ "SAL",
                             Species == "Des" ~ "DES",
                             Species == "Jun" ~ "JUN",
                             Species == "Rub" ~ "RUB",
                             Species == "Cor" ~ "COR",
                             Species == "Soil" | Species == "soil" | Species == "SOIL" ~ "SOI",
                             Species == "SAL" & MP == 4 & Replicate == 5 ~ "SOI", # Special case of mix-up. SAL_4_5 => SOI_4_5, while SAL_4_6 => SAL_4_5
                             TRUE ~ Species)) %>%
  #
  # Ensure the rounds are correct
  # mutate(MP = case_when(Species == "LOI" & MP == 64 ~ "6", # No space between MP and replicate messed up split
  #                       TRUE ~ MP)) %>%
  # if_else(Species == "SAL" & MP == 6 & Replicate == "Ab", "4", MP)) %>%
  #
  # Ensure the replicates are correct, or at least that there are duplicates
  mutate(Replicate = case_when(Species == "SAL" & MP == 4 & Replicate == "Ab" ~ "6",
                               Species == "SOI" & MP == 3 & Replicate == "5a" ~ "5", # Or 5b?? The other 1-4 exist for CR
                               Species == "SOI" & MP == 3 & Replicate == "5b" ~ NA, # Or 5b?? The other 1-4 exist for CR
                               Species == "VIT" & MP == 3 & Replicate == "5a" ~ "5", # Or 5b?? The other 1-4 exist for CR
                               Species == "VIT" & MP == 3 & Replicate == "5b" ~ NA, # Or 2b?? The other 1-4 exist for CR
                               Species == "VIT" & MP == 4 & Replicate == "2?" ~ NA, # Already exist a 2 for AB
                               Species == "JUN" & MP == 4 & Replicate == "1a" ~ "1", # Or 1b?? Check powder against other JUN samples!
                               Species == "JUN" & MP == 4 & Replicate == "1b" ~ NA, # Or 1b?? Check powder against other JUN samples!
                               Species == "LOI" & MP == 6 & Replicate == "AB" ~ "4", # No space between MP and replicate messed up split
                               Species == "SAL" & MP == 3 & Replicate == "1" & weight_µg == 5165 ~ "2",
                               Organ == "FR(2)" ~ NA, # Remove extra (?) fine root sample from ULI_5_3
                               TRUE ~ Replicate)) %>%
  #
  # Standarize name of organ part
  # Control samples are a mix of belowground organs and should as such simply be called BG
  mutate(Organ = case_when(Organ == "veg" ~ "AB", # Species == "SAL" & MP == 4 & Replicate == 6 & 
                           Organ == "Ab" ~ "AB",
                           Species == "LOI" & MP == 6 & Replicate == 4 & is.na(Organ) ~ "AB", # No space between MP and replicate messed up split
                           Organ == "FR+CR" ~ "BG",
                           Organ == "FR+CR+LR" ~ "BG+",
                           TRUE ~ Organ)) %>%
  mutate(Replicate = case_when(Species == "SAL" & MP == 4 & Replicate == "6" ~ "5",
                               TRUE ~ Replicate)) %>%
  filter(!is.na(Replicate))


#
#
#
#------- ## Combine and clean ## -------
#
# Clean naming to standard
IRMS_all.1 <- IRMS_all %>%
  # Remove the blank lines
  filter(!is.na(Identifier)) %>%
  #
  # Unify weight in same unit (µg) and in the same column
  mutate(weight_µg = case_when(!is.na(`Weight (µg)`) & is.na(`Weight (mg)`) & is.na(Weight) ~ `Weight (µg)`,
                               is.na(`Weight (µg)`) & !is.na(`Weight (mg)`) & is.na(Weight) & `Weight (mg)` <= 100 ~ `Weight (mg)`*1000,
                               is.na(`Weight (µg)`) & !is.na(`Weight (mg)`) & is.na(Weight) & `Weight (mg)` >= 100 ~ `Weight (mg)`,
                               is.na(`Weight (µg)`) & is.na(`Weight (mg)`) & !is.na(Weight) ~ Weight)) %>%
  relocate(weight_µg, .before = `Sample type`) %>%
  select(!c(Plate, Well, `Sample type`, Analysis, Comments, `Other comments`, ...12, `ωC / %`, `d13C / ‰`, `FC / %`, ...16, `C/N ratio`, `Weight (µg)`, `Weight (mg)`, Weight)) %>%
  #
  # Separate ID into Species, MP, Replicate, and Organ
  mutate(Sample = str_replace_all(Sample, "-", "_")) %>%
  separate_wider_delim(Sample, delim = " ", names = c("Type", "Organ1"), too_few = "debug", too_many = "debug") %>%
  separate_wider_delim(Type, delim = "_", names = c("Species", "MP", "Replicate", "Organ"), too_few = "debug", too_many = "debug") %>%
  # 
  # Remove samples from winterecology 1
  filter(Species != "V",
         Species != "A") %>%
  mutate(Organ = if_else(is.na(Organ), Organ1, Organ)) %>%
  select(!c(Type_remainder, Organ1)) %>%
  #
  # Rename variables for easier use in R
  rename("Nconc_pc" = `ωN / %`,
         "d15N" = `d15N / ‰`,
         "Atom_pc" = `FN / %`)
#
# Go through cases to clean mistakes and unify naming scheme
IRMS_all.2 <- IRMS_all.1 %>%
  # Unknown species are unhelpful
  filter(Species != "Unknown") %>%
  #
  # Standardize species names and ensure the right species is given
  mutate(Species = case_when(Species == "Cass" | Species == "CASS" | Species == "Cas" ~ "CAS",
                             Species == "Emp" ~ "EMP",
                             Species == "Loi" ~ "LOI",
                             Species == "Vit" ~ "VIT",
                             Species == "Myr" ~ "MYR",
                             Species == "Uli" ~ "ULI",
                             Species == "Sal" ~ "SAL",
                             Species == "Des" ~ "DES",
                             Species == "Jun" ~ "JUN",
                             Species == "Rub" ~ "RUB",
                             Species == "Cor" ~ "COR",
                             Species == "Soil" | Species == "soil" | Species == "SOIL" ~ "SOI",
                             Species == "SAL" & MP == 4 & Replicate == 5 ~ "SOI", # Special case of mix-up. SAL_4_5 => SOI_4_5, while SAL_4_6 => SAL_4_5
                             TRUE ~ Species)) %>%
  #
  # Ensure the rounds are correct
  mutate(MP = case_when(Species == "SAL" & MP == 6 & Replicate == "Ab" ~ "4",
                        Species == "LOI" & MP == 64 ~ "6", # No space between MP and replicate messed up split
                        TRUE ~ MP)) %>%
          # if_else(Species == "SAL" & MP == 6 & Replicate == "Ab", "4", MP)) %>%
  #
  # Ensure the replicates are correct, or at least that there are duplicates
  mutate(Replicate = case_when(Species == "SAL" & MP == 4 & Replicate == "Ab" ~ "6",
                               Species == "SOI" & MP == 3 & Replicate == "5a" ~ "5", # Or 5b?? The other 1-4 exist for CR
                               Species == "SOI" & MP == 3 & Replicate == "5b" ~ NA, # Or 5b?? The other 1-4 exist for CR
                               Species == "VIT" & MP == 3 & Replicate == "5a" ~ "5", # Or 5b?? The other 1-4 exist for CR
                               Species == "VIT" & MP == 3 & Replicate == "5b" ~ NA, # Or 2b?? The other 1-4 exist for CR
                               Species == "VIT" & MP == 4 & Replicate == "2?" ~ NA, # Already exist a 2 for AB
                               Species == "JUN" & MP == 4 & Replicate == "1a" ~ "1", # Or 1b?? Check powder against other JUN samples!
                               Species == "JUN" & MP == 4 & Replicate == "1b" ~ NA, # Or 1b?? Check powder against other JUN samples!
                               Species == "LOI" & MP == 6 & Replicate == "AB" ~ "4", # No space between MP and replicate messed up split
                               Species == "SAL" & MP == 3 & Replicate == "1" & weight_µg == 5165 ~ "2",
                               Organ == "FR(2)" ~ NA, # Remove extra (?) fine root sample from ULI_5_3
                               TRUE ~ Replicate)) %>%
  #
  # Standarize name of organ part
  # Control samples are a mix of belowground organs and should as such simply be called BG
  mutate(Organ = case_when(Organ == "veg" ~ "AB", # Species == "SAL" & MP == 4 & Replicate == 6 & 
                           Organ == "Ab" ~ "AB",
                           Species == "LOI" & MP == 6 & Replicate == 4 & is.na(Organ) ~ "AB", # No space between MP and replicate messed up split
                           Organ == "FR+CR" ~ "BG",
                           Organ == "FR+CR+LR" ~ "BG+",
                           TRUE ~ Organ)) %>%
  mutate(Replicate = case_when(Species == "SAL" & MP == 4 & Replicate == "6" ~ "5",
                               TRUE ~ Replicate)) %>%
  filter(!is.na(Replicate))
#
# As the setup of controls are different from the rest of the samples, they are split
# Control values
IRMS_all.export.control <- IRMS_all.2 %>%
  filter(MP == "C") %>%
  select(c(Species, MP, Replicate, Organ, weight_µg, Nconc_pc, d15N, Atom_pc)) %>%
  complete(Species, MP, Replicate, Organ) %>%
  # There are 3 (three) cases of two species where the samples are not combined, but split in FR and CR (MYR_C_A) or there simply is only FR (DES_C_E) or LR is split from FR+CR (MYR_C_C)
  # keep only these as FR, CR, and LR
  filter(Organ == "BG" | Organ == "BG+" | Organ == "FR" & !is.na(weight_µg) | Organ == "CR" & !is.na(weight_µg) | Organ == "LR" & !is.na(weight_µg))
#
# Rest
IRMS_all.export.samples <- IRMS_all.2 %>%
  filter(MP != "C") %>%
  select(c(Species, MP, Replicate, Organ, weight_µg, Nconc_pc, d15N, Atom_pc)) %>%
  complete(Species, MP, Replicate, Organ)
#
# Combine
IRMS_all.export <- bind_rows(IRMS_all.export.samples, IRMS_all.export.control) %>%
  rename("species" = Species,
         "measuringPeriod" = MP,
         "replicate" = Replicate,
         "organ" = Organ,
         "sample_weight" = weight_µg,
         "nitrogen_content" = Nconc_pc,
         "delta_nitrogen_15" = d15N,
         "atom_nitrogen_15" = Atom_pc)
#
# Save as CSV
write_csv(IRMS_all.export, "export/GardenExperiment1_EA_IRMS.csv", na = "NA")









IRMS_all.2 %>%
  filter(MP != "C") %>%
  ggplot(aes(x = Organ)) + geom_bar() + facet_wrap(~Species + MP)

IRMS_all.2 %>%
  filter(MP == "C") %>%
  ggplot(aes(x = Organ)) + geom_bar() + facet_wrap(~Species + Replicate)







biomass <- read_xlsx("raw_data/GardenExperiment1_EA_DryWeights_202204-202308.xlsx", col_names = TRUE, na = "NA")

biomass.1 <- biomass %>%
  select(!Comments) %>%
  rename("MP" = Measurementperiod,
         "AB" = DWAbovegroundBiomass_g,
         "FR" = DWFineRoots_g,
         "CR" = DWCoarseRoots_g,
         "LR" = DWLargeRoots_g) %>%
  mutate(across(c(MP, Replicate), ~as.character(.x))) %>%
  mutate(Species = case_when(Species == "Cass" | Species == "CASS" | Species == "Cas" ~ "CAS",
                             Species == "Emp" ~ "EMP",
                             Species == "Loi" ~ "LOI",
                             Species == "Vit" ~ "VIT",
                             Species == "Myr" ~ "MYR",
                             Species == "Uli" ~ "ULI",
                             Species == "Sal" ~ "SAL",
                             Species == "Des" ~ "DES",
                             Species == "Jun" ~ "JUN",
                             Species == "Rub" ~ "RUB",
                             Species == "Cor" ~ "COR",
                             Species == "Soil" ~ "SOI",
                             TRUE ~ Species)) %>%
  pivot_longer(cols = 5:8, names_to = "Organ", values_to = "Biomass_g")

#
biomass.control <- read_xlsx("raw_data/Field and Lab sheets_Control harvest_EA.xlsx", sheet = 8, skip = 1, col_names = TRUE, na = "NA")

biomass.control.1 <- biomass.control %>%
  rename("Species" = SP,
         "Replicate" = Box,
         "FR" = `Fine roots`,
         "CR" = `Coarse roots`,
         "LR" = `Large roots`) %>%
  mutate(Species = case_when(Species == "Cass" | Species == "CASS" | Species == "Cas" ~ "CAS",
                             Species == "Emp" ~ "EMP",
                             Species == "Loi" ~ "LOI",
                             Species == "Vit" ~ "VIT",
                             Species == "Myr" ~ "MYR",
                             Species == "Uli" ~ "ULI",
                             Species == "Sal" ~ "SAL",
                             Species == "Des" ~ "DES",
                             Species == "Jun" ~ "JUN",
                             Species == "Rub" ~ "RUB",
                             Species == "Cor" ~ "COR",
                             Species == "Soil" ~ "SOI",
                             TRUE ~ Species)) %>%
  select(!c(Researcher, `Green moss (without bag)`, `Birch leaves`, `Brown moss + litter`, ...11, ...12)) %>%
  pivot_longer(cols = 4:6, names_to = "Organ", values_to = "Biomass_g")



# Samples where there is enough biomass, but no measurements
x <- left_join(IRMS_all.export, biomass.1, by = join_by(Species, MP, Replicate, Organ)) %>%
  filter(is.na(weight_µg) & !is.na(Biomass_g)) %>%
  filter(Biomass_g >= 0.5)


y <- x %>%
  select(1:4, Biomass_g)

xx <- left_join(IRMS_all.export.control, biomass.control.1, by = join_by(Species, MP, Replicate, Organ)) %>%
  filter(is.na(weight_µg) & !is.na(Biomass_g)) %>%
  filter(Biomass_g >= 0.5)

yy <- xx %>%
  select(1:4, Biomass_g)


yy %>%
  ggplot(aes(x = Organ, y = Replicate)) + geom_jitter() + facet_wrap(~Species)


#
#



write_csv2(y, "export/Missing_15N_2.csv", na = "NA")
write_csv2(yy, "export/Missing_15N_control_2.csv", na = "NA")

#
























#
Isotope15N.1 <- Isotope15N.0 %>%
  mutate(Sample = str_replace_all(Sample, "-", "_")) %>%
  separate_wider_delim(Sample, delim = " ", names = c("Type", "Organ1"), too_few = "debug", too_many = "debug") %>%
  separate_wider_delim(Type, delim = "_", names = c("Species", "MP", "Replicate", "Organ"), too_few = "debug", too_many = "debug") %>%
  filter(Species != "V",
         Species != "A") %>%
  mutate(Organ = if_else(is.na(Organ), Organ1, Organ)) %>%
  select(!c(Type_remainder, Organ1))
#
Isotope15N.2 <- Isotope15N.1 %>%
  filter(Species != "Unknown") %>%
  mutate(Species = case_when(Species == "Cass" | Species == "Cas" ~ "CAS",
                             Species == "Emp" ~ "EMP",
                             Species == "Loi" ~ "LOI",
                             Species == "Vit" ~ "VIT",
                             Species == "Myr" ~ "MYR",
                             Species == "Uli" ~ "ULI",
                             Species == "Sal" ~ "SAL",
                             Species == "Des" ~ "DES",
                             Species == "Jun" ~ "JUN",
                             Species == "Rub" ~ "RUB",
                             Species == "Cor" ~ "COR",
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
  mutate(Weight_µg = if_else(is.na(`Weight (mg)`), if_else(is.na(`Weight (µg)`), `Weight`, `Weight (µg)`), `Weight (mg)`))
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
  ggplot(aes(x = MP, y = n)) + geom_point() + facet_wrap(~Species) + labs(title = "Fine roots")
#
# Coarse Roots
Isotope15N.3 %>%
  mutate(across(c(MP, Replicate), ~as.numeric(.x))) %>%
  filter(Organ == "CR") %>%
  count(Species, MP) %>%
  ggplot(aes(x = MP, y = n)) + geom_point() + facet_wrap(~Species) + labs(title = "Coarse roots")
#
# Aboveground shoots
Isotope15N.3 %>%
  mutate(across(c(MP, Replicate), ~as.numeric(.x))) %>%
  filter(Organ == "AB") %>%
  count(Species, MP) %>%
  ggplot(aes(x = MP, y = n)) + geom_point() + facet_wrap(~Species) + labs(title = "Aboveground shoots")
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
