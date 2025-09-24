#0) Package 

library(arrow)
library(haven)        # For reading .dta files
library(dplyr)        # For data manipulation (mutate, case_when, group_by, etc.)
library(tidyverse)    # Includes ggplot2, dplyr, tidyr, etc.
library(janitor)      # For cleaning data, e.g., renaming variables
library(questionr)
library(summarytools) # For frequency tables (freq)
library(reshape2)     # For reshaping data (melt, cast)
library(stargazer)    # For regression tables (if needed)
library(plm)          # For panel data models (if needed)
library(questionr)

#I) Datasets 

data_2000 <- read_dta("2000/empl00qi.dta")

data_2005 <- read_dta("2005/indiv051.dta")
data_2005_2 <- read_dta("2005/indiv052.dta")
data_2005_3 <- read_dta("2005/indiv053.dta")
data_2005_4 <- read_dta("2005/indiv054.dta")

data_2010 <- read_dta("2010/indiv101.dta")
data_2010_2 <- read_dta("2010/indiv102.dta")
data_2010_3 <- read_dta("2010/indiv103.dta")
data_2010_4 <- read_dta("2010/indiv104.dta")



#II) Variables

# 2000

data_2000 <- data_2000 %>%
  mutate(
    Nationality = case_when(
      n == "01" ~ "Native",                     # Français de naissance
      n == "02" ~ "Naturalized",               # Français par acquisition
      n %in% c("11", "12", "13", "14", "15", 
               "21", "22", "23", "24", "25", "26", "27", "28", "29", 
               "31", "32", 
               "41", "42", "43", "44", "45", "46", "47", "48",
               "51", "52", "60") ~ "Immigrant", # Tous les codes étrangers
    )
  )

freq(data_2000$Nationality)


data_2000 <- data_2000 %>%
  mutate(
    Origin = case_when(
      n %in% c("21") ~ "Italy",
      n %in% c("31") ~ "Spain",
      n %in% c("32") ~ "Portugal",
      n %in% c("22", "23", "24", "25", "26", "27", "28", "29", 
               "41", "42", "43", "44", "46", "47", "48") ~ "Europe",
      n %in% c("11") ~ "Algeria",
      n %in% c("12") ~ "Tunisia",
      n %in% c("13") ~ "Morocco",
      n == "14" ~ "Africa",
      n %in%  c("15") ~ "Asia",
      n == "45" ~ "Turkey",
      n %in% c("51", "52") ~ "America"
    ))

freq(data_2000$Origin)

data_2000 <- data_2000 %>%
  mutate(Diploma = case_when(
    ddipl  == "" ~ NA_character_,  # < 15 yo, N.A
    ddipl  %in% c("7") ~ "Low",  # No diploma, CEP
    ddipl  %in% c("5", "6") ~ "Mid",  # BEPC, BEP, CAP, etc.
    ddipl  %in% c("1", "3", "4") ~ "High",  # BAC or more
  )) %>%
  mutate(Diploma = factor(Diploma, levels = c("Low", "Mid", "High")))

freq(data_2000$Diploma)

#B) Totals

immi_2000 <- data_2000 %>%
  filter(Nationality == "Immigrant") %>%
  drop_na(Origin, extri) %>%   
  group_by(Origin) %>%
  summarise(
    total_immi_2000 = sum(extri),
    .groups = "drop"
  )

shift_2000 <- data_2000 %>%
  filter(!is.na(Diploma)) %>%
  mutate(
    Origin = ifelse(Nationality %in% c("Native", "Naturalized"), "French", Origin)
  ) %>%
  group_by(Nationality, Origin, Diploma) %>%
  summarise(
    shift = sum(extri, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  drop_na()

write_parquet(shift_2000, "shift_2000_11nat_dipp.parquet")



# 2005 

process_quarter <- function(file_path, year = 2005) {
  
  data <- read_dta(file_path)
  
  # Recodage des nationalités
  data <- data %>%
    mutate(Nationality = recode(NFR,
                                "1" = "Native",
                                "2" = "Naturalized",
                                "3" = "Immigrant"))
  
  data <- data %>%
    mutate(Diploma = case_when(
      DDIPL == "" ~ NA_character_,
      DDIPL %in% c("7") ~ "Low",
      DDIPL %in% c("5", "6") ~ "Mid",
      DDIPL %in% c("1", "3", "4") ~ "High"
    )) %>%
    mutate(Diploma = factor(Diploma, levels = c("Low", "Mid", "High")))
    
  # Recodage des origines
  data <- data %>%
    mutate(
      Origin = case_when(
        NAT28 %in% c("21") ~ "Italy",
        NAT28 %in% c("31") ~ "Spain",
        NAT28 %in% c("32") ~ "Portugal",

        NAT28 %in% c("22", "23", "24", "25", "26", "27", "28", "29", 
                     "41", "42", "43", "44", "46", "47", "48") ~ "Europe",

        NAT28 %in% c("11") ~ "Algeria",
        NAT28 %in% c("12") ~ "Tunisia",
        NAT28 %in% c("13") ~ "Morocco",

        NAT28 == "14" ~ "Africa",

        NAT28 %in%  c("15", "60") ~ "Asia", # 60 : Other

        NAT28 %in% c("45") ~ "Turkey",

        NAT28 %in% c("51", "52") ~ "America"
      )
    )
  
  # Agrégation uniquement pour les immigrés
  shift_2005 <- data %>%
    filter(!is.na(Diploma)) %>%
    mutate(
      Origin = ifelse(Nationality %in% c("Native", "Naturalized"), "French", Origin)
    ) %>%
    group_by(Nationality, Origin, Diploma) %>%
    summarise(
      shift = sum(EXTRI, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    drop_na()
  }

files_2005 <- c("2005/indiv051.dta", 
                "2005/indiv052.dta", 
                "2005/indiv053.dta", 
                "2005/indiv054.dta")

# Appliquer la fonction à tous les fichiers
all_quarters <- lapply(files_2005, process_quarter)

# Fusionner tous les trimestres en une seule base
shift_2005_all <- bind_rows(all_quarters)

shift_2005_avg <- shift_2005_all %>%
  group_by(Nationality, Origin, Diploma) %>%
  summarise(shift = mean(shift, na.rm = TRUE), .groups = "drop")

sum(shift_2005_avg$shift)

#write_parquet(shift_2005_avg, "shift_2005_11nat_dipp.parquet")




# 2010 

process_quarter <- function(file_path, year = 2010) {
  
  data <- read_dta(file_path)
  
  # Recodage des nationalités
  data <- data %>%
    mutate(Nationality = recode(NFR,
                                "1" = "Native",
                                "2" = "Naturalized",
                                "3" = "Immigrant"))
                                
  data <- data %>%
    mutate(Diploma = case_when(
      DDIPL == "" ~ NA_character_,
      DDIPL %in% c("7") ~ "Low",
      DDIPL %in% c("5", "6") ~ "Mid",
      DDIPL %in% c("1", "3", "4") ~ "High"
    )) %>%
    mutate(Diploma = factor(Diploma, levels = c("Low", "Mid", "High")))
  
  # Recodage des origines
  data <- data %>%
    mutate(
      Origin = case_when(
        NAT28 %in% c("21") ~ "Italy",
        NAT28 %in% c("31") ~ "Spain",
        NAT28 %in% c("32") ~ "Portugal",
        
        NAT28 %in% c("22", "23", "24", "25", "26", "27", "28", "29", 
                     "41", "42", "43", "44", "46", "47", "48") ~ "Europe",
        
        NAT28 %in% c("11") ~ "Algeria",
        NAT28 %in% c("12") ~ "Tunisia",
        NAT28 %in% c("13") ~ "Morocco",
        
        NAT28 == "14" ~ "Africa",
        
        NAT28 %in%  c("15", "60") ~ "Asia", # 60 : Autres
        NAT28 %in% c("45") ~ "Turkey",
        
        NAT28 %in% c("51", "52") ~ "America"
      )
    )
  
  # Agrégation uniquement pour les immigrés
  shift_2010 <- data %>%
    filter(!is.na(Diploma)) %>%
    mutate(
      Origin = ifelse(Nationality %in% c("Native", "Naturalized"), "French", Origin)
    ) %>%
    group_by(Nationality, Origin, Diploma) %>%
    summarise(
      shift = sum(EXTRI, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    drop_na()
}

files_2010 <- c("2010/indiv101.dta", 
                "2010/indiv102.dta", 
                "2010/indiv103.dta", 
                "2010/indiv104.dta")

# Appliquer la fonction à tous les fichiers
all_quarters <- lapply(files_2010, process_quarter)

# Fusionner tous les trimestres
shift_2010_all <- bind_rows(all_quarters)

# Moyenne annuelle par origine
shift_2010_avg <- shift_2010_all %>%
  group_by(Nationality, Origin, Diploma) %>%
  summarise(shift = mean(shift, na.rm = TRUE), .groups = "drop")

write_parquet(shift_2010_avg, "shift_2010_11nat_dipp.parquet")


# 2015 

process_quarter <- function(file_path, year = 2015) {
  
  data <- read_dta(file_path)
  
  # Recodage des nationalités
  data <- data %>%
    mutate(Nationality = recode(nfrred,
                                "1" = "Native",
                                "2" = "Naturalized",
                                "3" = "Immigrant"))
  
    data <- data %>%
    mutate(Diploma = case_when(
      ddipl == "" ~ NA_character_,
      ddipl %in% c("7") ~ "Low",
      ddipl %in% c("5", "6") ~ "Mid",
      ddipl %in% c("1", "3", "4") ~ "High",
    )) %>%
    mutate(Diploma = factor(Diploma, levels = c("Low", "Mid", "High")))
  
  # Recodage des origines (NAT14, 14 postes)
  data <- data %>%
    mutate(
      Origin = case_when(
        nat14 == "11" ~ "Spain",
        nat14 == "12" ~ "Italy",
        nat14 == "13" ~ "Portugal",
        
        nat14 %in% c("14", "15") ~ "Europe",

        nat14 == "21" ~ "Algeria",
        nat14 == "22" ~ "Morocco",
        nat14 == "23" ~ "Tunisia",
        
        nat14 == "24" ~ "Africa",
        
        nat14 == "31" ~ "Turkey",

        nat14 %in% c("32", "51") ~ "Asia",
        
        nat14 == "41" ~ "America",
              )
    )
  
  # Agrégation uniquement pour les immigrés
  shift_2015 <- data %>%
    filter(!is.na(Diploma)) %>%
    mutate(
      Origin = ifelse(Nationality %in% c("Native", "Naturalized"), "French", Origin)
    ) %>%
    group_by(Nationality, Origin, Diploma) %>%
    summarise(
      shift = sum(extri, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    drop_na()
}

files_2015 <- c("2015/INDIV151.dta", 
                "2015/INDIV152.dta", 
                "2015/INDIV153.dta", 
                "2015/INDIV154.dta")

# Appliquer la fonction à tous les fichiers
all_quarters <- lapply(files_2015, process_quarter)

# Fusionner tous les trimestres
shift_2015_all <- bind_rows(all_quarters)

# Moyenne annuelle par origine
shift_2015_avg <- shift_2015_all %>%
  group_by(Nationality, Origin, Diploma) %>%
  summarise(shift = mean(shift, na.rm = TRUE), .groups = "drop") # One weird row for Europe/High but no nationality status


write_parquet(shift_2015_avg, "shift_2015_11nat_dipp.parquet")


# 2020 


process_quarter <- function(file_path, year = 2020) {
  
  data <- read_dta(file_path)
  
  # Recodage des nationalités
  data <- data %>%
    mutate(Nationality = recode(nfrred,
                                "1" = "Native",
                                "2" = "Naturalized",
                                "3" = "Immigrant"))
  
    data <- data %>%
    mutate(Diploma = case_when(
      ddipl == "" ~ NA_character_,
      ddipl %in% c("7") ~ "Low",
      ddipl %in% c("5", "6") ~ "Mid",
      ddipl %in% c("1", "3", "4") ~ "High",
    )) %>%
    mutate(Diploma = factor(Diploma, levels = c("Low", "Mid", "High")))
  
  # Recodage des origines (même nomenclature NAT14 qu’en 2015)
  data <- data %>%
    mutate(
      Origin = case_when(
        nat14 == "11" ~ "Spain",
        nat14 == "12" ~ "Italy",
        nat14 == "13" ~ "Portugal",
        
        nat14 %in% c("14", "15") ~ "Europe",

        nat14 == "21" ~ "Algeria",
        nat14 == "22" ~ "Morocco",
        nat14 == "23" ~ "Tunisia",
        
        nat14 == "24" ~ "Africa",
        
        nat14 == "31" ~ "Turkey",

        nat14 %in% c("32", "51") ~ "Asia",
        
        nat14 == "41" ~ "America",
              )
    )
  
  # Agrégation uniquement pour les immigrés
  shift_2020 <- data %>%
    filter(!is.na(Diploma)) %>%
    mutate(
      Origin = ifelse(Nationality %in% c("Native", "Naturalized"), "French", Origin)
    ) %>%
    group_by(Nationality, Origin, Diploma) %>%
    summarise(
      shift = sum(extri, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    drop_na()
}

files_2020 <- c("2020/INDIV201.dta", 
                "2020/INDIV202.dta", 
                "2020/INDIV203.dta", 
                "2020/INDIV204.dta")

# Appliquer la fonction à tous les fichiers
all_quarters <- lapply(files_2020, process_quarter)

# Fusionner tous les trimestres
shift_2020_all <- bind_rows(all_quarters)

# Moyenne annuelle par origine
shift_2020_avg <- shift_2020_all %>%
  group_by(Nationality, Origin, Diploma) %>%
  summarise(shift = mean(shift, na.rm = TRUE), .groups = "drop")

# Sauvegarde
write_parquet(shift_2020_avg, "shift_2020_11nat_dipp.parquet")









# III) Final datsets 

write_parquet(immi_2000, "shift_2000_11nat.parquet")
write_parquet(shift_immi2005_avg, "shift_2005_11nat.parquet")
write_parquet(shift_immi2010_avg, "shift_2010_11nat.parquet")
write_parquet(shift_immi2015_avg, "shift_2015_11nat.parquet")
write_parquet(shift_immi2020_avg, "shift_2020_11nat.parquet")