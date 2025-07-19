# Shift-share (EE, post-2000)

#O) Packages 

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

### --- SHIFT-SHARE IV (SHIFT COMPUTATION) --- ###

#I) 2000 Year 

#A) Variables

data_2000 <- read_dta("2000/empl00qi.dta")

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


freq(data_2000$ddipl)

data_2000 <- data_2000 %>%
  mutate(Diploma = case_when(
    ddipl  == "" ~ NA_character_,  # < 15 yo, N.A
    ddipl  %in% c("7") ~ "Low",  # No diploma, CEP
    ddipl  %in% c("5", "6") ~ "Mid",  # BEPC, BEP, CAP, etc.
    ddipl  %in% c("1", "3", "4") ~ "High",  # BAC or more
  )) %>%
  mutate(Diploma = factor(Diploma, levels = c("Low", "Mid", "High")))

freq(data_2000$Diploma)

data_2000 <- data_2000 %>%
  mutate(
    Origin = case_when(
      n %in% c("21", "31", "32") ~ "South_Europe",
      n %in% c("22", "23", "24", "25", "26", "27", "28", "29", 
               "41", "42", "43", "44", "46", "47", "48") ~ "Europe",
      n %in% c("11", "12", "13") ~ "Maghreb",
      n == "14" ~ "Africa",
      n %in%  c("15", "45") ~ "Asia",
      n %in% c("51", "52", "60") ~ "Other"
    ))

freq(data_2000$Origin)

#B) Totals

immi_2000 <- data_2000 %>%
  filter(Nationality == "Immigrant") %>%
  group_by(Origin, Diploma) %>%
  summarise(
    total_imm_2000 = sum(extri, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(
    Origin_group = Origin
  )

sum(immi_2000$total_imm_2000)


native_2000 <- data_2000 %>%
  filter(Nationality == "Native") %>%
  group_by(Diploma) %>%
  summarise(
    total_native_2000 = sum(extri, na.rm = TRUE),
    .groups = "drop"
  )

naturalized_2000 <- data_2000 %>%
  filter(Nationality == "Naturalized") %>%
  group_by(Diploma) %>%
  summarise(
    total_naturalized_2000 = sum(extri, na.rm = TRUE),
    .groups = "drop"
  )


shift_2000 <- bind_rows(
  immi_2000 %>%
    mutate(
      Nationality = "Immigrant",
      Year = as.factor(2000),
      Count = total_imm_2000
    ) %>%
    select(Nationality, Origin_group, Diploma, Count, Year),

  native_2000 %>%
    mutate(
      Nationality = "Native",
      Origin_group = "French",
      Year = as.factor(2000),
      Count = total_native_2000
    ) %>%
    select(Nationality, Origin_group, Diploma, Count, Year),

  naturalized_2000 %>%
    mutate(
      Nationality = "Naturalized",
      Origin_group = "French",
      Year = as.factor(2000),
      Count = total_naturalized_2000
    ) %>%
    select(Nationality, Origin_group, Diploma, Count, Year)
)

#write_parquet(shift_2000, "shift_2000.parquet")

#II) 2005 Year

#A) Variables

data_2005 <- read_dta("2005/indiv051.dta")

data_2005_2 <- read_dta("2005/indiv052.dta")
data_2005_3 <- read_dta("2005/indiv053.dta")
data_2005_4 <- read_dta("2005/indiv054.dta")

freq(data_2005$NFR)

data_2005 <- data_2005 %>%
  mutate(Nationality = recode(NFR,
         "1" = "Native",
         "2" = "Naturalized",
         "3" = "Immigrant"))

freq(data_2005$Nationality)

freq(data_2005$DDIPL)

data_2005 <- data_2005 %>%
  mutate(Diploma = case_when(
    DDIPL  == "" ~ NA_character_,  # < 15 yo, N.A
    DDIPL  %in% c("7") ~ "Low",  # No diploma, CEP
    DDIPL  %in% c("5", "6") ~ "Mid",  # BEPC, BEP, CAP, etc.
    DDIPL  %in% c("1", "3", "4") ~ "High",  # BAC or more
  )) %>%
  mutate(Diploma = factor(Diploma, levels = c("Low", "Mid", "High")))

freq(data_2005$Diploma)

freq(data_2005$NAT28)

data_2005 <- data_2005 %>%
  mutate(
    Origin = case_when(
      NAT28 %in% c("21", "31", "32") ~ "South_Europe",
      NAT28 %in% c("22", "23", "24", "25", "26", "27", "28", "29", 
               "41", "42", "43", "44", "46", "47", "48") ~ "Europe",
      NAT28 %in% c("11", "12", "13") ~ "Maghreb",
      NAT28 == "14" ~ "Africa",
      NAT28 %in%  c("15", "45") ~ "Asia",
      NAT28 %in% c("51", "52", "60") ~ "Other"
    ))

freq(data_2005$Origin)

#B) Totals

immi_2005 <- data_2005 %>%
  filter(Nationality == "Immigrant") %>%
  group_by(Origin, Diploma) %>%
  summarise(
    total_imm_2005 = sum(EXTRI, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(
    Origin_group = Origin
  )

sum(immi_2005$total_imm_2005)

native_2005 <- data_2005 %>%
  filter(Nationality == "Native") %>%
  group_by(Diploma) %>%
  summarise(
    total_native_2005 = sum(EXTRI, na.rm = TRUE),
    .groups = "drop"
  )

naturalized_2005 <- data_2005 %>%
  filter(Nationality == "Naturalized") %>%
  group_by(Diploma) %>%
  summarise(
    total_naturalized_2005 = sum(EXTRI, na.rm = TRUE),
    .groups = "drop"
  )


shift_2005 <- bind_rows(
  immi_2005 %>%
    mutate(
      Nationality = "Immigrant",
      Year = as.factor(2005),
      Count = total_imm_2005
    ) %>%
    select(Nationality, Origin_group, Diploma, Count, Year),

  native_2005 %>%
    mutate(
      Nationality = "Native",
      Origin_group = "French",
      Year = as.factor(2005),
      Count = total_native_2005
    ) %>%
    select(Nationality, Origin_group, Diploma, Count, Year),

  naturalized_2005 %>%
    mutate(
      Nationality = "Naturalized",
      Origin_group = "French",
      Year = as.factor(2005),
      Count = total_naturalized_2005
    ) %>%
    select(Nationality, Origin_group, Diploma, Count, Year)
)







process_quarter <- function(file_path, year = 2005) {
  
  data <- read_dta(file_path)
  
  # Recodage des nationalités
  data <- data %>%
    mutate(Nationality = recode(NFR,
                                "1" = "Native",
                                "2" = "Naturalized",
                                "3" = "Immigrant"))
  
  # Recodage des diplômes
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
        NAT28 %in% c("21", "31", "32") ~ "South_Europe",
        NAT28 %in% c("22", "23", "24", "25", "26", "27", "28", "29", 
                     "41", "42", "43", "44", "46", "47", "48") ~ "Europe",
        NAT28 %in% c("11", "12", "13") ~ "Maghreb",
        NAT28 == "14" ~ "Africa",
        NAT28 %in%  c("15", "45") ~ "Asia",
        NAT28 %in% c("51", "52", "60") ~ "Other"
      )
    )
  
  # Agrégation par catégorie
  immi <- data %>%
    filter(Nationality == "Immigrant") %>%
    group_by(Origin, Diploma) %>%
    summarise(
      Count = sum(EXTRI, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      Nationality = "Immigrant",
      Origin_group = Origin
    )
  
  native <- data %>%
    filter(Nationality == "Native") %>%
    group_by(Diploma) %>%
    summarise(
      Count = sum(EXTRI, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      Nationality = "Native",
      Origin_group = "French"
    )
  
  naturalized <- data %>%
    filter(Nationality == "Naturalized") %>%
    group_by(Diploma) %>%
    summarise(
      Count = sum(EXTRI, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      Nationality = "Naturalized",
      Origin_group = "French"
    )
  
  bind_rows(immi, native, naturalized) %>%
    mutate(Year = as.factor(year)) %>%
    select(Nationality, Origin_group, Diploma, Count, Year)
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
  group_by(Nationality, Origin_group, Diploma) %>%
  summarise(Avg_Annual_Count = mean(Count, na.rm = TRUE), .groups = "drop") %>%
  mutate(Year = as.factor(2005))

write_parquet(shift_2005_avg, "shift_2005.parquet")


#III) 2010 Year 

data_2010 <- read_dta("2010/indiv101.dta")

data_2010_2 <- read_dta("2010/indiv102.dta")
data_2010_3 <- read_dta("2010/indiv103.dta")
data_2010_4 <- read_dta("2010/indiv104.dta")

freq(data_2010$NAT28)

process_quarter <- function(file_path, year = 2010) {
  
  data <- read_dta(file_path)
  
  # Recodage des nationalités
  data <- data %>%
    mutate(Nationality = recode(NFR,
                                "1" = "Native",
                                "2" = "Naturalized",
                                "3" = "Immigrant"))
  
  # Recodage des diplômes
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
        NAT28 %in% c("21", "31", "32") ~ "South_Europe",
        NAT28 %in% c("22", "23", "24", "25", "26", "27", "28", "29", 
                     "41", "42", "43", "44", "46", "47", "48") ~ "Europe",
        NAT28 %in% c("11", "12", "13") ~ "Maghreb",
        NAT28 == "14" ~ "Africa",
        NAT28 %in%  c("15", "45") ~ "Asia",
        NAT28 %in% c("51", "52", "60", "61") ~ "Other"
      )
    )
  
  # Agrégation par catégorie
  immi <- data %>%
    filter(Nationality == "Immigrant") %>%
    group_by(Origin, Diploma) %>%
    summarise(
      Count = sum(EXTRI, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      Nationality = "Immigrant",
      Origin_group = Origin
    )
  
  native <- data %>%
    filter(Nationality == "Native") %>%
    group_by(Diploma) %>%
    summarise(
      Count = sum(EXTRI, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      Nationality = "Native",
      Origin_group = "French"
    )
  
  naturalized <- data %>%
    filter(Nationality == "Naturalized") %>%
    group_by(Diploma) %>%
    summarise(
      Count = sum(EXTRI, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      Nationality = "Naturalized",
      Origin_group = "French"
    )
  
  bind_rows(immi, native, naturalized) %>%
    mutate(Year = as.factor(year)) %>%
    select(Nationality, Origin_group, Diploma, Count, Year)
}

files_2010 <- c("2010/indiv101.dta", 
                "2010/indiv102.dta", 
                "2010/indiv103.dta", 
                "2010/indiv104.dta")

# Appliquer la fonction à tous les fichiers
all_quarters <- lapply(files_2010, process_quarter)

# Fusionner tous les trimestres en une seule base
shift_2010_all <- bind_rows(all_quarters)


shift_2010_avg <- shift_2010_all %>%
  group_by(Nationality, Origin_group, Diploma) %>%
  summarise(Avg_Annual_Count = mean(Count, na.rm = TRUE), .groups = "drop") %>%
  mutate(Year = as.factor(2010))

write_parquet(shift_2010_avg, "shift_2010.parquet")





