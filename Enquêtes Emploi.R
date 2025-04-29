# Enquêtes Emploi (1974, etc.)

#O) Packages 

library(arrow)
library(haven)        # For reading .dta files
library(dplyr)        # For data manipulation (mutate, case_when, group_by, etc.)
library(tidyverse)    # Includes ggplot2, dplyr, tidyr, etc.
library(janitor)      # For cleaning data, e.g., renaming variables
library(summarytools) # For frequency tables (freq)
library(reshape2)     # For reshaping data (melt, cast)
library(stargazer)    # For regression tables (if needed)
library(plm)          # For panel data models (if needed)

#I) Data --------------------------------------------------------------------------

data_1974 <- read_dta("1974/ee74.dta")

data_1974 <- data_1974 %>%
  filter(ad1 > 18) # Only individuals with 18 yo or more (potential voters)

#II) Variables --------------------------------------------------------------------

freq(data_1974$s74) # Rural (0 + 1)

data_1974 <- data_1974 %>%
  mutate(
    Rural = ifelse(s74 %in% c("0", "1"), 1, 0))

freq(data_1974$Rural)

freq(data_1974$ad1) # Young (18 to 30)

data_1974 <- data_1974 %>%
  mutate(
    Young = if_else(ad1 %in% sprintf("%02d", 18:30), 1, 0)
  )
freq(data_1974$Young)

freq(data_1974$eg) # General Diploma
freq(data_1974$ep) # Profesionnal Diploma

data_1974 <- data_1974 %>%
  mutate(
    Diploma = case_when(
      # High: baccalauréat ou supérieur (général) OU BTS/DUT/université/etc. (pro)
      eg %in% c("3", "4", "5", "6", "7", "8") | ep %in% c("6", "7", "8", "9") ~ "High",
      
      # Medium: BEPC (général) ou CAP/BEP/etc. (pro)
      eg == "2" | ep %in% c("1", "2", "3", "4", "5") ~ "Medium",
      
      # Low: aucun diplôme ou non déclaré ou inférieur au BEPC
      TRUE ~ "Low"
    ),
    Diploma = factor(Diploma, levels = c("Low", "Medium", "High"))
  )

freq(data_1974$Diploma) 

freq(data_1974$taec) # 1 : Active and # 2 : Unemployed

data_1974 <- data_1974 %>%
  mutate(
    Active = ifelse(taec == "1", 1, 0),
    Unemployed = ifelse(taec == "2", 1, 0)
  )

freq(data_1974$Active)
freq(data_1974$Unemployed)

freq(data_1974$cse1) # Occupation

data_1974 <- data_1974 %>%
  mutate(
    Occupation = case_when(
      cse1 %in% c("00", "10") ~ "Farmers", # + 10 : Workers in agriculture
      cse1 %in% c("20") ~ "Craftsmen",
      cse1 %in% c("41", "42", "43", "44") ~ "PI",
      cse1 %in% c("30", "32", "33", "34") ~ "Executive",
      cse1 %in% c("51", "53", "73") ~ "Employees", # + personnes en service
      cse1 %in% c("60", "61", "63", "65", "66", "67", "68") ~ "Workers",
      cse1 %in% c("83", "90", "91", "92", "96", "97", "98", "99") ~ "Inactive", # 83: Others categories
      TRUE ~ NA_character_
    )
  )

freq(data_1974$Occupation)

freq(data_1974$d) # Metropolitans Departments

freq(data_1974$redechi) # Weighting variable ?


weighted_occupation <- data_1974 %>%
  group_by(Occupation) %>%
  summarise(weighted_n = sum(redechi, na.rm = TRUE)) %>%
  mutate(weighted_prop = weighted_n / sum(weighted_n))

weighted_occupation