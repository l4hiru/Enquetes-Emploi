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
library(questionr)

#I) Data --------------------------------------------------------------------------

data_1974 <- read_dta("1974/ee74.dta")
data_1988 <- read_dta("1988/ee88qi.dta")

#II) Variables --------------------------------------------------------------------

data_1974 <- data_1974 %>%
  mutate(
    Age = as.numeric(ad1),
    Study = as.factor(et),
    Rural = ifelse(s74 %in% c("0", "1"), 1, 0),
    Diploma = case_when(
      eg %in% c("3", "4", "5", "6", "7", "8") | ep %in% c("6", "7", "8", "9") ~ "High",
      eg == "2" | ep %in% c("1", "2", "3", "4", "5") ~ "Medium",
      TRUE ~ "Low"
    ),
    Diploma = factor(Diploma, levels = c("Low", "Medium", "High")),
    Active = ifelse(taec == "1", 1, 0),
    Unemployed = ifelse(taec == "2", 1, 0),
    Occupation = case_when(
      cse1 %in% c("00", "10") ~ "Farmers",
      cse1 %in% c("20") ~ "Craftsmen",
      cse1 %in% c("41", "42", "43", "44") ~ "PI",
      cse1 %in% c("30", "32", "33", "34") ~ "Executive",
      cse1 %in% c("51", "53", "73") ~ "Employees",
      cse1 %in% c("60", "61", "63", "65", "66", "67", "68") ~ "Workers",
      cse1 %in% c("83", "90", "91", "92", "96", "97", "98", "99") ~ "Inactive",
      TRUE ~ NA_character_
    )
  )

freq(data_1974$redechi) # Weighting variable

#III) Departemental dataset --------------------------------------------------------------

dep_1974 <- data_1974 %>%
  group_by(d) %>%
  summarise(
    UnemploymentRate = sum((Unemployed == 1) * redechi, na.rm = TRUE) / 
                       sum(((Active == 1) | (Unemployed == 1)) * redechi, na.rm = TRUE),
    
    InactiveShare = sum((Occupation == "Inactive")) / 
                    sum((Age >= 18 & Age <= 64)),
    
    YoungShare = sum((Age >= 18 & Age <= 30)) / 
                 sum(Age >= 18),
    
    ExecutiveShare = sum(Occupation == "Executive" & Active == 1) / sum(Active == 1),
    WorkersShare = sum(Occupation == "Workers" & Active == 1) / sum(Active == 1), 
    FarmersShare = sum(Occupation == "Farmers" & Active == 1) / sum(Active == 1),
    
    HighEducShare = sum(Diploma == "High") / 
                         sum(Study == 0 & Age >= 18 & Age <= 64),  # High-educated share among 18-64, active, and not studying
    
    RuralShare = sum(Rural == 1) / n()
  ) %>%
  ungroup()

write_parquet(dep_1974, "dep_1974.parquet")