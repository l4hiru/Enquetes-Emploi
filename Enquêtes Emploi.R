# Enquêtes Emploi (1974, etc.)

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
      cse1 %in% c("00", "10") ~ "Farmer",
      cse1 %in% c("20") ~ "Craftsmen",
      cse1 %in% c("41", "42", "43", "44") ~ "PI",
      cse1 %in% c("30", "32", "33", "34") ~ "Executive",
      cse1 %in% c("51", "53", "73") ~ "Employee",
      cse1 %in% c("60", "61", "63", "65", "66", "67", "68") ~ "Worker",
      cse1 %in% c("83", "90", "91", "92", "96", "97", "98", "99") ~ "Inactive",
      TRUE ~ NA_character_
    )
  )

freq(data_1974$redechi) # Weighting variable


data_1988 <- data_1988 %>%
  mutate(
    Rural = ifelse(tur2 %in% c("0", "1"), 1, 0), 
    Age = as.numeric(age), 
    Study = ifelse(et == "1", 1, 0),
    Diploma = case_when(
      dipl %in% c("10", "11", "30", "31", "32", "33", "40", "41", "42", "43") ~ "High",
      dipl %in% c("50", "51", "60") ~ "Medium",
      dipl %in% c("70", "71", "") ~ "Low",
    ),
    Diploma = factor(Diploma, levels = c("Low", "Medium", "High")),
    Active = ifelse(actbit == "1", 1, 0),
    Inactive = ifelse(actbit == "4", 1, 0),
    Unemployed = ifelse(actbit %in% c("2", "3"), 1, 0),
    Occupation = case_when(
      cstoti %in% c("10") ~ "Farmer",
      cstoti %in% c("21", "22", "23") ~ "Craftmen",
      cstoti %in% c("31", "32", "36") ~ "Executive",
      cstoti %in% c("41", "46", "47", "48") ~ "PI",
      cstoti %in% c("51", "54", "55", "56") ~ "Employee",
      cstoti %in% c("61", "66", "69") ~ "Worker",
      cstoti %in% c("71", "72", "73", "76") ~ "Pensioner",
      cstoti %in% c("81", "82") ~ "Inactive")
  )

summarytools::freq(data_1988$extri)

wtd.table(data_1988$Occupation, weights = data_1988$extri) # Weighting thanks to questionr package
prop.table(wtd.table(data_1988$Occupation, weights = data_1988$extri)) # Weighted %

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
    WorkersShare = sum(Occupation == "Worker" & Active == 1) / sum(Active == 1), 
    FarmersShare = sum(Occupation == "Farmer" & Active == 1) / sum(Active == 1),
    
    HighEducShare = sum(Diploma == "High") / 
                         sum(Study == 0 & Age >= 18 & Age <= 64),  # High-educated share among 18-64, active, and not studying
    
    RuralShare = sum(Rural == 1) / n()
  ) %>%
  ungroup()

write_parquet(dep_1974, "dep_1974.parquet")






dep_1988 <- data_1988 %>%
  group_by(d) %>%
  summarise(
    UnemploymentRate = sum((Unemployed == 1) * redechi, na.rm = TRUE) / 
                       sum(((Active == 1) | (Unemployed == 1)) * redechi, na.rm = TRUE),
    
    InactiveShare = sum((Occupation == "Inactive")) / 
                    sum((Age >= 18 & Age <= 64)),
    
    YoungShare = sum((Age >= 18 & Age <= 30)) / 
                 sum(Age >= 18),
    
    ExecutiveShare = sum(Occupation == "Executive" & Active == 1) / sum(Active == 1),
    WorkersShare = sum(Occupation == "Worker" & Active == 1) / sum(Active == 1), 
    FarmersShare = sum(Occupation == "Farmer" & Active == 1) / sum(Active == 1),
    
    HighEducShare = sum(Diploma == "High") / 
                         sum(Study == 0 & Age >= 18 & Age <= 64),  # High-educated share among 18-64, active, and not studying
    
    RuralShare = sum(Rural == 1) / n()
  ) %>%
  ungroup()