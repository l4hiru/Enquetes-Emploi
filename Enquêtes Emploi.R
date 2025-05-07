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
data_1994 <- read_dta("1994/empl94qi.dta")
data_2001 <- read_dta("2001/empl01qi.dta")
data_2006 <- read_dta("2006/indiv064.dta") # Last quarter of 2006 Employment Survey


#II) Variables --------------------------------------------------------------------

# 1974 Year

data_1974 <- data_1974 %>%
  mutate(
    Age = as.numeric(ad1),
    Study = as.factor(et),
    Rural = ifelse(s74 %in% c("0", "1"), 1, 0),
    Diploma = case_when(
      dip %in% c("", "00", "10") ~ "Low",
      dip %in% c("21", "22", "23", "30", "31", "32", "33") ~ "Medium",
      dip %in% c("40", "41", "42", "43", "44", "45", "46", "50", "51", "52", "53", "54", 
      "60", "61") ~ "High", # N.B à partir de DDIP de l'année 1988!
      dip %in% c("90") ~ NA_character_
    ),
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

freq(data_1974$Diploma)

freq(data_1974$redechi) # Weighting variable

# 1988 Year

data_1988 <- data_1988 %>%
  mutate(
    Rural = ifelse(tur2 %in% c("0", "1"), 1, 0), 
    Age = as.numeric(age), 
    Study = ifelse(et == "1", 1, 0),
    Diploma = case_when(
      dipl %in% c("10", "11", "30", "31", "32", "33", "40", "41", "42", "43") ~ "High",
      dipl %in% c("50", "51", "60") ~ "Medium",
      dipl %in% c("70", "71") ~ "Low",
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
      cstoti %in% c("71", "72", "73", "76", "81", "82") ~ "Inactive")
  )

freq(data_1988$Diploma)

summarytools::freq(data_1988$extri) # Weighting variable 
summarytools::freq(data_1988$dep)   # Departement variable

wtd.table(data_1988$Occupation, weights = data_1988$extri) # Weighting thanks to questionr package
prop.table(wtd.table(data_1988$Occupation, weights = data_1988$extri)) # Weighted %

# 1994 Year 

data_1994 <- data_1994 %>%
  mutate(
    Rural = ifelse(tur5 %in% c("1"), 1, 0), 
    Age = as.numeric(agd), 
    Study = ifelse(ddipl1 == "7", 1, 0),
    Diploma = case_when(
      ddipl1 %in% c("4", "5", "6") ~ "High",
      ddipl1 %in% c("2", "3") ~ "Medium",
      ddipl1 %in% c("1") ~ "Low",
    ),
    Diploma = factor(Diploma, levels = c("Low", "Medium", "High")),
    Active = ifelse(act == "1", 1, 0),
    Inactive = ifelse(act == "3", 1, 0),
    Unemployed = ifelse(act %in% c("2"), 1, 0),
    Occupation = case_when(
      dcstot %in% c("1") ~ "Farmer",
      dcstot %in% c("2") ~ "Craftmen",
      dcstot %in% c("3") ~ "Executive",
      dcstot %in% c("4") ~ "PI",
      dcstot %in% c("5") ~ "Employee",
      dcstot %in% c("6") ~ "Worker",
      dcstot %in% c("7", "8") ~ "Inactive")
  )

freq(data_1994$extri)


# 2001 Year 

data_2001 <- data_2001 %>%
  mutate(
    Rural = ifelse(tur5 %in% c("1"), 1, 0), 
    Age = as.numeric(agd), 
    Study = ifelse(ddipl1 == "7", 1, 0),
    Diploma = case_when(
      ddipl1 %in% c("4", "5", "6") ~ "High",
      ddipl1 %in% c("2", "3") ~ "Medium",
      ddipl1 %in% c("1") ~ "Low",
    ),
    Diploma = factor(Diploma, levels = c("Low", "Medium", "High")),
    Active = ifelse(act == "1", 1, 0),
    Inactive = ifelse(act == "3", 1, 0),
    Unemployed = ifelse(act %in% c("2"), 1, 0),
    Occupation = case_when(
      dcstot %in% c("1") ~ "Farmer",
      dcstot %in% c("2") ~ "Craftmen",
      dcstot %in% c("3") ~ "Executive",
      dcstot %in% c("4") ~ "PI",
      dcstot %in% c("5") ~ "Employee",
      dcstot %in% c("6") ~ "Worker",
      dcstot %in% c("7", "8") ~ "Inactive")
  )

freq(data_2001$extri)

# 2006 (last quarter)

data_2006 <- data_2006 %>%
  mutate(
    Rural = ifelse(TUR5 %in% c("1"), 1, 0), 
    Age = as.numeric(AG), 
    Study = ifelse(FORTER == "2", 1, 0),
    Diploma = case_when(
      DDIPL %in% c("1", "3", "4") ~ "High",
      DDIPL %in% c("5", "6") ~ "Medium",
      DDIPL %in% c("7") ~ "Low",
    ),
    Diploma = factor(Diploma, levels = c("Low", "Medium", "High")),
    Active = ifelse(ACTANC == "1", 1, 0),
    Inactive = ifelse(ACTANC == "3", 1, 0),
    Unemployed = ifelse(ACTANC %in% c("2"), 1, 0),
    Occupation = case_when(
      CSTOTR %in% c("1") ~ "Farmer",
      CSTOTR %in% c("2") ~ "Craftmen",
      CSTOTR %in% c("3") ~ "Executive",
      CSTOTR %in% c("4") ~ "PI",
      CSTOTR %in% c("5") ~ "Employee",
      CSTOTR %in% c("6") ~ "Worker",
      CSTOTR %in% c("7", "8") ~ "Inactive")
  )


#III) Departemental dataset --------------------------------------------------------------

# 1974 Year

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
    
    HighEducShare = sum(Diploma == "High", na.rm = TRUE) / 
                         sum(Study == 0 & Age >= 18 & Age <= 64),  # High-educated share among 18-64, active, and not studying
    
    RuralShare = sum(Rural == 1) / n()
  ) %>%
  ungroup()

# 1988 Year

dep_1988 <- data_1988 %>%
  group_by(dep) %>%
  summarise(
    UnemploymentRate = sum((Unemployed == 1) * extri) / 
                       sum(((Active == 1) | (Unemployed == 1)) * extri),

    InactiveShare = sum((Occupation == "Inactive") * extri) / 
                    sum((Age >= 18 & Age <= 64) * extri),
  
    YoungShare = sum((Age >= 18 & Age <= 30) * extri) / 
                 sum((Age >= 18) * extri), 

    ExecutiveShare = sum((Occupation == "Executive" & Active == 1) * extri) / sum((Active == 1) * extri),
    WorkerShare = sum((Occupation == "Worker" & Active == 1) * extri) / sum((Active == 1) * extri), 
    FarmerShare = sum((Occupation == "Farmer" & Active == 1) * extri) / sum((Active == 1) * extri),

    HighEducShare = sum((Diploma == "High") * extri, na.rm = TRUE) / 
      sum((Study == 0 & Age >= 18 & Age <= 64) * extri),  # High-educated share among 18-64, active, and not studying

    RuralShare = sum((Rural == 1) * extri) / sum(extri)
  ) %>%
  ungroup()

# 1994 Year

dep_1994 <- data_1994 %>%
  group_by(dep) %>%
  summarise(
    UnemploymentRate = 100 * sum((Unemployed == 1) * extri) / 
                             sum(((Active == 1) | (Unemployed == 1)) * extri),

    InactiveShare = 100 * sum((Occupation == "Inactive") * extri) / 
                          sum((Age >= 18 & Age <= 64) * extri),

    YoungShare = 100 * sum((Age >= 18 & Age <= 30) * extri) / 
                       sum((Age >= 18) * extri), 

    ExecutiveShare = 100 * sum((Occupation == "Executive") * extri) / 
                            sum((Active == 1) * extri),

    WorkerShare = 100 * sum((Occupation == "Worker") * extri) / 
                         sum((Active == 1) * extri),

    FarmerShare = 100 * sum((Occupation == "Farmer") * extri) / 
                         sum((Active == 1) * extri),

    HighEducShare = 100 * sum((Diploma == "High") * extri, na.rm = TRUE) / 
                           sum((Study == 0 & Age >= 18 & Age <= 64) * extri),

    RuralShare = 100 * sum((Rural == 1) * extri) / 
                       sum(extri)
  ) %>%
  ungroup()

# 2001 Year 

dep_2001 <- data_2001 %>%
  group_by(dep) %>%
  summarise(
    UnemploymentRate = 100 * sum((Unemployed == 1) * extri, na.rm = TRUE) / 
                             sum(((Active == 1) | (Unemployed == 1)) * extri, na.rm = TRUE),

    InactiveShare = 100 * sum((Occupation == "Inactive") * extri, na.rm = TRUE) / 
                          sum((Age >= 18 & Age <= 64) * extri, na.rm = TRUE),

    YoungShare = 100 * sum((Age >= 18 & Age <= 30) * extri, na.rm = TRUE) / 
                       sum((Age >= 18) * extri, na.rm = TRUE), 

    ExecutiveShare = 100 * sum((Occupation == "Executive") * extri, na.rm = TRUE) / 
                            sum((Active == 1) * extri, na.rm = TRUE),

    WorkerShare = 100 * sum((Occupation == "Worker") * extri, na.rm = TRUE) / 
                         sum((Active == 1) * extri, na.rm = TRUE),

    FarmerShare = 100 * sum((Occupation == "Farmer") * extri, na.rm = TRUE) / 
                         sum((Active == 1) * extri, na.rm = TRUE),

    HighEducShare = 100 * sum((Diploma == "High") * extri, na.rm = TRUE) / 
                           sum((Study == 0 & Age >= 18 & Age <= 64) * extri, na.rm = TRUE),

    RuralShare = 100 * sum((Rural == 1) * extri, na.rm = TRUE) / 
                       sum(extri, na.rm = TRUE)
  ) %>%
  ungroup()

# 2006 Year 

dep_2006 <- data_2006 %>%
  group_by(DEP) %>%
  summarise(
    UnemploymentRate = 100 * sum((Unemployed == 1) * EXTRI, na.rm = TRUE) / 
                             sum(((Active == 1) | (Unemployed == 1)) * EXTRI, na.rm = TRUE),

    InactiveShare = 100 * sum((Occupation == "Inactive") * EXTRI, na.rm = TRUE) / 
                          sum((Age >= 18 & Age <= 64) * EXTRI, na.rm = TRUE),

    YoungShare = 100 * sum((Age >= 18 & Age <= 30) * EXTRI, na.rm = TRUE) / 
                       sum((Age >= 18) * EXTRI, na.rm = TRUE), 

    ExecutiveShare = 100 * sum((Occupation == "Executive") * EXTRI, na.rm = TRUE) / 
                            sum((Active == 1) * EXTRI, na.rm = TRUE),

    WorkerShare = 100 * sum((Occupation == "Worker") * EXTRI, na.rm = TRUE) / 
                         sum((Active == 1) * EXTRI, na.rm = TRUE),

    FarmerShare = 100 * sum((Occupation == "Farmer") * EXTRI, na.rm = TRUE) / 
                         sum((Active == 1) * EXTRI, na.rm = TRUE),

    HighEducShare = 100 * sum((Diploma == "High") * EXTRI, na.rm = TRUE) / 
                           sum((Study == 0 & Age >= 18 & Age <= 64) * EXTRI, na.rm = TRUE),

    RuralShare = 100 * sum((Rural == 1) * EXTRI, na.rm = TRUE) / 
                       sum(EXTRI, na.rm = TRUE)
  ) %>%
  ungroup()

write_parquet(dep_1974, "1974/dep_1974.parquet")
write_parquet(dep_1988, "1988/dep_1988.parquet")
write_parquet(dep_1994, "1994/dep_1994.parquet")
write_parquet(dep_2001, "2001/dep_2001.parquet")
write_parquet(dep_2006, "2006/dep_2006.parquet")