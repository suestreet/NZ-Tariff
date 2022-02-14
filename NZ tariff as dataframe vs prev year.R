######################################################################
######################################################################
## NZ WTO Tariff 2020 2021 comparison
## Purpose: identify issues with extraction
######################################################################
######################################################################

setwd("D:/Policy Working Area/Trade/Tariff/WTO submissions/HS2022 for WTO")
library(tidyr)
library(dplyr)
library(stringr)
library(openxlsx)
source("constructFunctions.R")

tariff2021 <- readWorkbook("../Data/Tariff_processed_Jan_2022.xlsx")
tariff2020 <- readWorkbook("../Data/NZ Submission to the WTO 2021.xlsx", sheet = 2)

tariff2021 <- tariff2021 %>%
  mutate(between = Tariff_Level >= 2 & Tariff_Level < 999)

