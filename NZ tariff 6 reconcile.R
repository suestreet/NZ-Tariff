setwd("D:/Policy Working Area/Trade/Tariff/WTO submissions/HS2022 for WTO")

library(tidyr)
library(dplyr)
library(openxlsx)

t22 <- readRDS("NZ_tariff_12_May_2022.RDS")
t21 <- readWorkbook("Tariff_Rates_May2021_v1.xlsx")

t22 <- t22 %>% select(Tariff_Code, Tariff_Description, NML)
t21 <- t21 %>% select(HS_Code, Description, NML)

same <- t21 %>% semi_join(t22, by = c('HS_Code' = 'Tariff_Code')) 
old_gone <- t21 %>% anti_join(t22, by = c('HS_Code' = 'Tariff_Code'))
new_lines <- t22 %>% anti_join(t21, by = c('Tariff_Code' = 'HS_Code'))

old_gone <- old_gone %>% 
  filter(nchar(HS_Code)>9)

new_lines <- new_lines %>% 
  filter(nchar(Tariff_Code)>9)

correlation<-read.csv("WTD/Correlation Table_Customs.txt", sep='\t')
correlation <- correlation %>% 
  mutate(t21_code = substr(t21_TARIFF, 1,10),
         t22_code = substr(t22_TARIFF, 1,10)) %>% 
  select(-t21_TARIFF, -t22_TARIFF) %>% 
  unique() %>% # remove duplications at 8 digit level
  filter(!t21_code==t22_code) # and where only the stats key has changed

old_gone_not_in_correlation <- old_gone %>%
  anti_join(correlation, by = c('HS_Code' = 't21_code'))

new_not_in_correlation  <-  new_lines %>% 
  anti_join(correlation, by = c('Tariff_Code' = 't22_code'))



