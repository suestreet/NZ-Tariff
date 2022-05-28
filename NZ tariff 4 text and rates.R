######################################################################
######################################################################
## Join tariff item and CusMod rate
######################################################################
######################################################################

setwd("D:/Policy Working Area/Trade/Tariff/WTO submissions/HS2022 for WTO")
#setwd("/home/sue/Documents/HS2022 for WTO/")
library(tidyr)
library(dplyr)
library(stringr)
library(openxlsx)

# 2022 errors backed out here
error_lines <- c('30021900','38000000','74199925','84323009','84433926',
                 '84862039','84798900','84863025','85279210','85271910',
                 '85198145','87033330')


tariff <- readRDS("tariff_8.rds")

tariff <- tariff %>%
  select(line_number, Tariff_Code, Tariff_Description, Tariff_Code_short)

rate_text <- readRDS("WTD/rate.rds")

tariff <- tariff %>%
  left_join(rate_text, by=c('Tariff_Code_short' = 'HS8')) %>% 
  mutate(line_number = seq(1: dim(tariff)[1]))

saveRDS(tariff, "NZ_tariff_13_May_2022TEST.RDS")
write.xlsx(tariff, "NZ_tariff_13_May_2022TEST.xlsx", overwrite = TRUE)

