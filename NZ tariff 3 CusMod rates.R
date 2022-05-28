
#------------------------------
#
# Tariff current for analysis period
#
#------------------------------
  
library(dplyr)
library(tidyr)
library(openxlsx)
library(lubridate)
library(stringr)

options(scipen=999) # Suppress scientific notation

setwd("D:/Policy Working Area/Trade/Tariff/WTO submissions/HS2022 for WTO")
#setwd("/home/sue/Documents/HS2022 for WTO/")

# Select details current within the 2022 year

start_period <- as.Date('2022-01-01')
end_period <- as.Date('2022-12-31')

tariff.detail <- read.csv('../../../Common data/CusMod tariff/Tariff_Details.csv',sep='~', as.is=T, colClasses='character', header=T)
tariff.detail$Tic.Start.Date <- as.Date(strptime(tariff.detail$Tic.Start.Date,'%b%d%Y%I:%M'))
tariff.detail$Tic.Expiry.Date <- as.Date(strptime(tariff.detail$Tic.Expiry.Date,'%b%d%Y%I:%M'))

# Select details current within the 2022 year

tariff.detail <- subset(tariff.detail, Tic.Start.Date <= as.Date(end_period))
tariff.detail <- subset(tariff.detail, Tic.Expiry.Date >= as.Date(start_period))
tariff.detail$HS8 <- paste(tariff.detail$Tic.Tariff.Level.1,tariff.detail$Tic.Tariff.Level.2,tariff.detail$Tic.Tariff.Level.3,tariff.detail$Tic.Tariff.Level.4,sep='')
tariff.detail <- tariff.detail[,c("HS8", "Tic.Start.Date","Tic.Expiry.Date")]
tariff.detail <- distinct(tariff.detail)

# And take the most recent applicable

tariff.detail <- tariff.detail %>%
  group_by(HS8) %>%
  mutate(most_recent = max(Tic.Start.Date)) %>%
  filter(Tic.Start.Date == most_recent) %>%
  select(HS8) %>%
  ungroup() %>%
  distinct()

# Remove a line that is not actually current

tariff.detail <- tariff.detail %>% filter(!HS8=='37019901')

#------------------------------
#
# Load current tariff rates 
#
#------------------------------

rate <- read.csv('../../../Common data/CusMod tariff/Tariff_Rates.csv',
                 sep='~', as.is=T, colClasses='character', header=T)
rate$Tdrc.Start.Date <- as.Date(strptime(rate$Tdrc.Start.Date,'%b%d%Y%I:%M'))
rate$Tdrc.Expiry.Date <- as.Date(strptime(rate$Tdrc.Expiry.Date,'%b%d%Y%I:%M'))

# Fix formula for 0303.89.20 79L currently formula in CusMod is 2 (Manual)

rate[rate$Tdrc.Tariff.Level.1=='03' &
     rate$Tdrc.Tariff.Level.2=='03' &
     rate$Tdrc.Tariff.Level.3=='89' &
     rate$Tdrc.Tariff.Level.4=='20' &
     rate$Tdrc.Tariff.Level.5=='79',
     "Tdrc.Rate.Formula"] <- '1'

# Select rates applicable within the 2022 year

rate <- subset(rate, Tdrc.Start.Date <= as.Date(end_period))
rate <- subset(rate, Tdrc.Expiry.Date >= as.Date(start_period))

rate$HS8 <- paste(rate$Tdrc.Tariff.Level.1,rate$Tdrc.Tariff.Level.2,
                  rate$Tdrc.Tariff.Level.3,rate$Tdrc.Tariff.Level.4,sep='')
rate <- rate %>%
  select(HS8, Tdrc.Rate.Group, Tdrc.Rate.Formula, Tdrc.Start.Date,
         Tdrc.Expiry.Date, Tdrc.Factor.A, Tdrc.Factor.B) %>%
  distinct()

rate$Tdrc.Factor.A <- round(as.numeric(rate$Tdrc.Factor.A),3)
rate$Tdrc.Factor.B <- round(as.numeric(rate$Tdrc.Factor.B),3)

rate$Tdrc.Factor.A <- ifelse(is.na(rate$Tdrc.Factor.A), 0,rate$Tdrc.Factor.A)
rate$Tdrc.Factor.B <- ifelse(is.na(rate$Tdrc.Factor.B), 0,rate$Tdrc.Factor.B)

rate_parts <- rate %>% 
  filter(Tdrc.Rate.Formula == '2', Tdrc.Rate.Group=='NML') %>%
  mutate(current = 'Manual') %>% 
  select(HS8, Tdrc.Rate.Group, current) %>% 
  pivot_wider(names_from = Tdrc.Rate.Group, values_from = current) %>% 
  mutate(AAN = 'Manual', AU = 'Manual', CA = 'Manual',
         CN = 'Manual', CPT = 'Manual', HK = 'Manual',
         KR = 'Manual', LDC = 'Manual', LLDC = 'Manual',
         MY = 'Manual', PAC = 'Manual', PPP = 'Manual',
         SG = 'Manual', TH = 'Manual', TPA = 'Manual',
         TW = 'Manual', RCEP = 'Manual', Pac = 'Manual', 
         GB = 'Manual') %>% 
  pivot_longer(cols=2:21, names_to = 'Tdrc.Rate.Group', values_to = 'current')
  

rate.2020 <- rate %>%
  select(HS8, Tdrc.Rate.Group, Tdrc.Start.Date, Tdrc.Expiry.Date,
         Tdrc.Factor.A, Tdrc.Factor.B) %>%
  filter(!(HS8 %in% levels(as.factor(rate_parts$HS8)))) %>% 
  group_by(HS8, Tdrc.Rate.Group) %>%
  mutate(most_recent = max(Tdrc.Start.Date)) %>%
  filter(Tdrc.Start.Date == most_recent) %>%
  summarise(most_recent = max(Tdrc.Start.Date)) %>%
  right_join(rate, by=c("HS8", "Tdrc.Rate.Group")) %>%
  filter(Tdrc.Start.Date == most_recent) %>%
  select(HS8, Tdrc.Rate.Group, Tdrc.Rate.Formula,
         Tdrc.Factor.A, Tdrc.Factor.B) %>%
  ungroup() %>%
  distinct() %>%
  mutate(qty_rate = if_else(Tdrc.Rate.Formula == '4', Tdrc.Factor.A,
                            if_else(Tdrc.Rate.Formula == '5',Tdrc.Factor.B,0)),
         val_rate = if_else(Tdrc.Rate.Formula %in% c('3', '5'), Tdrc.Factor.A, 0)) %>%
  select(c(HS8, Tdrc.Rate.Formula, Tdrc.Rate.Group, qty_rate, val_rate)) %>% 
  filter(!(Tdrc.Rate.Formula==5 & qty_rate==0)) # Remove stray CA rate that has rate formula 5 and no qty rate

rate <-left_join(tariff.detail,rate.2020,by='HS8')
rate <- unique(rate)

remove(rate.2020, tariff.detail)

#####
#
# Split and pivot into qty and val rates
#
#####

rate_qty <- rate %>% 
  filter(!Tdrc.Rate.Formula == '2') %>%
  select(HS8, Tdrc.Rate.Group, qty_rate) %>% 
  unique() %>% 
  pivot_wider(names_from = Tdrc.Rate.Group, values_from = qty_rate) %>% 
  mutate(AAN = ifelse(is.na(AAN), NML, AAN),
         AU = ifelse(is.na(AU), NML, AU),
         CA = ifelse(is.na(CA), NML, CA), 
         CN = ifelse(is.na(CN), NML, CN), 
         CPT = ifelse(is.na(CPT), NML, CPT), 
         HK = ifelse(is.na(HK), NML, HK), 
         KR = ifelse(is.na(KR), NML, KR), 
         LDC = ifelse(is.na(LDC), NML, LDC), 
         LLDC = ifelse(is.na(LLDC), NML, LLDC),
         MY = ifelse(is.na(MY), NML, MY), 
         PAC = ifelse(is.na(PAC), NML, PAC), 
         PPP = ifelse(is.na(PPP), NML, PPP), 
         SG = ifelse(is.na(SG), NML, SG), 
         TH = ifelse(is.na(TH), NML, TH), 
         TPA = ifelse(is.na(TPA), NML, TPA), 
         TW = ifelse(is.na(TW), NML, TW), 
         RCEP = ifelse(is.na(RCEP), NML, RCEP), 
         Pac = ifelse(is.na(Pac), NML, Pac), 
         GB = ifelse(is.na(GB), NML, GB),
         AAN = AAN - KR,
         AU = AU - KR,
         CA = CA - KR, 
         CN = CN - KR, 
         CPT = CPT - KR, 
         HK = HK - KR, 
         LDC = LDC - KR, 
         LLDC = LLDC - KR,
         MY = MY - KR,
         NML = NML - KR,
         PAC = PAC - KR, 
         PPP = PPP - KR, 
         SG = SG - KR, 
         TH = TH - KR, 
         TPA = TPA - KR, 
         TW = TW - KR, 
         RCEP = RCEP - KR, 
         Pac = Pac - KR, 
         GB = GB - KR,
         KR = KR - KR) %>% 
  pivot_longer(cols = 2:21, 
               names_to = 'Tdrc.Rate.Group', 
               values_to = 'qty_rate')

rate_val <-rate %>% 
  filter(!Tdrc.Rate.Formula == '2') %>%
  select(HS8, Tdrc.Rate.Group, val_rate) %>% 
  pivot_wider(names_from = Tdrc.Rate.Group, values_from = val_rate) %>% 
  mutate(AAN = ifelse(is.na(AAN), NML, AAN),
         AU = ifelse(is.na(AU), NML, AU),
         CA = ifelse(is.na(CA), NML, CA), 
         CN = ifelse(is.na(CN), NML, CN), 
         CPT = ifelse(is.na(CPT), NML, CPT), 
         HK = ifelse(is.na(HK), NML, HK), 
         KR = ifelse(is.na(KR), NML, KR), 
         LDC = ifelse(is.na(LDC), NML, LDC), 
         LLDC = ifelse(is.na(LLDC), NML, LLDC),
         MY = ifelse(is.na(MY), NML, MY), 
         PAC = ifelse(is.na(PAC), NML, PAC), 
         PPP = ifelse(is.na(PPP), NML, PPP), 
         SG = ifelse(is.na(SG), NML, SG), 
         TH = ifelse(is.na(TH), NML, TH), 
         TPA = ifelse(is.na(TPA), NML, TPA), 
         TW = ifelse(is.na(TW), NML, TW), 
         RCEP = ifelse(is.na(RCEP), NML, RCEP), 
         Pac = ifelse(is.na(Pac), NML, Pac), 
         GB = ifelse(is.na(GB), NML, GB)) %>% 
  pivot_longer(cols = 2:21,
               names_to = 'Tdrc.Rate.Group',
               values_to = 'val_rate')

rate <- rate_qty %>% 
  left_join(rate_val, by = c('HS8', 'Tdrc.Rate.Group')) %>% 
  mutate(val_rate = as.character(val_rate),
         qty_rate = as.character(qty_rate),
         val_rate = ifelse(val_rate == '0', 'Free', paste(val_rate, '%', sep='')),
         qty_rate = ifelse(qty_rate == '0'| qty_rate == '0.0', '', qty_rate),
         current = paste(val_rate, qty_rate, sep='+'),
         current = gsub('^\\+', '', current),
         current = gsub('\\+$', '', current)) %>% 
  select(-qty_rate, -val_rate) %>% 
  bind_rows(rate_parts)

rate <- rate %>% 
  mutate(current = str_replace(current, "Free\\+0\\.39+$", "40c\\/l al"),
         current = str_replace(current, "Free\\+0\\.469+$", "47c \\/l al"),
         current = str_replace(current, "Free\\+0\\.4$", "40c\\/l al"), 
         current = str_replace(current, "Free\\+0\\.47", "47c\\/l al"),
         current = str_replace(current, "Free\\+0\\.5$","50c \\/l al"),
         current = str_replace(current, "Free\\+1\\.7$", "$1.7/kg"),
         current = str_replace(current, "Free\\+1\\.87$", "$1.87/kg"))

rate <- rate %>% pivot_wider(names_from = Tdrc.Rate.Group, values_from = current)

saveRDS(rate, "WTD/rate.rds")
