#setwd("~/NZ-Tariff")
setwd("D:/Policy Working Area/Trade/WTO/IDB implementation of HS/WTD 2024 HS22")

library(tidyr)
library(dplyr)
library(stringr)
library(openxlsx)

data_location <- if_else(getwd()=='~/NZ-Tariff/', '~/Documents/Tariff/', 'WTD/')

######################################################################
#
# Published tariff Jan 2024
#
######################################################################

t.print <- data.frame(readLines(con <- paste(data_location, 'Tariff01.txt', sep=''), encoding = "latin1"))

names(t.print)<- "line"
t.print$line <- as.character(t.print$line)
for(i in 2:9){
  chapter <- paste(data_location,'Tariff0', as.character(i),'.txt', sep='')
  addChapter <- data.frame(con<- readLines(chapter, encoding = "latin1"))
  names(addChapter)<- "line"
  addChapter$line <- as.character(addChapter$line)
  t.print <- bind_rows(t.print,addChapter)
}
for(i in 10:76){
  chapter <- paste(data_location,'Tariff', as.character(i),'.txt', sep='')
  addChapter <- data.frame(con <- readLines(chapter, encoding = "latin1"))
  names(addChapter)<- "line"
  addChapter$line <- as.character(addChapter$line)
  t.print <- bind_rows(t.print,addChapter)
}
for(i in 78:98){
  chapter <- paste(data_location,'Tariff', as.character(i),'.txt', sep='')
  addChapter <- data.frame(con <- readLines(chapter, encoding = "latin1"))
  names(addChapter)<- "line"
  addChapter$line <- as.character(addChapter$line)
  t.print <- bind_rows(t.print,addChapter)
}

remove(addChapter, chapter, i, con)

######################################################################
#
# Older report version
#
######################################################################

t.WTO.prev <- readWorkbook(paste(data_location, 'NZ_tariff_August_2023.xlsx', sep=''))

######################################################################
#
# CusMod rates and details
#
######################################################################

t.cusmod.detail <- read.csv(paste(data_location,'Tariff_Details.csv', sep = ''), sep = '~',
                            colClasses = c(rep('character', 6), 'integer', rep('character', 7),
                                           'character'))

t.cusmod.rate <- read.csv(paste(data_location, 'Tariff_Rates.csv', sep=''), sep = '~',
                          colClasses = c(rep('character',8),
                                         'numeric', 'integer', rep('numeric', 6)))

# Format dates

#t.cusmod.detail$Tic.Start.Date <- as.Date(strptime(t.cusmod.detail$Tic.Start.Date,'%b%d%Y%I:%M'))
#t.cusmod.detail$Tic.Expiry.Date <- as.Date(strptime(t.cusmod.detail$Tic.Expiry.Date,'%b%d%Y%I:%M'))

#t.cusmod.rate$Tdrc.Start.Date <- as.Date(strptime(t.cusmod.rate$Tdrc.Start.Date,'%b%d%Y%I:%M'))
#t.cusmod.rate$Tdrc.Expiry.Date <- as.Date(strptime(t.cusmod.rate$Tdrc.Expiry.Date,'%b%d%Y%I:%M'))

# filter to be current in 2024 - Start before end of 2024, finish after start 2024

end_period <- as.Date('2025-1-01')
start_period <- as.Date('2023-12-31')

t.cusmod.detail <- t.cusmod.detail %>%
  mutate(Tic.Start.Date = as.Date(strptime(Tic.Start.Date,'%b%d%Y%I:%M')),
         Tic.Expiry.Date = as.Date(strptime(Tic.Expiry.Date,'%b%d%Y%I:%M'))) %>% 
  filter(Tic.Start.Date <= end_period) %>%
  filter(Tic.Expiry.Date >= start_period) %>% 
  group_by(Tic.Tariff.Level.1, Tic.Tariff.Level.2,
           Tic.Tariff.Level.3, Tic.Tariff.Level.4) %>% 
  mutate(maxDate = max(Tic.Start.Date)) %>% 
  filter(Tic.Start.Date == maxDate) %>% 
  select(-maxDate) %>% 
  ungroup
  unique

t.cusmod.rate <- t.cusmod.rate %>% 
  mutate(Tdrc.Start.Date = as.Date(strptime(Tdrc.Start.Date,'%b%d%Y%I:%M')),
         Tdrc.Expiry.Date = as.Date(strptime(Tdrc.Expiry.Date,'%b%d%Y%I:%M'))) %>% 
  filter(Tdrc.Expiry.Date >= start_period) %>%
  filter(Tdrc.Start.Date <= end_period)%>% 
  group_by(Tdrc.Tariff.Level.1, Tdrc.Tariff.Level.2, 
           Tdrc.Tariff.Level.3, Tdrc.Tariff.Level.4) %>% 
  mutate(maxDate = max(Tdrc.Start.Date)) %>% 
  filter(Tdrc.Start.Date == maxDate) %>% 
  select(-maxDate) %>% 
  ungroup
  unique

# Select fields, and modify names

t.cusmod.detail <- t.cusmod.detail %>% 
  select(c("Tic.Tariff.Level.1", "Tic.Tariff.Level.2", "Tic.Tariff.Level.3", "Tic.Tariff.Level.4",
            "Tic.Statistical.Unit", "Tic.Start.Date", "Tic.Expiry.Date"))

names(t.cusmod.detail) <- c("Tariff.Level.1", "Tariff.Level.2", "Tariff.Level.3", "Tariff.Level.4",
                            "Statistical.Unit", "Start.Date", "Expiry.Date")      

t.cusmod.rate <- t.cusmod.rate %>% 
  select(c("Tdrc.Tariff.Level.1", "Tdrc.Tariff.Level.2", "Tdrc.Tariff.Level.3", "Tdrc.Tariff.Level.4",
           "Tdrc.Rate.Group", "Tdrc.Start.Date", "Tdrc.Expiry.Date", 
           "Tdrc.Rate.Formula", "Tdrc.Factor.A", "Tdrc.Factor.B"))

names(t.cusmod.rate) <- c("Tariff.Level.1", "Tariff.Level.2", "Tariff.Level.3", "Tariff.Level.4", "Rate.Group",    
      "Start.Date", "Expiry.Date", "Rate.Formula", "Factor.A", "Factor.B")

t.cusmod.rate <- t.cusmod.rate %>% 
  mutate(t8d_item = paste(Tariff.Level.1, Tariff.Level.2, '.',
                          Tariff.Level.3, '.', Tariff.Level.4, sep=''))

# Check that all current rate lines are current tariff lines

t.cusmod.old <- t.cusmod.rate %>% 
  select(Tariff.Level.1, Tariff.Level.2,
         Tariff.Level.3, Tariff.Level.4, t8d_item) %>% 
  anti_join(t.cusmod.detail, by = c('Tariff.Level.1', 'Tariff.Level.2',
                                    'Tariff.Level.3', 'Tariff.Level.4')) %>% 
  arrange('Tariff.Level.1', 'Tariff.Level.2',
          'Tariff.Level.3', 'Tariff.Level.4') %>% 
  unique()

# There are 340 rate lines that are not currently active detail lines 

# Check non-current lines really are non-current against text

affected_lines <- t.cusmod.old$t8d_item 

t.print.8d <- t.print %>%
  mutate(t8d = if_else(grepl('\\d{4}\\.\\d{2}\\.\\d{2}', line), TRUE,FALSE)) %>% 
  filter(t8d) %>% 
  mutate(t8d_item = str_extract(line, '\\d{4}\\.\\d{2}\\.\\d{2}'))

t.cusmod.old.error <-  t.cusmod.old %>%
  inner_join(t.print.8d, by = 't8d_item')

# None from previous line

t.cusmod.old.error <-  t.print.8d %>%
  filter(t8d_item %in% affected_lines)

# And again none

# Remove the non-current lines

t.WTO <- t.WTO.prev %>% 
  select(line_number, Tariff_Code, Tariff_Description, Tariff_Code_short)

t.cusmod.current <- t.cusmod.rate %>% 
  select("t8d_item") %>% 
  unique %>% 
  anti_join(t.WTO, by = c("t8d_item" = "Tariff_Code"))

t.WTO.lines  <-  t.WTO %>% 
  select(line_number, Tariff_Code, Tariff_Description) %>% 
  filter(grepl('\\d{4}\\.\\d{2}\\.\\d{2}', Tariff_Code))

# Now check whether all lines are in the previous WTO file

cusmod_oldWTO <- t.cusmod.rate %>% 
  left_join(t.WTO.lines, by = c("t8d_item" = "Tariff_Code"))

# Everything in current CusMod has an existing corresponding line in 
# the previous WTO
#
# Tidy up

remove(affected_lines, cusmod_oldWTO, data_location, end_period,start_period, 
       t.cusmod.current, t.cusmod.detail, t.cusmod.old, t.cusmod.old.error,  
       t.print, t.print.8d, t.WTO.lines, t.WTO.prev)

# 
# Just need to add the new rates to the existing WTO description segment
# 

#t.cusmod.rate.bak <- t.cusmod.rate
#t.cusmod.rate <- t.cusmod.rate.bak

t.cusmod.rate <- t.cusmod.rate %>%
  rename(Tariff_Code = t8d_item) %>% 
  filter(Rate.Group %in% c("AAN", "AU", "CA", "CN", "CPT", "UK",
                           "HK", "KR", "LDC", "LLDC", "MY", "NML",
                           "Pac", "PAC", "PPP", "RCEP", "SG", "TH", "TPA", "TW")) %>%
  select(Tariff_Code, 
         Rate.Group, Rate.Formula, 
         Start.Date, Expiry.Date,
         Factor.A, Factor.B) %>%
  unique() %>% 
  group_by(Tariff_Code, Rate.Group) %>%
  mutate(max.Start = max(Start.Date))%>%
  filter(Start.Date==max.Start) %>%
  mutate(max.Expiry = max(Expiry.Date)) %>%
  filter(Expiry.Date==max.Expiry)

t.cusmod.rate <- t.cusmod.rate %>%
  select(-c(Start.Date, Expiry.Date, max.Start, max.Expiry)) %>% 
  mutate(Duty = ifelse(Rate.Formula == 1, 'Free',
                       ifelse(Rate.Formula == 2, 'Parts',   
                              ifelse(Rate.Formula == 3,
                                     paste(Factor.A,'%',sep=''),
                                     ## replace excise equivalent duty to MFN= 0
                                     ifelse(Rate.Formula == 4,'Free', 
                                            # remove excise equivalent duty component but keep MFN %
                                            ifelse(Rate.Formula == 5,
                                                   paste(Factor.A,'%',sep=''),'Other'))))),
         Duty = gsub('\\.0*%','%',Duty),
         Value.Rate = ifelse(Rate.Formula %in% c(1,4),0,
                             ifelse(Rate.Formula == 2,0,
                                    ifelse(Rate.Formula %in% c(3,5),
                                           as.numeric(Factor.A)/100,NA))),
         Qty.Rate = ifelse(Rate.Formula %in% c(1,3),0,
                           ifelse(Rate.Formula == 2,0,
                                  ifelse(Rate.Formula == 4,as.numeric(Factor.A),
                                         ifelse(Rate.Formula == 5,as.numeric(Factor.B),NA))))) %>% 
  select(-c(Factor.A, Factor.B)) %>%
  select(c(Tariff_Code, Rate.Group, Value.Rate, Qty.Rate)) %>% 
  ungroup

t.rate.value <- t.cusmod.rate %>%
  select(-Qty.Rate) %>% 
  mutate(Value.Rate = if_else(Rate.Group=='NML'& is.na(Value.Rate), 0, Value.Rate)) %>% 
  unique() %>% 
  pivot_wider(names_from = Rate.Group, values_from = Value.Rate) %>% 
  mutate(AU = if_else(is.na(AU), NML, AU),
         NML = NML-AU,
         AAN = if_else(is.na(AAN), NML, AAN-AU),
         CA = if_else(is.na(CA), NML, CA-AU),
         CN = if_else(is.na(CN), NML, CN-AU),
         CPT = if_else(is.na(CPT), NML, CPT-AU),
         UK = if_else(is.na(UK), NML, UK-AU),
         HK = if_else(is.na(HK), NML, HK-AU),
         KR = if_else(is.na(KR), NML, KR-AU),
         LDC = if_else(is.na(LDC), NML, LDC-AU),
         LLDC = if_else(is.na(LLDC), NML, LLDC-AU),
         MY = if_else(is.na(MY), NML, MY-AU),
         Pac = if_else(is.na(Pac), NML, Pac-AU),
         PAC = if_else(is.na(PAC), NML, PAC-AU),
         PPP = if_else(is.na(PPP), NML, PPP-AU),
         RCEP = if_else(is.na(RCEP), NML, RCEP-AU),
         SG = if_else(is.na(SG), NML, SG-AU),
         TH = if_else(is.na(TH), NML, TH-AU),
         TPA = if_else(is.na(TPA), NML, TPA-AU),
         TW = if_else(is.na(TW), NML, TW-AU),
         AU = AU - AU) %>% 
  unique()

t.rate.quantity <- t.cusmod.rate %>%
  select(-Value.Rate) %>% 
  mutate(Qty.Rate = if_else(Rate.Group=='NML'& is.na(Qty.Rate), 0, Qty.Rate)) %>% 
  unique() %>% 
  pivot_wider(names_from = Rate.Group, values_from = Qty.Rate) %>% 
  mutate(AU = if_else(is.na(AU), NML, AU),
         NML = NML-AU,
         AAN = if_else(is.na(AAN), NML, AAN-AU),
         CA = if_else(is.na(CA), NML, CA-AU),
         CN = if_else(is.na(CN), NML, CN-AU),
         CPT = if_else(is.na(CPT), NML, CPT-AU),
         UK = if_else(is.na(UK), NML, UK-AU),
         HK = if_else(is.na(HK), NML, HK-AU),
         KR = if_else(is.na(KR), NML, KR-AU),
         LDC = if_else(is.na(LDC), NML, LDC-AU),
         LLDC = if_else(is.na(LLDC), NML, LLDC-AU),
         MY = if_else(is.na(MY), NML, MY-AU),
         Pac = if_else(is.na(Pac), NML, Pac-AU),
         PAC = if_else(is.na(PAC), NML, PAC-AU),
         PPP = if_else(is.na(PPP), NML, PPP-AU),
         RCEP = if_else(is.na(RCEP), NML, RCEP-AU),
         SG = if_else(is.na(SG), NML, SG-AU),
         TH = if_else(is.na(TH), NML, TH-AU),
         TPA = if_else(is.na(TPA), NML, TPA-AU),
         TW = if_else(is.na(TW), NML, TW-AU),
         AU = AU - AU) %>% 
  unique()

rate.qty <- t.rate.quantity %>% 
  pivot_longer(cols = 2:21, names_to = 'Rate.Group', values_to = 'Qty.Rate')

rate.value <- t.rate.value %>% 
  pivot_longer(cols = 2:21, names_to = 'Rate.Group', values_to = 'Value.Rate')

rates <- rate.value %>% left_join(rate.qty, by = c("Tariff_Code", "Rate.Group"))

rates$Qty.Rate <- round(rates$Qty.Rate, 4)

levels(as.factor(rates$Value.Rate))
levels(as.factor(rates$Qty.Rate))

rates<-rates %>%
  filter(!Tariff_Code %in% c('85232931', '37079010', '37050010'))%>% 
  mutate(Qty.Rate = if_else(Tariff_Code %in% c('2105.00.21', '2105.00.29', '2105.00.31',
                                               '2105.00.39', '2105.00.42', '2105.00.49',
                                               '2106.90.92', '2106.90.93', '2106.90.94',
                                               '2106.90.95', '2106.90.97', '2106.90.98',
                                               '2204.10.18', '2204.21.13', '2204.21.18',
                                               '2204.22.19', '2204.22.90', '2204.29.20',
                                               '2204.29.90', '2205.10.33', '2205.10.38',
                                               '2205.90.33', '2205.90.38', '2208.50.04',
                                               '2208.60.19', '2208.60.29', '2208.70.60',
                                               '2208.70.71', '2402.20.10', '2402.20.90', 
                                               '2403.11.90', '2403.19.90', '2403.91.90', 
                                               '2403.99.90', '2710.12.23', '2710.12.25',
                                               '2710.12.29', '2710.20.25', '3606.10.09'), 0, Qty.Rate),
         Qty.Rate = if_else(Tariff_Code %in% c('2208.50.08') & Rate.Group == 'PPP', 0, Qty.Rate))

# Errors RCEP should be 0.4

rates_2208_50_06 <- rates %>% filter(Tariff_Code == '2208.50.06')
rates <- rates %>% 
  mutate(Qty.Rate = if_else(Tariff_Code == '2208.50.06' & Rate.Group == 'RCEP', 0.4, Qty.Rate))
         
rates_2208_60_21 <- rates %>% filter(Tariff_Code == '2208.60.21')
rates <- rates %>% 
  mutate(Qty.Rate = if_else(Tariff_Code == '2208.60.21' & Rate.Group == 'RCEP', 0.4, Qty.Rate))

rates_2208_60_29 <- rates %>% filter(Tariff_Code == '2208.60.29') # Missing completely

rates_2208_60_29 <- rates_2208_60_21 %>% 
  mutate(Tariff_Code = '2208.60.29')

rates <- rates %>% bind_rows(rates_2208_60_29)
rates <- rates %>% 
  mutate(Qty.Rate = if_else(Tariff_Code == '2208.60.29' & Rate.Group == 'RCEP', 0.4, Qty.Rate))

# Fix of 2208.50 and 2208.60 

saveRDS(rates, "WTD/rates2024.RDS")

t.rate.text <- rates %>% 
  mutate(Value.Rate = as.character(Value.Rate*100),
         Value.Rate = if_else(Value.Rate=='0', 'Free',
                              paste(Value.Rate, '%', sep='')),
         Qty.Rate = as.character(Qty.Rate),
         Qty.Rate = if_else(Qty.Rate == '0', '',
                     if_else(Qty.Rate == '0.50', '+ 50c/L.al.',
                      if_else(Qty.Rate == '0.47', '+ 47c/L.al.',
                       if_else(Qty.Rate == '1.3', '+ $1.30/kg',
                        if_else(Qty.Rate == '1.5', '+ $1.50/kg',
                                '+ $1.87/kg'))))))



quantity_unit <- rates %>% 
  filter(Qty.Rate > 0,
         Qty.Rate < 1.3)

  
