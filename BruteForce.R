setwd("~/NZ-Tariff")

library(tidyr)
library(dplyr)
library(stringr)
library(openxlsx)

######################################################################
#
# Published tariff Jan 2024
#
######################################################################

t.print <- data.frame(readLines(con <- "~/Documents/Tariff/Tariff01.txt", encoding = "latin1"))

names(t.print)<- "line"
t.print$line <- as.character(t.print$line)
for(i in 2:9){
  chapter <- paste('~/Documents/Tariff/Tariff0', as.character(i),'.txt', sep='')
  addChapter <- data.frame(con<- readLines(chapter, encoding = "latin1"))
  names(addChapter)<- "line"
  addChapter$line <- as.character(addChapter$line)
  t.print <- bind_rows(t.print,addChapter)
}
for(i in 10:76){
  chapter <- paste('~/Documents/Tariff/Tariff', as.character(i),'.txt', sep='')
  addChapter <- data.frame(con <- readLines(chapter, encoding = "latin1"))
  names(addChapter)<- "line"
  addChapter$line <- as.character(addChapter$line)
  t.print <- bind_rows(t.print,addChapter)
}
for(i in 78:98){
  chapter <- paste('~/Documents/Tariff/Tariff', as.character(i),'.txt', sep='')
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

t.WTO.prev <- readWorkbook('~/Documents/Tariff/NZ_tariff_05_September_2022.xlsx')

######################################################################
#
# CusMod rates and details
#
######################################################################

t.cusmod.detail <- read.csv('~/Documents/Tariff/Tariff_Details.csv', sep = '~',
                            colClasses = c(rep('integer', 5),'character',
                                           'integer', rep('character', 7),
                                           'character'))

t.cusmod.rate <- read.csv('~/Documents/Tariff/Tariff_Rates.csv', sep = '~',
                          colClasses = c(rep('integer', 5),rep('character',3),
                                         'numeric', 'integer', rep('numeric', 6)))

# Format dates

t.cusmod.detail$Tic.Start.Date <- as.Date(strptime(t.cusmod.detail$Tic.Start.Date,'%b%d%Y%I:%M'))
t.cusmod.detail$Tic.Expiry.Date <- as.Date(strptime(t.cusmod.detail$Tic.Expiry.Date,'%b%d%Y%I:%M'))

t.cusmod.rate$Tdrc.Start.Date <- as.Date(strptime(t.cusmod.rate$Tdrc.Start.Date,'%b%d%Y%I:%M'))
t.cusmod.rate$Tdrc.Expiry.Date <- as.Date(strptime(t.cusmod.rate$Tdrc.Expiry.Date,'%b%d%Y%I:%M'))

# filter to be current in Jan 2024 - Start before end Jan, finish after start Jan (should this be end?)

start_period <- as.Date('2024-02-01')
end_period <- as.Date('2024-01-01')

t.cusmod.detail <- t.cusmod.detail %>% filter(Tic.Start.Date <= end_period)
t.cusmod.detail <- t.cusmod.detail %>% filter(Tic.Expiry.Date >= start_period)
t.cusmod.rate <- t.cusmod.rate %>% filter(Tdrc.Start.Date <= end_period)
t.cusmod.rate <- t.cusmod.rate %>% filter(Tdrc.Expiry.Date >= start_period)

# Check that all current rate lines are current tariff lines

t.cusmod <- t.cusmod.rate %>% 
  left_join(t.cusmod.detail, by = c('Tdrc.Tariff.Level.1' = 'Tic.Tariff.Level.1',
                                    'Tdrc.Tariff.Level.2' = 'Tic.Tariff.Level.2',
                                    'Tdrc.Tariff.Level.3' = 'Tic.Tariff.Level.3',
                                    'Tdrc.Tariff.Level.4' = 'Tic.Tariff.Level.4',
                                    'Tdrc.Tariff.Level.5' = 'Tic.Tariff.Level.5'))

# Hallelujah!

