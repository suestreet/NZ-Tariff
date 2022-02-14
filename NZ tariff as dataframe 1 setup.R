######################################################################
######################################################################
##
## NZ WTO Tariff transposition
## pull in chapters, and classify lines
##
######################################################################
######################################################################

#setwd("D:/Policy Working Area/Trade/Tariff/WTO submissions/HS2022 for WTO")
setwd("/home/sue/Documents/HS2022 for WTO/")

library(tidyr)
library(dplyr)
library(stringr)
library(openxlsx)

######################################################################
#
# Published tariff
# Find headings; format tab xx.xx tab
# Find subheadings; format tab tab tab tab - space
# Find 8 digit; format tab xxxx.xx.xx tab
#
######################################################################

tariff <- data.frame(readLines(con <- "WTD/Tariff01.txt", encoding = "latin1"))

names(tariff)<- "tariff01"
tariff$tariff01 <- as.character(tariff$tariff01)
for(i in 2:9){
  chapter <- paste('WTD/Tariff0', as.character(i),'.txt', sep='')
  addChapter <- data.frame(con<- readLines(chapter, encoding = "latin1"))
  names(addChapter)<- "tariff01"
  addChapter$tariff01 <- as.character(addChapter$tariff01)
  tariff <- bind_rows(tariff,addChapter)
}
for(i in 10:76){
  chapter <- paste('WTD/Tariff', as.character(i),'.txt', sep='')
  addChapter <- data.frame(con <- readLines(chapter, encoding = "latin1"))
  names(addChapter)<- "tariff01"
  addChapter$tariff01 <- as.character(addChapter$tariff01)
  tariff <- bind_rows(tariff,addChapter)
}
for(i in 78:97){
  chapter <- paste('WTD/Tariff', as.character(i),'.txt', sep='')
  addChapter <- data.frame(con <- readLines(chapter, encoding = "latin1"))
  names(addChapter)<- "tariff01"
  addChapter$tariff01 <- as.character(addChapter$tariff01)
  tariff <- bind_rows(tariff,addChapter)
}

tariff.bak <- tariff
tariff <- tariff.bak

#
tariff$tariff01 <- gsub("\t \t", "\t\t", tariff$tariff01)
tariff$tariff01 <- gsub("\t \t", "\t\t", tariff$tariff01)
tariff$tariff01 <- gsub(" \t", "\t", tariff$tariff01)
tariff$tariff01 <- sub("^ \t", "\t", tariff$tariff01)
tariff$tariff01 <- gsub("-", "-", tariff$tariff01)
tariff$tariff01 <- gsub("\t.- -\\s*","\t- - ", tariff$tariff01)
tariff$tariff01 <- gsub("-", "-", tariff$tariff01)
tariff$tariff01 <- gsub("\t.- ", "\t- ", tariff$tariff01)
tariff$tariff01 <- gsub("\t– ", "\t- ", tariff$tariff01)
tariff$tariff01 <- gsub("\t– –", "\t- ", tariff$tariff01)
tariff$tariff01 <- gsub("- - ", "- - ", tariff$tariff01)
tariff$tariff01 <- gsub("– – ", "- - ", tariff$tariff01)
tariff$tariff01 <- gsub("- – ", "- - ", tariff$tariff01)
tariff$tariff01 <- gsub("\t- - ", "\t- - ", tariff$tariff01)
#tariff$tariff01 <- gsub("-", "-", tariff$tariff01)
#tariff$tariff01 <- gsub("-", "-", tariff$tariff01)
#tariff$tariff01 <- gsub("-", "-", tariff$tariff01)
#tariff$tariff01 <- gsub("- -", "--", tariff$tariff01)
#tariff$tariff01 <- gsub("- -", "--", tariff$tariff01)
tariff$tariff01 <- gsub("\\. \\.", "..", tariff$tariff01)
tariff$tariff01 <- gsub("\\. \\.", "..", tariff$tariff01)
tariff$tariff01 <- sub("\t*\\s$", "", tariff$tariff01)
tariff$tariff01 <- gsub("\\*\t\\s\\*", "*\t*", tariff$tariff01) #change all

tariff$line_number <- c(1:dim(tariff)[1])

tariff$chapter_ind <- grepl("^Chapter\\s[0-9]+$", tariff$tariff01)
tariff$heading_ind <- grepl("(\t[0-9][0-9]\\.[0-9][0-9]\t)|(\t[0-9][0-9]\\.[0-9][0-9]$)",
                            substr(tariff$tariff01,1,8))

tariff$subheading_ind <- grepl("^\t[0-9]{4}\\.[0-9]{2}\t+",
                               tariff$tariff01)

tariff$subheading_ind[1] <- FALSE

tariff$item_ind <- grepl("^\t[0-9]{4}\\.[0-9]{2}\\.[0-9]{2}\t+", 
                        substr(tariff$tariff01,1,13))

tariff$stats_ind <- grepl("\\<[0-9]{2}[A-Z]\\s", substr(tariff$tariff01,1,16))

tariff$intermediate_ind <- !tariff$chapter_ind & !tariff$heading_ind & !tariff$subheading_ind &
  !tariff$subheading_ind & !tariff$item_ind & !tariff$stats_ind &
  (grepl("^\\s*\t+\\s*-",tariff$tariff01) |
     grepl("^\\s*\t+\\s*-",tariff$tariff01) |
     grepl("^\\s*\t+\\s*\\.",tariff$tariff01))

tariff$Tariff_Level <- ifelse(tariff$chapter_ind,2,
                           ifelse(tariff$heading_ind,4,
                                  ifelse(tariff$subheading_ind, 6,
                                         ifelse(tariff$item_ind, 8,
                                                ifelse(tariff$stats_ind, 999,0)))))

tariff$branch_hyphen_level <- ifelse(grepl("\t+(-\\s){8}", tariff$tariff01), 8,
                        ifelse(grepl("\t+(-\\s){7}", tariff$tariff01), 7,
                         ifelse(grepl("\t+(-\\s){6}", tariff$tariff01), 6,
                          ifelse(grepl("\t+(-\\s){5}", tariff$tariff01), 5, 
                           ifelse(grepl("\t+(-\\s){4}", tariff$tariff01), 4, 
                            ifelse(grepl("\t+(-\\s){3}", tariff$tariff01), 3, 
                             ifelse(grepl("\t+(-\\s){2}", tariff$tariff01), 2, 
                              ifelse(grepl("\t+(-\\s){1}", tariff$tariff01), 1,
                              0))))))))

tariff$branch_dots_level <- ifelse(grepl("\t+(\\.\\s\\s){8}", tariff$tariff01), 8,
                             ifelse(grepl("\t+(\\.\\s\\s){7}", tariff$tariff01), 7,
                              ifelse(grepl("\t+(\\.\\s\\s){6}", tariff$tariff01), 6,
                               ifelse(grepl("\t+(\\.\\s\\s){5}", tariff$tariff01), 5, 
                                ifelse(grepl("\t+(\\.\\s\\s){4}", tariff$tariff01), 4, 
                                 ifelse(grepl("\t+(\\.\\s\\s){3}", tariff$tariff01), 3, 
                                  ifelse(grepl("\t+(\\.\\s\\s){2}", tariff$tariff01), 2, 
                                   ifelse(grepl("\t+(\\.\\s\\s){1}", tariff$tariff01), 1,
                                   0))))))))


tariff$continued <- grepl("continued$", tariff$tariff01)

# remove continuation lines
tariff <- subset(tariff, !continued)

# And renumber as lines have been removed
tariff$line_number <- c(1:dim(tariff)[1])

# check results for hyphens
check <- tariff %>%
  filter(branch_hyphen_level == 0, branch_dots_level == 0, !continued) %>% 
  


#


saveRDS(tariff, "WTD/tariff_so_far.RDS")
