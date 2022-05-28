######################################################################
######################################################################
## NZ WTO Tariff extraction
######################################################################
######################################################################

setwd("D:/Policy Working Area/Trade/Tariff/WTO submissions/HS2022 for WTO")
#setwd("/home/sue/Documents/HS2022 for WTO/")
library(tidyr)
library(dplyr)
library(stringr)
library(openxlsx)
source("constructFunctions.R")

tariff <- readRDS("WTD/tariff_so_far.RDS")

lines_in_file <- as.numeric(dim(tariff)[1])

tariff$Tariff_Code <- as.character(NA)
tariff$Tariff_Description <- as.character(NA)

#########
#
# Extract Chapters from tariff txt files
#
#########

line <- 1

while(line <= lines_in_file){
  if(tariff$chapter_ind[line]==TRUE){
    heading_line=line
    tariff$Tariff_Code[line] <- str_extract(tariff$tariff01[line],"[0-9]+$")
    line <- line+1
    tariffText <- tariff$tariff01[line]
    line <- line+1
    while(!grepl("NOTES|NOTE",tariff$tariff01[line])){ # Line is not NOTES
    # if not, there are more lines to append
      lineText <- tariff$tariff01[line]
      tariffText <- paste(tariffText,lineText,sep=" ")
      tariffText <- gsub("  "," ",tariffText)
      line = line + 1
    }
    tariff$Tariff_Description[heading_line] <- tariffText
    line=line+1
  }
  else{
    line<-line + 1
  }
}

#########
# Extract Headings from tariff txt files
#
# Note: this does not pick up Headings that have only a single tariff line
# at this stage. Pulling these from the tariff line when those are done
#
#########

line <- 1
while(line<=lines_in_file){
  if(tariff$heading_ind[line]){
    text <- tariff$tariff01[line]
    returned <- startBuildHeading(text)
    tariff$Tariff_Code[line] <- as.character(returned[1])
    tariffText <- as.character(returned[2])
    tariffText <- sub("^\t+","",tariffText)
    tariffText <- sub("\t+$","",tariffText)
    tariff$Tariff_Description[line] <- tariffText
    line = line + 1
  }
  else{
    line<-line + 1
    }
}

#########
#
# Extract Subheadings from tariff txt files
#
#########

line <- 1
while(line<=lines_in_file){
    if(tariff$subheading_ind[line]==TRUE){
    text <- tariff$tariff01[line]
#   returned <- startBuildHeading(text)
    returned <- startSubhead(text)
    tariff$Tariff_Code[line] <- as.character(returned[1])
    tariffText <- as.character(returned[2])
    tariffText <- sub("^\t+","",tariffText)
    tariffText <- sub("\t+$","",tariffText)
    heading_line <- line
    line = line + 1
    while(!grepl("(:$)|(\t.*\t.*$)",tariffText)){ # neither ends with ':' or 'Free Free'
      # if not, there are more lines to append
      lineText <- tariff$tariff01[line]
      lineText <- sub("^\t[0-9]{4}\\.[0-9]{2}\\.[0-9]{2}\t","",lineText)
      lineText <- sub("[0-9]{2}[A-Z]\t","",lineText)
      lineText <- sub("^[a-z]+\t","",lineText)
      lineText <- sub("(^\\sNo\\.\t)|(^No\\.\t)","",lineText)
      lineText <- sub("^([a-z] [a-z]{2})+\t","",lineText)
      lineText <- sub("^\t+","",lineText)
      lineText <- sub("^( \t)+","",lineText)
      lineText <- sub("\\.\t","",lineText)
      lineText <- sub("\\s+$","",lineText)
      tariffText <- paste(tariffText,substr(lineText,regexpr("[[:alnum:]]", lineText), nchar(lineText)),sep=" ")
      tariffText <- gsub("  "," ",tariffText)
      line = line + 1
    }
    tariffText <- sub("\t.*\t.*$","",tariffText) # remove e.g. Free Free
    tariff$Tariff_Description[heading_line] <- tariffText
  }
  else{
    line<-line + 1
  }
}

#########
#
# extract 8-digit level information
#
#########

line <- 1

while(line<=lines_in_file){
#  while(line<=15340){
    if(tariff$item_ind[line]==TRUE){
    text <- tariff$tariff01[line]
    returned <- startBuildHS8(text)
    tariff$Tariff_Code[line] <- as.character(returned[[1]])
    tariffText <- as.character(returned[[2]])
    heading_line <- line
    # Only two types of ending for the tariff line: Free, or ..
    while(!(str_detect(tariff$tariff01[12051], "\t\\s*Free\\s*$") |
            str_detect(tariff$tariff01[12051], "\t\\s*\\.\\s*\\.\\s*$"))){
      line = line + 1
      lineText <- tariff$tariff01[line]
      lineText <- sub("^\t+","",lineText)
      tariffText <- paste(tariffText,substr(lineText,regexpr("[[:alnum:]]", lineText), nchar(lineText)),sep=" ")
      tariffText <- gsub("  "," ",tariffText)
    }
    tariffText <- sub("\t.*$","",tariffText)
    tariff$Tariff_Description[heading_line] <- tariffText
    line <- line + 1
  
    }
  else{
    line<-line + 1
    }
}

#debug

#source("constructFunctions.R")
#line<=lines_in_file#{
#tariff$item_ind[line]==TRUE#{
#    text <- tariff$tariff01[line]
#    lineText<-text
#    returned <- startBuildHS8(text)
#    tariff$Tariff_Code[line] <- as.character(returned[[1]])
#    tariffText <- as.character(returned[[2]])
#    heading_line <- line
#    # Only two types of ending for the tariff line: Free, or ..
# !(str_detect(tariff$tariff01[12051], "\t\\s*Free\\s*$") |
#            str_detect(tariff$tariff01[12051], "\t\\s*\\.\\s*\\.\\s*$"))#{
#      line = line + 1
#      lineText <- tariff$tariff01[line]
#      lineText <- sub("^\t+","",lineText)
#      tariffText <- paste(tariffText,substr(lineText,regexpr("[[:alnum:]]", lineText), nchar(lineText)),sep=" ")
#      tariffText <- gsub("  "," ",tariffText)
# #   }
#    tariffText <- sub("\t.*$","",tariffText)
#    tariff$Tariff_Description[heading_line] <- tariffText
##   }
##}


####
# Intermediate line text
####

line <- 1
#while(line<=17){
#  if(tariff$intermediate_ind[line]&tariff$branch_dots_level[line]==0){
#    Tariff_Description <- tariff$tariff01[line]
#    line <- line + 1
#  }
#  else{
#    line<-line + 1
#  }
#}
while(line<=lines_in_file){
  if(tariff$intermediate_ind[line]&tariff$branch_dots_level[line]==0){
    tariffText <- tariff$tariff01[line]
    tariffText <- gsub('^\t*','',tariffText)
    tariff$Tariff_Description[line] <- tariffText
    line <- line + 1
  }
  else{
    line<-line + 1
  }
}

# }

#######
#
# Now backfill the Heading blanks
#
#######

tariff <- tariff %>% 
  mutate(next_descrn = lead(Tariff_Description, 1),
         Tariff_Description = if_else(nchar(Tariff_Description)==0,
                                   next_descrn, Tariff_Description))
  
tariff$next_descrn <- NULL           


######
#
# 
#
######

write.csv(tariff, "tariff_8.csv")
write.xlsx(tariff, "tariff_8.xlsx")
saveRDS(tariff, "tariff_8.rds")

#########
#
# extract stats level information
#
#########

tariff$Stats_Key <- as.character(NA)

# This row is a continuation of a stats row if
#     NOT (other type)
#     AND hyphens == 0
#     AND dots == 0 <= THESE 3 lines are ttfo
#     AND starts with tabs then alphanumeric   

tariff$test_follow_on = !tariff$chapter_ind & !tariff$heading_ind &
  !tariff$subheading_ind & !tariff$item_ind & !tariff$stats_ind &
  tariff$branch_hyphen_level == 0 & tariff$branch_dots_level == 0
tariff$test_follow_on[lines_in_file] <- FALSE

line <- 1

while(line<=lines_in_file){
  if(tariff$item_ind[line]) {tariff_8d <- tariff$Tariff_Code[line]}
  if(tariff$stats_ind[line]==TRUE){
    tariff$Tariff_Code[line] <- tariff_8d
    stats_line <- line
    text <- tariff$tariff01[line]
    branch_dots <- tariff$branch_dots_level[line]
    returned <- startStats(text, branch_dots)
    tariff$Stats_Key[line] <- as.character(returned[1])
    tariffText <- as.character(returned[2])
    line = line + 1
    lineText <- tariff$tariff01[line]
    ttfo <- tariff$test_follow_on[line]
    while(ttfo & grepl("^\t+\\s*[[:alpha:]]",lineText)){
      lineText <- sub("^\t+","",lineText)
      tariffText <- paste(tariffText,substr(lineText,regexpr("[[:alnum:]]", lineText), nchar(lineText)),sep="")
      tariffText <- gsub("  "," ",tariffText)
      line = line + 1
      lineText <- tariff$tariff01[line]
    }
    str_replace(tariffText, "\t[[:alnum:][:punct:]]+\t[[:alnum:][:punct:]]+$","")
    tariff$Tariff_Description[stats_line] <- tariffText
  }
  else{
    line<-line + 1
  }
}


