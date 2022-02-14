######################################################################
######################################################################
## NZ WTO Tariff extraction
######################################################################
######################################################################

# setwd("D:/Policy Working Area/Trade/Tariff/WTO submissions/HS2022 for WTO")
setwd("/home/sue/Documents/HS2022 for WTO/")
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
  if(tariff$heading_ind[line]==TRUE){
    text <- tariff$tariff01[line]
    returned <- startBuildHeading(text)
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
  if(tariff$item_ind[line]==TRUE){
    text <- tariff$tariff01[line]
    returned <- startBuildHS8(text)
    tariff$Tariff_Code[line] <- as.character(returned[1])
    tariffText <- as.character(returned[2])
    heading_line <- line
    while(!grepl("\t[[:alnum:][:punct:]]+\t[[:alnum:][:punct:]]+$",tariffText)){
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

#########
#
# Extract line tariff rates
#
#########


tariff$Ad_Val_Rates <- str_extract(tariff$Tariff_Description, "\t[[:alnum:][:punct:]]+\t[[:alnum:][:punct:]]+$")



# Clean up the Tariff_Description removing the duty info at the end of the line

tariff$Tariff_Description <- str_replace(tariff$Tariff_Description, "\t[[:alnum:][:punct:]]+\t[[:alnum:][:punct:]]+$","")

#########
#
# extract subheading level information and
# other intermediate lines
#
#########

line <- 1

while(line <= lines_in_file){
  if(tariff$subheading_ind[line]|tariff$intermediate_ind[line]){
    heading_line <- line
    text <- tariff$tariff01[line]
    returned <- startSubhead(text)
    tariff$Tariff_Code[line] <- ifelse(is.na(returned[1]),NA,as.character(returned[1]))
    tariffText <- as.character(returned[2])
    while(!grepl(":$",tariffText)){
      line = line + 1
      lineText <- tariff$tariff01[line]
      lineText <- sub("^\t+","",lineText)
      lineText <- sub("\\s+$","",lineText)
      tariffText <- paste(tariffText,substr(lineText,regexpr("[[:alnum:]]", lineText), nchar(lineText)),sep=" ")
      tariffText <- gsub("  "," ",tariffText)
    }
#    tariffText <- sub("\t.*$","",tariffText)
    tariff$Tariff_Description[heading_line] <- tariffText
    line <- line + 1
  }
  else{
    line<-line + 1
  }
}

#
# Strip off leading ..
#

translated <- tariff %>%
  filter(!is.na(Tariff_Description)) %>%
  select(Tariff_Level,Tariff_Code, Stats_Key, Tariff_Description,
         line_number, chapter_ind, heading_ind, subheading_ind, item_ind,
         stats_ind, intermediate_ind, branch_hyphen_level)
  
translated$Tariff_Description <- gsub("-","\\-",translated$Tariff_Description)
translated$Tariff_Description <- gsub("\\-\\s\\-", "--",translated$Tariff_Description)
translated$Tariff_Description <- gsub("\\-\\s\\-", "--",translated$Tariff_Description)
translated$Tariff_Description <- gsub("\\-\\s\\-", "--",translated$Tariff_Description)
translated$Tariff_Level[is.na(translated$Tariff_Level)]<-'X'
translated$Tariff_Description <- ifelse(translated$Tariff_Level=='1',
                                        str_to_upper(translated$Tariff_Description),
                                        translated$Tariff_Description)
#
#write.xlsx(translated, "../Data/Tariff_processed_14_Feb_2022.xlsx", overwrite = TRUE)
write.xlsx(translated, "WTD/Tariff_processed_14_Feb_2022.xlsx", overwrite = TRUE)

