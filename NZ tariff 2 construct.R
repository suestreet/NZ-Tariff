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

tariff <- readRDS("WTD/tariff_so_far.rds")

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
    line <- line + 1
    while(!(grepl("NOTES|NOTE|Notes|Note",tariff$tariff01[line])| 
            grepl("^\t*\\d{2}\\.\\d{2}",tariff$tariff01[line]))){ # Line is not NOTES or 98.01
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
    headingLine = line
    text <- tariff$tariff01[line]
    returned <- startBuildHeading(text)
    tariff$Tariff_Code[line] <- as.character(returned[1])
    tariffText <- as.character(returned[2])
    tariffText <- sub("^\t+","",tariffText)
    tariffText <- sub("\t+$","",tariffText)
    tariff$Tariff_Description[headingLine] <- tariffText
    line = line + 1
    while(!(tariff$item_ind[line]|tariff$intermediate_ind[line])){ # Line is not tariff item or intermediate
      lineText <- tariff$tariff01[line]
      lineText <- sub("^\t+","",lineText)
      lineText <- sub("\t+$","",lineText)
      lineText <- sub("\\d{4}\\.\\d{2}","",lineText)
      tariffText <- paste(tariffText,lineText,sep=" ")
      tariffText <- gsub("  "," ",tariffText)
      tariff$Tariff_Description[headingLine] <- tariffText
      line = line + 1
    }
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
#while(line<=26764){
    if(tariff$subheading_ind[line]==TRUE){
    text <- tariff$tariff01[line]
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
#while(line<=26765){
    if(tariff$item_ind[line]){
    text <- tariff$tariff01[line]
    returned <- startBuildHS8(text)
    tariff$Tariff_Code[line] <- as.character(returned[[1]])
    tariffText <- as.character(returned[[2]])
    tariffText <- sub("(^m²\t)","",tariffText) # Brute force as for some reason not working within the function
    tariffText <- sub("(^m³\t)","",tariffText) # Brute force as for some reason not working within the function
    item_line <- line
    # Only three types of ending for the tariff line: Free, or .. , or The rates applicable
    while(!(str_detect(tariff$tariff01[line], "\t\\s*Free\\s*$") |
            str_detect(tariff$tariff01[line], "\t\\s*\\.\\s*\\.\\s*$")|
#            str_detect(tariff$tariff01[line], "\tThe rate.* applicable.*$"))){
            str_detect(tariff$tariff01[line], "\tThe rate.*$"))){
        line = line + 1
      lineText <- tariff$tariff01[line]
      lineText <- sub("^\t+","",lineText)
      tariffText <- paste(tariffText,substr(lineText,regexpr("[[:alnum:]]", lineText), nchar(lineText)),sep=" ")
      tariffText <- gsub("  "," ",tariffText)
    }
    tariffText <- sub("\t.*$","",tariffText)
    tariff$Tariff_Description[item_line] <- tariffText
    line <- line + 1
  
    }
  else{
    line<-line + 1
    }
}

####
# Intermediate line text
####

line <- 1
while(line<=lines_in_file){
#while(line<=42789){
    int_line = line
  if(tariff$intermediate_ind[line]&tariff$branch_dots_level[line]==0){
    tariffText <- tariff$tariff01[line]
    tariffText <- sub('^\t*','',tariffText)
    tariffText <- sub('\t*$','',tariffText)
    tariffText <- sub("\tFree\tFree$", "", tariffText) #Shouldn't be any of these, but...
    line <- line + 1
    while(!(tariff$item_ind[line]|
            tariff$intermediate_ind[line]|
            tariff$subheading_ind[line]|
            tariff$stats_ind[line])&
            tariff$branch_dots_level[line]==0){ # Line is not tariff item another intermediate or stats
      lineText <- tariff$tariff01[line]
      lineText <- sub("^\t+","",lineText) #strip any leading tabs
      lineText <- sub("\t+$","",lineText) #strip any following tabs
      tariffText <- paste(tariffText,lineText,sep=" ")
      tariffText <- gsub("  "," ",tariffText)
      line <- line+1
    }
    tariffText <- sub("\tFree\tFree$", "", tariffText) #Shouldn't be any of these, but...
    tariff$Tariff_Description[int_line] <- tariffText
#   line = line + 1
  }
  else{
    line<-line + 1
  }
}

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


write.csv(tariff, "tariff_8.csv")
saveRDS(tariff, "tariff_8.rds")

#####
#
# Output file
#
#####

output_file <- tariff %>% 
  select(line_number, intermediate_ind, Tariff_Level, Tariff_Code, Tariff_Description) %>% 
  mutate(Tariff_Level = if_else(intermediate_ind & grepl('^-', Tariff_Description),
                                3, Tariff_Level),
         Tariff_Code_short = Tariff_Code,
         Tariff_Code = ifelse(Tariff_Level == 4,
                               paste(substr(Tariff_Code, 1,2), substr(Tariff_Code, 3,4), sep = '.'),
                        ifelse(Tariff_Level == 6,
                                paste(substr(Tariff_Code, 1,4), substr(Tariff_Code, 5,6), sep = '.'),
                         ifelse(Tariff_Level == 8,
                                 paste(substr(Tariff_Code, 1,4), substr(Tariff_Code, 5,6),
                                        substr(Tariff_Code, 7,8), sep = '.'), Tariff_Code)))) %>% 
  filter(Tariff_Level>0,
         Tariff_Level<999)

write.xlsx(output_file, "tariff_8.xlsx", overwrite = TRUE)
write.csv(output_file, "tariff_8.csv")
saveRDS(output_file, "tariff_8.rds")


