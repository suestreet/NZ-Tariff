startBuildHeading <- function(lineText){
  lineText <- sub("^u\t","\t", lineText)
  lineText <- sub("^T\t","\t", lineText)
  lineText <- sub("^S\t","\t", lineText)
  lineText <- sub(" \t","\t",lineText)
  tariffNumber <- paste(substr(lineText, 2,3), substr(lineText, 5,6), sep="")
  lineText <- sub("^\t+[0-9]{2}\\.[0-9]{2}","", lineText)
  lineText <- sub("^\t+","",lineText)
  lineText <- sub("^(\\s\t)+","",lineText)
  lineText <- sub("\\.\t",". ",lineText)
  result <- list(tariffNumber, lineText)
  return(result)
}
startBuildHS8 <- function(lineText){
  lineText <- sub("^u\t","\t", lineText)
  lineText <- sub("^T\t","\t", lineText)
  lineText <- sub("^S\t","\t", lineText)
  lineText <- sub(" \t","\t",lineText)
  tariffNumber <- paste(substr(lineText, 2,5),
                        substr(lineText, 7,8),
                        substr(lineText, 10,11), sep="")
  lineText <- sub("^\t[0-9]{4}\\.[0-9]{2}\\.[0-9]{2}\t","",lineText) # Delete number off front of string
  lineText <- sub("[0-9]{2}[A-Z]\t","",lineText) # Delete stats code off front of string
  lineText <- sub("(^[A-z]+\t)|(^ [A-z]+\t)","",lineText)
  lineText <- sub("(^\\sNo\\.\t)|(^No\\.\t)","",lineText)
  lineText <- sub("^([a-z] [a-z]{2})+\t","",lineText)
  lineText <- sub("^\\.+\t","",lineText)
  lineText <- sub("^\t+","",lineText)
  lineText <- sub("^( \t)+","",lineText)
  lineText <- sub("\\.\t","",lineText)
  lineText <- sub("\t+$","\t",lineText)
  result <- list(tariffNumber, lineText)
  return(result)
}
startSubhead <- function(lineText){
  lineText <- sub("^u\t","\t", lineText)
  lineText <- sub("^T\t","\t", lineText)
  lineText <- sub("^S\t","\t", lineText)
  if(grepl("^\t[0-9]", lineText)){
    tariffNumber <- paste(substr(lineText, 2,5),
                        substr(lineText, 7,8), sep="")
  }
  else{
    tariffNumber <- 'NA'
  }
  lineText <- sub("^\t[0-9]{4}\\.[0-9]{2}\t","",lineText)
  lineText <- sub("[0-9]{2}[A-Z]\t","",lineText)
  lineText <- sub("^[a-z]+\t","",lineText)
  lineText <- sub("^([a-z] [a-z]{2})+\t","",lineText)
#  lineText <- sub("\\s\t","\t",lineText)
  lineText <- sub("^\t+","",lineText)
  lineText <- sub("\t+","",lineText)
  lineText <- sub("\\.\t",". ",lineText)
  lineText <- sub("^\\s+","",lineText)
  lineText <- sub("\\s+$","",lineText)
  result <- list(tariffNumber, lineText)
  return(result)
}

startStats <- function(lineText, dots_level){
  tariffNumber <- str_extract(lineText, "[0-9]{2}[A-Z]")

# Strip off tariff code, if this line has one
  lineText <- str_replace(lineText, "^.*\t+.*[0-9][0-9][0-9][0-9]\\.[0-9][0-9]\\.[0-9][0-9]\t\\s*","")
# Strip off the stats key
  lineText <- sub("\t*[0-9]{2}[A-Z]","",lineText) # Delete stats code off front of string
  lineText <- sub("^\t*\\s*No\\.", "", lineText)
  lineText <- sub("^\t*kg\\s*\t+\\s*", "", lineText)
  lineText <- sub("^\t*pr\\s*\t+\\s*", "", lineText)
  lineText <- sub("^\t*m3\\s*\t+\\s*", "", lineText)
  lineText <- sub("^\t*m2\\s*\t+\\s*", "", lineText)
  lineText <- sub("^\t*tne\\s*\t+\\s*", "", lineText)
  lineText <- sub("^\t*\\.\\.\\s*\t+\\s*", "", lineText)
  lineText <- sub("^\t*", "", lineText)
#  lineText <- ifelse(dots_level == 8, sub("(\\.\\s+){8}","", lineText),
#               ifelse(dots_level == 7, sub("(\\.\\s+){7}","", lineText),
#                ifelse(dots_level == 6, sub("(\\.\\s+){6}","", lineText),
#                 ifelse(dots_level == 5, sub("(\\.\\s+){5}","", lineText),
#                  ifelse(dots_level == 4, sub("(\\.\\s+){4}","", lineText),
#                   ifelse(dots_level == 3, sub("(\\.\\s+){3}","", lineText),
#                    ifelse(dots_level == 2, sub("(\\.\\s+){2}","", lineText),
#                     ifelse(dots_level == 1, sub("(\\.\\s+){1}","", lineText),
#                      lineText))))))))
  result <- list(tariffNumber, lineText)
  return(result)
}
