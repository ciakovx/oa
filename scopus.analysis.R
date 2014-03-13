#About Scopus
library(stringr)

setwd("J:/Escience/AcademicAnalytics") #set working directory
scopus <- read.csv(file=file.path(getwd(), "data", "2014-02-02", "Scopus", "Scopus.selected.csv")) # read in the Academic Analytics file

clean.scopus <- function(scopusdata) {
  # Get a list of all journals indexed by Scopus. 
  # 
  # Arg: The raw Scopus journal data
  # 
  # Returns: A dataframe of Scopus journals and types, capitalized and with duplicates eliminated
  
  scopus.titles <- data.frame("Title"=str_trim(factor(toupper(scopusdata$Title)), side = "both"), "Type"=str_trim(factor(toupper(scopusdata$Type)), side = "both")) # get list of Scopus titles
  dupe <- duplicated(scopus.titles$Title) # logical vector of duplicates
  scopus.list <- scopus.titles[!dupe,] # Get all values that are not duplicated
  scopus.list$Title <- as.character(scopus.list$Title) # Convert Title from factor to character
  return(scopus.list)
}
scopus.list <- clean.scopus(scopus)

aa.data <- function(aadata) {
  # Get a list of all journals indexed by Academic Analytics. This is used later in an intersection function to analyze which DOAJ titles have punctuation problems inside the string
  # 
  # Arg: The raw Academic Analytics journal data
  # 
  # Returns: A character string of 14,586 Academic Analytics data, capitalized and with duplicates eliminated
  
  aa.titles <- data.frame("Title"=str_trim(factor(toupper(aadata$AAD.2011.Journal.List)))) # get list of AA titles
  dupe <- duplicated(aa.titles) # logical vector of duplicates
  aa.list <- aa.titles[!dupe,] # return all AA journals as characters, in caps, without duplicates (14,586)
  aa.list <- as.character(aa.list) # convert from factor to character string
  #aa.list.dupes <- aa.titles[dupe] # return all duplicated journals from the AA list (203,883)
  return(aa.list)
}
aa.list <- aa.data(aa.journals)

aa.scopus <- intersect(scopus.list$Title, aa.list))


# Get a table of Type Freq & Percentage
scopus.type.table <- as.data.frame(table(scopus.list$Type)) # Get the Freq for Type
names(scopus.type.table) <- c("Type", "Count")

# Write CSVs
write.csv(scopus.type.table, file=file.path(getwd(), "results", "2014-02-26", "tables", "Scopus", "scopus.type.table.csv"))
