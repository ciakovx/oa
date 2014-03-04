#About Scopus
library(stringr)

setwd("J:/Escience/AcademicAnalytics") #set working directory
scopus <- read.csv(file=file.path(getwd(), "data", "2014-02-02", "Scopus", "Scopus.selected.csv")) # read in the Academic Analytics file

# Clean the data: removing whitespace and converting to uppercase
scopus.titles <- data.frame(scopus$Title, scopus$Type) # get list of Scopus titles
scopus.titles$scopus.Title <- factor(scopus$Title) # convert to factor
scopus.titles$scopus.Title <- str_trim(scopus.titles$scopus.Title, side = "both") # trim extra spaces on doaj list
scopus.titles$scopus.Title <- toupper(scopus.titles$scopus.Title) # convert to upper case
scopus.titles$scopus.Type <- factor(scopus$Type) # convert to factor
scopus.titles$scopus.Type <- str_trim(scopus.titles$scopus.Type, side = "both") # trim the whitespace (for Book Series)
scopus.titles$scopus.Type <- toupper(scopus.titles$scopus.Type) # convert to upper case
dupe.c <- duplicated(scopus.titles$scopus.Title) # logical vector of duplicates
scopus.list <- scopus.titles[!dupe.c,]
scopus.list.dupes <- scopus.titles[dupe.c,] # return all duplicated journals from the Scopus list (156)

# Get a table of Type Freq & Percentage
scopus.type.table <- as.data.frame(table(scopus.list$scopus.Type)) # Get the Freq for Type
names(scopus.type.table) <- c("Type", "Count")

# Write CSVs
write.csv(scopus.type.table, file=file.path(getwd(), "results", "2014-02-26", "tables", "Scopus", "scopus.type.table.csv"))
