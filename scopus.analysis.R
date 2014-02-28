#About Scopus
library(stringr)

setwd("J:/Escience/AcademicAnalytics") #set working directory


# Clean the Type column
scopus <- read.csv(file=file.path(getwd(), "data", "2014-02-02", "Scopus", "Scopus.selected.csv")) # read in the Academic Analytics file
scopus$Type <- str_trim(scopus$Type) # trim the whitespace (for Book Series)
scopus$Type[scopus$Type == "Trade journal"] <- "Trade Journal" # capitalize j in trade journal

# Get a table of Type Freq & Percentage
scopus.type.table <- as.data.frame(table(scopus$Type)) # Get the Freq for Type
scopus.type.counts <- as.data.frame(round(prop.table(table(scopus$Type)), digits = 3)) # calculates the frequency and percent of each type
scopus.type.tot <- cbind(scopus.type.table, as.numeric(scopus.type.counts$Freq)) # Bind together the columns
names(scopus.type.tot) <- c("Type", "Count", "PercentTotal")

# Write CSVs
write.csv(scopus.type.tot, file=file.path(getwd(), "results", "2014-02-26", "tables", "Scopus", "scopus.type.tot.csv"))
