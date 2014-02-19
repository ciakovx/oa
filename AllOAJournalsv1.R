# Get all DOAJ journals on the AA journal list


#first 
setwd("C:/Users/clarke/Desktop/AcademicAnalyticsData") #set working directory
#then read in 
doaj <- read.csv(file="C:/Users/clarke/Desktop/AcademicAnalyticsData/DOAJ.csv") # import directory of OA journals file
all.journals <- read.csv(file=file.path(getwd(), "Copy of Journals_AAD2011.csv")) # read in the file


a <- data.frame(doaj$Title) # get list of DOAJ titles
a <- factor(a$doaj.Title) # convert to factor
a <- toupper(a) # convert to upper case
dupe.a <- duplicated(a) # logical vector of duplicates
a <- a[!dupe.a] # return all DOAJ titles

b <- data.frame(all.journals$AAD.2011.Journal.List) # get list of AA titles
b <- factor(b$all.journals.AAD.2011.Journal.List) # convert to factor
b <- toupper(b) # convert to upper case
dupe.b <- duplicated(b) # logical vector of duplicates
b <- b[!dupe.b] # return all AA journals

c <- intersect(a,b) # intersection of AA & DOAJ journals

## This was a first go, but because of the capitalization issues, it was a no go.
# matches <- all.journals$AAD.2011.Journal.List %in% doaj$Title # logical vector of journals in the list that match DOAJ Titles
# all.oa <- all.journals[matches, ] # create a subset of all.journals list according to the matches vector
# names(all.oa) <- c("Journal", "Discipline") #rename columns
# all.oa2 <- factor(all.oa$Journal) # remove factors (journals) that were excluded from subset 
# oalist <- unique(all.oa) # get unique values
# all.oa <- as.character(oalist) # final vector 
# write.csv(all.oa, file="alloa.csv")
