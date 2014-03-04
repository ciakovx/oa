# Need stringr package:
library(stringr)

setwd("J:/Escience/AcademicAnalytics") #set working directory


scopus <- read.csv(file=file.path(getwd(), "data", "2014-02-02", "Scopus", "Scopus.selected.csv")) # read in the Academic Analytics file


doaj <- read.csv(file=file.path(getwd(), "data", "2014-02-02", "DOAJ", "DOAJ.csv")) # read in directory of OA journals file
aa.journals <- read.csv(file=file.path(getwd(), "data", "2014-02-02", "AAJournalList", "AAJournalList.csv")) # read in the Academic Analytics file



#get a list of DOAJ titles as character
doaj.main.titles <- data.frame("Title"=doaj$Title) # get list of DOAJ titles
doaj.main.titles <- as.character(doaj.main.titles$Title) # convert to character

#get list of DOAJ alternative titles as character
doaj.alt.titles <- data.frame("Title"=doaj$Title.Alternative) # get list of alternative DOAJ titles
doaj.alt.titles <- data.frame("Title"=doaj.alt.titles[complete.cases(doaj.alt.titles),]) #There are over 2,000 here, but running an intersect with AA only finds 30. Unfortunately, none of these are in this analysis.
doaj.alt.titles <- as.character(doaj.alt.titles$Title) # convert to character

doaj.titles <- c(doaj.main.titles, doaj.alt.titles) # concatenate main & alternative titles
doaj.titles <- factor(doaj.titles)
doaj.titles <- str_trim(doaj.titles, side = "both") # trim extra spaces on doaj list
doaj.titles <- toupper(doaj.titles) # convert to upper case
dupe.a <- duplicated(doaj.titles) # logical vector of duplicates
doaj.list <- doaj.titles[!dupe.a] # return all DOAJ titles as characters, in caps, without duplicates (9,786)
doaj.dupes <- doaj.titles[dupe.a]# return all duplicated journals from the DOAJ list (18)


aa.titles <- data.frame(aa.journals$AAD.2011.Journal.List) # get list of AA titles
aa.titles <- factor(aa.titles$aa.journals.AAD.2011.Journal.List) # convert to factor
aa.titles <- str_trim(aa.titles, side = "both") # trim extra spaces on aa list
aa.titles <- toupper(aa.titles) # convert to upper case
dupe.b <- duplicated(aa.titles) # logical vector of duplicates
aa.list <- aa.titles[!dupe.b] # return all AA journals as characters, in caps, without duplicates (14,586)
aa.list.dupes <- aa.titles[dupe.b] # return all duplicated journals from the AA list (203,883)

scopus.titles <- data.frame(scopus$Title) # get list of Scopus titles
scopus.titles <- factor(scopus$Title) # convert to factor
scopus.titles <- str_trim(scopus.titles, side = "both") # trim extra spaces on doaj list
scopus.titles <- toupper(scopus.titles) # convert to upper case
dupe.c <- duplicated(scopus.titles) # logical vector of duplicates
scopus.list <- scopus.titles[!dupe.c] # return all Scopus journals as characters, in caps, without duplicates (34,275)
scopus.list.dupes <- scopus.titles[dupe.c] # return all duplicated journals from the Scopus list (156)



# Updated totals after applying trim
aa.doaj <- intersect(doaj.list,aa.list) # intersection of AA & DOAJ journals (1,226)
aa.scopus <- intersect(aa.list, scopus.list$scopus.Title) # intersection of AA & Scopus (10,745) #the same: this proves it was DOAJ
scopus.doaj <- intersect(scopus.list$Scopus.Title, doaj.list) # intersection of Scopus & DOAJ (2,118)

doaj.total <- intersect(aa.doaj, scopus.doaj) # DOAJ titles in both AA & Scopus (837)


