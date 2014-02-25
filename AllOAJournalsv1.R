scopus <- read.csv(file=file.path(getwd(), "Scopus.csv")) # read in scopus file 
names(scopus) <- c("Title", "Print.issn", "E.issn") # change column names


doaj <- read.csv(file="C:/Users/clarke/Desktop/AcademicAnalyticsData/DOAJ.csv") # read in the DOAJ file
aa.journals <- read.csv(file=file.path(getwd(), "Copy of Journals_AAD2011.csv")) # read in the Academic Analytics file



doaj.list <- data.frame(doaj$Title) # get list of DOAJ titles
doaj.list <- factor(doaj.list$doaj.Title) # convert to factor
doaj.list <- toupper(doaj.list) # convert to upper case
dupe.a <- duplicated(doaj.list) # logical vector of duplicates
doaj.list <- doaj.list[!dupe.a] # return all DOAJ titles

aa.list <- data.frame(aa.journals$AAD.2011.Journal.List) # get list of AA titles
aa.list <- factor(aa.list$aa.journals.AAD.2011.Journal.List) # convert to factor
aa.list <- toupper(aa.list) # convert to upper case
dupe.b <- duplicated(aa.list) # logical vector of duplicates
aa.list <- aa.list[!dupe.b] # return all AA journals


scopus.list <- data.frame(scopus$Title) # get list of AA titles
scopus.list <- factor(scopus$Title) # convert to factor
scopus.list <- toupper(scopus.list) # convert to upper case
dupe.c <- duplicated(scopus.list) # logical vector of duplicates
scopus.list <- scopus.list[!dupe.c] # return all AA journals



aa.doaj <- intersect(doaj.list,aa.list) # intersection of AA & DOAJ journals (1,117)
aa.scopus <- intersect(aa.list, scopus.list) # intersection of AA & Scopus (10,745)
scopus.doaj <- intersect(scopus.list, doaj.list) # intersection of Scopus & DOAJ (2,015)
