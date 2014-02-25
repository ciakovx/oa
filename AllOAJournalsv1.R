scopus <- read.csv(file=file.path(getwd(), "Scopus.csv")) # read in scopus file 
names(scopus) <- c("Title", "Print.issn", "E.issn") # change column names


doaj <- read.csv(file="C:/Users/clarke/Desktop/AcademicAnalyticsData/DOAJ.csv") # read in the DOAJ file
aa.journals <- read.csv(file=file.path(getwd(), "Copy of Journals_AAD2011.csv")) # read in the Academic Analytics file



doaj.titles <- data.frame(doaj$Title) # get list of DOAJ titles
doaj.titles <- factor(doaj.titles$doaj.Title) # convert to factor
doaj.titles <- toupper(doaj.titles) # convert to upper case
dupe.a <- duplicated(doaj.titles) # logical vector of duplicates
doaj.list <- doaj.titles[!dupe.a] # return all DOAJ titles as characters, in caps, without duplicates
doaj.list.dupes <- doaj.titles[dupe.a] # return all duplicated journals from the DOAJ list (14)

aa.titles <- data.frame(aa.journals$AAD.2011.Journal.List) # get list of AA titles
aa.titles <- factor(aa.titles$aa.journals.AAD.2011.Journal.List) # convert to factor
aa.titles <- toupper(aa.titles) # convert to upper case
dupe.b <- duplicated(aa.titles) # logical vector of duplicates
aa.list <- aa.titles[!dupe.b] # return all AA journals as characters, in caps, without duplicates
aa.list.dupes <- aa.titles[dupe.b] # return all duplicated journals from the AA list (203,883)

scopus.titles <- data.frame(scopus$Title) # get list of Scopus titles
scopus.titles <- factor(scopus$Title) # convert to factor
scopus.titles <- toupper(scopus.titles) # convert to upper case
dupe.c <- duplicated(scopus.titles) # logical vector of duplicates
scopus.list <- scopus.titles[!dupe.c] # return all Scopus journals as characters, in caps, without duplicates
scopus.list.dupes <- scopus.titles[dupe.c] # return all duplicated journals from the Scopus list (156)



aa.doaj <- intersect(doaj.list,aa.list) # intersection of AA & DOAJ journals (1,117)
aa.scopus <- intersect(aa.list, scopus.list) # intersection of AA & Scopus (10,745)
scopus.doaj <- intersect(scopus.list, doaj.list) # intersection of Scopus & DOAJ (2,015)

doaj.total <- intersect(aa.doaj, scopus.doaj) # DOAJ titles in both AA & Scopus (796)
