# This looks at the complete list of OA journals and compares it with the DOAJ list to return a master list of OA journals listed in the AA database.
# I ended up doing it in Excel because this one omitted specifically foreign language journals and journals with unusual capitalization
# Need to figure out if that is avoidable
# But the difference was negligible (This code did not pick up 40 journals)


all.journals <- read.csv(file=file.path(getwd(), "Copy of Journals_AAD2011.csv"))
all.oa <- all.journals$AAD.2011.Journal.List %in% doaj$Title # full list of journals in the list that match DOAJ Titles
all.oa <- all.journals[all.oa, ] # subset that dataframe with matches
names(all.oa) <- c("Journal", "Discipline") #rename columns
all.oa <- as.character(all.oa$Journal)
str(all.oa)
oalist <- unique(all.oa) # get unique values
all.oa <- as.character(oalist) # final vector 
write.csv(all.oa, file="alloa.csv")

