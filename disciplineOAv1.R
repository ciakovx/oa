#first 
setwd("J:/Escience/AcademicAnalytics") #set working directory
#then read in 
doaj <- read.csv(file=file.path(getwd(), "data", "2014-02-02", "DOAJ", "DOAJ.csv"), na.strings="") # read in directory of OA journals file
aa.journals <- read.csv(file=file.path(getwd(), "data", "2014-02-02", "AAJournalList", "AAJournalList.csv"), na.strings="") # read in the Academic Analytics file


library(stringr) # Load stringr package


#get a list of DOAJ titles as character
doaj.main.titles <- data.frame("Title"=doaj$Title) # get list of DOAJ titles
doaj.main.titles <- as.character(doaj.main.titles$Title) # convert to character

#get list of DOAJ alternative titles as character
doaj.alt.titles <- data.frame("Title"=doaj$Title.Alternative) # get list of alternative DOAJ titles
doaj.alt.titles <- data.frame("Title"=doaj.alt.titles[complete.cases(doaj.alt.titles),]) #There are over 2,000 here, but running an intersect with AA only finds 30. Unfortunately, none of these are in this analysis.
doaj.alt.titles <- as.character(doaj.alt.titles$Title) # convert to character

#In Progress Creating Frames Main
doaj.titles <- data.frame("Title"=doaj$Title, "Publisher"=doaj$Publisher) # get list of DOAJ titles
doaj.titles$Title <- str_trim(factor(toupper(doaj.titles$Title)), side = "both") # trim extra spaces on doaj list
doaj.titles$Publisher <- str_trim(factor(toupper(doaj.titles$Publisher)), side = "both") # trim extra spaces on doaj list
dupe <- duplicated(doaj.titles) # logical vector of duplicates
doaj.list <- doaj.titles[!dupe,] # return all DOAJ titles as characters, in caps, without duplicates (9,786)
doaj.dupes <- doaj.titles[dupe,] # return list of dupes

# get a list of AA journals by character
aa.titles <- data.frame(aa.journals$AAD.2011.Journal.List) # get list of AA titles
aa.titles <- factor(aa.titles$aa.journals.AAD.2011.Journal.List) # convert to factor
aa.titles <- str_trim(aa.titles, side = "both") # trim extra spaces on aa list
aa.titles <- toupper(aa.titles) # convert to upper case
dupe.b <- duplicated(aa.titles) # logical vector of duplicates
aa.list <- aa.titles[!dupe.b] # return all AA journals as characters, in caps, without duplicates (14,586)
aa.list.dupes <- aa.titles[dupe.b] # return all duplicated journals from the AA list (203,883)

# All of this is to fix the problems with punctuation and spacing in the DOAJ list
doaj.aa.trim <- intersect(doaj.list$Title, aa.list) # 1,199 common journals after trimming whitespace
# Delete all spaces.
doaj.repl <- str_replace_all(doaj.list$Title, pattern = " ", repl="") # delete ALL spaces on doaj list
aa.repl <- str_replace_all(aa.list, pattern = " ", repl="") # delete ALL spaces on aa list
doaj.aa.repl <- intersect(doaj.repl, aa.repl) # 1,226 common journals after deleting all spaces
# Create a vector of journals that appear in both lists but have punctuation problems aside from leading/trailing whitespace
doaj.aa.trim.repl <- str_replace_all(doaj.aa.trim, pattern = " ", repl="") # delete ALL spaces on trimmed list list
all.repl <- doaj.aa.repl %in% doaj.aa.trim.repl # get all values in the deleted space list that are in the trimmed space list
missing <- doaj.aa.repl[!all.repl] # Number of journals that appear in both lists but have punctuation problems aside from leading/trailing whitespace
missing <- as.data.frame(missing) # Convert from string to dataframe so merge will work
missing$missing <- as.character(missing$missing) # Coerce the titles to characters
names(missing) <- "Title"
#Create dataframe of missing journals and publishers
#Need to first create doaj.list with everything replaced to merge with the above list
doaj.list.repl <- doaj.list # get the already created list
doaj.list.repl$Title <- str_replace_all(doaj.list.repl$Title, pattern = " ", repl="") # trim extra spaces on titles list
missing <- merge(x = missing, y = doaj.list.repl, by.y="Title", by.x="Title", all.x = TRUE) # Merge the missing journals with their publishers
names(missing) <- c("Title", "Publisher")

oa.journals <- function(directory) {
  # Analyze the journal variable in CSV files for each academic department (or school or program), locating matches with journals in the Directory of Open Access Journals
  #
  # Arg:
  #   directory: the file including CSVs for each department (or school or program)
  #  
  # Returns:
  #   a dataframe with three variables: department (or school or program), journal name, and number of articles published in that journal by that department/school/program
  
  dirct <- file.path(getwd(), "data", "2014-02-02", "AA", directory) # set path to folder with CSV files (by department, school, or program)
  files <- list.files(dirct) # list files in directory
  filepath <- file.path(dirct, files) # path of files
  df <- data.frame(matrix(nrow=0, ncol=4)) # create empty data frame
  names(df) <- c("Journal", "ArticlesPublished", "Discipline", "Publisher") # name dataframe columns
  for (i in 1:length(files)) {
    discipline <- read.csv(file=filepath[i]) # read first csv
    discbasename <- basename(filepath[i]) # get name of discipline from filename
    q <- nchar(discbasename) # the next three commands get rid of the ".csv" and return the discipline name. First get an integer value for the number of characters in the basename
    q <- q-4 # subtract 4? characters from that (".csv")
    discbasename <- substr(discbasename, 1, q) # get a character variable of the basename minus the ".csv"
    sbst <- subset(discipline, discipline$UnitArticles >= 1) # create subset of journals with >=1 citation from researchers in the unity
    Trimmed.Journal.Name <- str_trim(sbst$JournalName, side = "both") # Delete leading and trailing spaces
    Trimmed.Journal.Name <- toupper(Trimmed.Journal.Name) # Convert journal name to uppercase
    newdf <- data.frame("JournalName" = Trimmed.Journal.Name, "UnitArticles"=sbst$UnitArticles, stringsAsFactors = FALSE) # create dataframe including only journal name and unit articles and convert to upper case
    oa <- newdf$Journal %in% doaj.list$Title # create logical variable of journals matching DOAJ's list of open access journals as created in the above command
    # run the first loop with the leading/trailing whitespace removed
    if (any(oa) == T) { # if there are any matches, 
      jrns <- subset(newdf, oa) # out of that subset, return the journals that are oa & the unit article count for that journal
      oadf <- data.frame("Title"=jrns$JournalName, jrns$UnitArticles, rep(discbasename,length(jrns$JournalName)), stringsAsFactors = F) # add to the dataframe
      oadf <- merge(x = oadf, y = doaj.list, by.x="Title", by.y="Title", all.x = TRUE)
      names(oadf) <- names(df) # change column names so rbind will work
      df <- rbind(df, oadf)
    } else { # if there are no T values (i.e. no OA publications)
      oadf <- data.frame(NA,NA,discbasename, NA) # put NA values into dataframe
      names(oadf) <- names(df)
      df <- rbind(df, oadf)
    }
    # run a second loop with ALL whitespaces removed
    Replaced.Journal.Name <- str_replace_all(sbst$JournalName, pattern = " ", repl="") # Delete all spaces in department publication titles
    Replaced.Journal.Name <- toupper(Replaced.Journal.Name) # Convert journal name to uppercase
    newdf <- data.frame("JournalName" = Replaced.Journal.Name, "UnitArticles"=sbst$UnitArticles, stringsAsFactors = FALSE) # create dataframe including only journal name and unit articles and convert to upper case
    oa <- newdf$Journal %in% missing$Title # create logical variable of journals matching the department's publications with the journals that have spacing issues
    if (any(oa) == T) { # if there are any matches, 
      jrns <- subset(newdf, oa) # out of that subset, return the journals that are oa & the unit article count for that journal
      oadf <- data.frame("Title"=jrns$JournalName, jrns$UnitArticles, rep(discbasename,length(jrns$JournalName)), stringsAsFactors = F) # add to the dataframe
      oadf <- merge(x = oadf, y = missing, all.x = TRUE)
      names(oadf) <- names(df) # change column names so rbind will work
      df <- rbind(df, oadf)
    }
  }
  return(df)
}

depts.x <- oa.journals("AADepartments")


#Write CSVs
write.csv(doaj.list, file=file.path(getwd(), "results", "2014-02-26", "tables", "doaj.list.csv"))
write.csv(aa.list, file=file.path(getwd(), "results", "2014-02-26", "tables",  "aa.list.csv"))
write.csv(doaj.aa.trim, file=file.path(getwd(), "results", "2014-02-26", "tables", "doaj.aa.trim.csv"))
write.csv(doaj.aa.repl, file=file.path(getwd(), "results", "2014-02-26", "tables", "doaj.aa.repl.csv"))
write.csv(doaj.aa.trim.repl, file=file.path(getwd(), "results", "2014-02-26", "tables", "doaj.aa.trim.repl.csv"))
write.csv(missing, file=file.path(getwd(), "results", "2014-02-26", "tables",  "missing.csv"))
write.csv(depts, file=file.path(getwd(), "results", "2014-02-26", "tables", "depts.csv"))
