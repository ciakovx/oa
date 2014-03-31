#first 
setwd("J:/Escience/AcademicAnalytics") #set working directory
#then read in 
doaj <- read.csv(file=file.path(getwd(), "data", "2014-02-02", "DOAJ", "DOAJ.csv"), na.strings="") # read in directory of OA journals file
aa.journals <- read.csv(file=file.path(getwd(), "data", "2014-02-02", "AAJournalList", "AAJournalList.csv"), na.strings="") # read in the Academic Analytics file


library(stringr) # Load stringr package


clean.doaj.trim.main <- function(doajdata) {
  # Get a list of all DOAJ journals and their publishers with whitespace trimmed
  #
  # Arg: The raw DOAJ journal data
  #
  # Returns: A dataframe of DOAJ journals, capitalized, and their publishers, capitalized, with duplicates eliminated
  
  doaj.titles <- data.frame("Title"=str_trim(factor(toupper(doajdata$Title)), side = "both"), "Publisher"=str_trim(factor(toupper(doajdata$Publisher)), side = "both")) # get list of DOAJ titles
  dupe <- duplicated(doaj.titles) # logical vector of duplicates
  doaj.list <- doaj.titles[!dupe,] # return all DOAJ titles as characters, in caps, without duplicates (9,786)
  doaj.list$Title <- as.character(doaj.list$Title) # Convert Title variable to character from factor
  return(doaj.list)
}
doaj.list.trim <- clean.doaj.trim.main(doaj)

clean.aa <- function(aadata) {
  # Get a list of all journals indexed by Academic Analytics. This is used later in an intersection function to analyze which DOAJ titles have punctuation problems inside the string
  # 
  # Arg: The raw Academic Analytics journal data
  # 
  # Returns: A character vector of 14,586 Academic Analytics data, capitalized and with duplicates eliminated
  
  aa.titles <- data.frame("Title"=str_trim(factor(toupper(aadata$AAD.2011.Journal.List)))) # get list of AA titles
  dupe <- duplicated(aa.titles) # logical vector of duplicates
  aa.list <- aa.titles[!dupe,] # return all AA journals as characters, in caps, without duplicates (14,586)
  aa.list <- as.character(aa.list) # convert from factor to character string
  return(aa.list)
}
aa.list <- clean.aa(aa.journals)


doaj.aa.punctuation <- function(doajlistdata, aalistdata) {
  # Delete all spaces in the above created DOAJ & AA Lists, and find the intersection.
  # Compare that intersection to an intersection of the lists with only leading/trailing whitespace trimmed
  # Get the journals from the first list only.
  # 
  # Args: The above created doaj.list.trim and aa.list
  #
  # Returns: A dataframe of journals that appear on both lists, but are not discoverable until all spaces are deleted
  
  #Get intersection of DOAJ trimmed list and AA list
  doaj.aa.trim <- intersect(doajlistdata$Title, aalistdata) # 1,199 common journals after trimming whitespace
  
  # Delete all spaces in both lists and get new intersection
  doaj.repl <- str_replace_all(doajlistdata$Title, pattern = " ", repl="") # delete ALL spaces on doaj list
  aa.repl <- str_replace_all(aalistdata, pattern = " ", repl="") # delete ALL spaces on aa list
  doaj.aa.repl <- intersect(doaj.repl, aa.repl) # 1,226 common journals after deleting all spaces
  
  # Create a vector of journals that appear in both lists but have punctuation problems aside from leading/trailing whitespace
  doaj.aa.trim.repl <- str_replace_all(doaj.aa.trim, pattern = " ", repl="") # delete ALL spaces on trimmed list list
  all.repl <- doaj.aa.repl %in% doaj.aa.trim.repl # get all values in the deleted space list that are in the trimmed space list
  missing <- doaj.aa.repl[!all.repl] # Number of journals that appear in both lists but have punctuation problems aside from leading/trailing whitespace
  missing <- as.data.frame(missing) # Convert from string to dataframe so merge will work
  missing$missing <- as.character(missing$missing) # Coerce the titles to characters
  names(missing) <- "Title"
  
  #Create final dataframe of missing journals and publishers
  #Need to first create doaj.list with spaces in the Title deleted to merge with the above list
  doaj.list.repl <- doajlistdata # get the already created list
  doaj.list.repl$Title <- str_replace_all(doaj.list.repl$Title, pattern = " ", repl="") # trim extra spaces on titles list
  missing <- merge(x = missing, y = doaj.list.repl, by.y="Title", by.x="Title", all.x = TRUE) # Merge the missing journals with their publishers
  names(missing) <- c("Title", "Publisher") #Rename columns
  return(missing)
}
missing <- doaj.aa.punctuation(doaj.list.trim, aa.list)


clean.doaj.alt <- function(doajdata) {
  # Get a list of all alternative DOAJ journal names and their publishers with spaces deleted.
  # Because the list is so small, we can safely delete all spaces and any ramifications in visualization are easily fixed
  #
  # Arg: The raw DOAJ journal data
  #
  # Returns: A dataframe of alternative DOAJ journal titles, capitalized & trimmed, and their publishers, capitalized & trimmed, with duplicates eliminated
  
  doaj.alt.titles <- doaj[complete.cases(doajdata$Title.Alternative),] #There are over 2,000 here, but running an intersect with AA only finds 30. Unfortunately, none of these are in this analysis.
  doaj.alt.titles <- data.frame("Title"=str_trim(factor(toupper(doaj.alt.titles$Title.Alternative)), side = "both"), "Publisher"=str_trim(factor(toupper(doaj.alt.titles$Publisher)), side = "both")) # get list of DOAJ titles
  dupe <- duplicated(doaj.alt.titles) # logical vector of duplicates
  doaj.alt.list <- doaj.alt.titles[!dupe,] # return all DOAJ titles as characters, in caps, without duplicates (9,786)
  doaj.alt.list$Title <- as.character(doaj.alt.list$Title) # Convert Title variable to character from factor
  return(doaj.alt.list)
}
doaj.alt.list <- clean.doaj.alt(doaj)

disciplineOA <- function(directory) {
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
    
    # Get rid of the ".csv" in the basename and return the discipline name for later 
    discbasename <- basename(filepath[i]) # get name of discipline from filename
    q <- nchar(discbasename)# First get an integer value for the number of characters in the basename
    q <- q-4 # subtract 4? characters from that (".csv")
    discbasename <- substr(discbasename, 1, q) # get a character variable of the basename minus the ".csv"
    
    # Return all journals where the department has published at least one article
    sbst <- subset(discipline, discipline$UnitArticles >= 1) # create subset of journals with >=1 citation from researchers in the unity
    
    # Trim whitespace & capitalize journal name. Get trimmed journal name and unit articles
    df.trim <- data.frame("JournalName" = str_trim(toupper(sbst$JournalName), side = "both"), "UnitArticles"=sbst$UnitArticles, stringsAsFactors = FALSE) # create dataframe including only journal name and unit articles and convert to upper case
    
    # Replace all whitespace in & capitalize journal name. Get replaced journal name and unit articles
    df.repl <- data.frame("JournalName" = str_replace_all(toupper(sbst$JournalName), pattern = " ", repl=""), "UnitArticles"=sbst$UnitArticles, stringsAsFactors = FALSE) # create dataframe including only journal name and unit articles and convert to upper case
    
    # run the first loop with the leading/trailing whitespace removed (compare with doaj.list.trim$Title)
    oa1 <- df.trim$Journal %in% doaj.list.trim$Title # create logical variable of journals matching DOAJ's list of open access journals as created in the above command
    if (any(oa1) == T) { # if there are any matches, 
      jrns <- subset(df.trim, oa1) # out of that subset, return the journals that are oa & the unit article count for that journal
      oadf <- data.frame("Title"=jrns$JournalName, jrns$UnitArticles, rep(discbasename,length(jrns$JournalName)), stringsAsFactors = F) # add to the dataframe
      oadf <- merge(x = oadf, y = doaj.list.trim, all.x = TRUE) # join the publisher list to the newly created dataframe
      names(oadf) <- names(df) # change column names so rbind will work
      df <- rbind(df, oadf)
    } 
    
    # run a second loop with ALL whitespaces removed (compare with missing$Title)
    oa2 <- df.repl$Journal %in% missing$Title # create logical variable of journals matching the department's publications with the journals that have spacing issues
    if (any(oa2) == T) { # if there are any matches, 
      jrns <- subset(df.repl, oa2) # out of that subset, return the journals that are oa & the unit article count for that journal
      oadf <- data.frame("Title"=jrns$JournalName, jrns$UnitArticles, rep(discbasename,length(jrns$JournalName)), stringsAsFactors = F) # add to the dataframe
      oadf <- merge(x = oadf, y = missing, all.x = TRUE) # join the publisher list from the missing dataframe to the just-created dataframe
      names(oadf) <- names(df) # change column names so rbind will work
      df <- rbind(df, oadf)
    }
    
    # run the third loop checking for alternative titles (compare with doaj.alt.list.trim$Title)
    oa3 <- df.trim$Journal %in% doaj.alt.list$Title # create logical variable of journals matching DOAJ's list of open access journals as created in the above command
    if (any(oa3) == T) { # if there are any matches, 
      jrns <- subset(df.trim, oa3) # out of that subset, return the journals that are oa & the unit article count for that journal
      oadf <- data.frame("Title"=jrns$JournalName, jrns$UnitArticles, rep(discbasename,length(jrns$JournalName)), stringsAsFactors = F) # add to the dataframe
      oadf <- merge(x = oadf, y = doaj.alt.list, all.x = TRUE) # join the publisher list to the newly created dataframe
      names(oadf) <- names(df) # change column names so rbind will work
      df <- rbind(df, oadf)
    }
    
    #If no values are found in any of the loops, return NAs
    if (!any(oa1) & !any(oa2) & !any(oa3)) { 
    oadf <- data.frame(NA, NA, discbasename, NA) # put NA values into dataframe
      names(oadf) <- names(df)
      df <- rbind(df, oadf)
    }
  }
  return(df)
}

depts <- disciplineOA("AADepartments")


#Write CSVs
#write.csv(doaj.list.trim, file=file.path(getwd(), "results", "2014-03-08", "tables", "doaj.list.trim.csv"))
#write.csv(doaj.alt.list, file=file.path(getwd(), "results", "2014-03-08", "tables", "doaj.alt.list.csv"))
#write.csv(aa.list, file=file.path(getwd(), "results", "2014-03-08", "tables",  "aa.list.csv"))
#write.csv(missing, file=file.path(getwd(), "results", "2014-03-08", "tables",  "missing.csv"))
#write.csv(depts, file=file.path(getwd(), "results", "2014-03-08", "tables", "depts.csv"))

# Need to be pulled from within functions
#write.csv(doaj.aa.trim, file=file.path(getwd(), "results", "2014-02-26", "tables", "doaj.aa.trim.csv"))
#write.csv(doaj.aa.repl, file=file.path(getwd(), "results", "2014-02-26", "tables", "doaj.aa.repl.csv"))
#write.csv(doaj.aa.trim.repl, file=file.path(getwd(), "results", "2014-02-26", "tables", "doaj.aa.trim.repl.csv"))
