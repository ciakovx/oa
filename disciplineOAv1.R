#first 
setwd("C:/Users/clarke/Desktop/AcademicAnalyticsData") #set working directory
#then read in 
doaj <- read.csv(file="C:/Users/clarke/Desktop/AcademicAnalyticsData/DOAJ.csv") # import directory of OA journals file


#get a list of DOAJ titles as character
doaj.titles <- data.frame(doaj$Title) # get list of DOAJ titles
doaj.titles <- factor(doaj.titles$doaj.Title) # convert to factor
doaj.titles <- toupper(doaj.titles) # convert to upper case
dupe.a <- duplicated(doaj.titles) # logical vector of duplicates
doaj.list <- doaj.titles[!dupe.a] # return all DOAJ titles as characters, in caps, without duplicates
doaj.list.dupes <- doaj.titles[dupe.a] # return all duplicated journals from the DOAJ list (14)


oa.journals <- function(directory) {
  # Analyze the journal variable in CSV files for each academic department (or school or program), locating matches with journals in the Directory of Open Access Journals
  #
  # Arg:
  #   directory: the file including CSVs for each department (or school or program)
  #  
  # Returns:
  #   a dataframe with three variables: department (or school or program), journal name, and number of articles published in that journal by that department/school/program
  
  dirct <- file.path(getwd(), directory) # set path to folder with CSV files (by department, school, or program)
  files <- list.files(dirct) # list files in directory
  filepath <- file.path(dirct, files) # path of files
  df <- data.frame(matrix(nrow=0, ncol=3)) # create empty data frame
  names(df) <- c("Journal", "ArticlesPublished", "Discipline") # name dataframe columns
  for (i in 1:length(files)) {
    discipline <- read.csv(file=filepath[i]) # read first csv
    discbasename <- basename(filepath[i]) # get name of discipline from filename
    q <- nchar(discbasename) # the next three commands get rid of the ".csv" and return the discipline name. First get an integer value for the number of characters in the basename
    q <- q-4 # subtract 4 characters from that (".csv")
    discbasename <- substr(discbasename, 1, q) # get a character variable of the basename minus the ".csv"
    sbst <- subset(discipline, discipline$UnitArticles >= 1) # create subset of journals with >=1 citation from researchers in the unity
    newdf <- data.frame("JournalName"=toupper(sbst$JournalName), "UnitArticles"=sbst$UnitArticles, stringsAsFactors = FALSE) # create dataframe including only journal name and unit articles and convert to upper case
    oa <- newdf$Journal %in% doaj.list # create logical variable of journals matching DOAJ's list of open access journals as created in the above command
    if (any(oa) == T) { # if there are any matches, 
      jrns <- subset(newdf, oa) # out of that subset, return the journals that are oa & the unit article count for that journal
      oadf <- data.frame(jrns$JournalName, jrns$UnitArticles, rep(discbasename,length(jrns$JournalName)), stringsAsFactors = F) # add to the dataframe
      names(oadf) <- names(df) # change column names so rbind will work
      df <- rbind(df, oadf)
    } else { # if there are no T values (i.e. no OA publications)
      oadf <- data.frame(NA,NA,discbasename) # put NA values into dataframe
      names(oadf) <- names(df)
      df <- rbind(df, oadf)
    }  
  }
  return(df)
}

depts <- oa.journals("Departments")