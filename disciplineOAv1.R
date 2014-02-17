#first 
setwd("C:/Users/clarke/Desktop/AcademicAnalyticsData") #set working directory
#then read in 
doaj <- read.csv(file="C:/Users/clarke/Desktop/AcademicAnalyticsData/DOAJ.csv") # import directory of OA journals file

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
    q <- nchar(discbasename) # the next three commands get rid of the .csv and extract the discipline name
    q <- q-4
    discbasename <- substr(discbasename, 1, q)
    sbst <- subset(discipline, discipline$UnitArticles >= 1) # create subset of journals with >=1 citation from researchers in the unity
    newdf <- data.frame(sbst$JournalName, sbst$UnitArticles) # create dataframe including only journal name and unit articles
    oa <- newdf$sbst.JournalName %in% doaj$Title # create logical variable of journals matching DOAJ's list of open access journals
    if (any(oa) == T) { # if there are any matches, 
      jrns <- subset(newdf, oa) # out of that subset, return the journals that are oa & the unit article count for that journal
      oadf <- data.frame(as.character(jrns$sbst.JournalName), jrns$sbst.UnitArticles, rep(discbasename,length(jrns$sbst.JournalName)), stringsAsFactors = F) # add to the dataframe -must coerce journal names to character!
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