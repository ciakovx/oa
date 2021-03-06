# Experimenting with finding spaces screwing up the matches



# Second attempt at finding intersection: deleting leading/trailing white space:
doaj.trim <- str_trim(doaj.list, side = "both") # trim extra spaces on doaj list
aa.trim <- str_trim(aa.list, side = "both") # trim extra spaces on aa list
doaj.aa.trim <- intersect(doaj.trim,aa.trim) # length is 1199. compare this to ok, difference of 27. That means 27 journals have spacing issues OTHER THAN leading/trailing white space.

# Third attempt at finding intersection: deleting all spaces.
doaj.repl <- str_replace_all(doaj.list, pattern = " ", repl="") # delete ALL spaces on doaj list
aa.repl <- str_replace_all(aa.list, pattern = " ", repl="") # delete ALL spaces on aa list
doaj.aa.repl <- intersect(doaj.repl, aa.repl) # length is 1226. This is your desired grand total.

aadoajstr <- str_replace_all(aa.doaj, pattern = " ", repl="") # This was your first final list. It was missing 109 titles.

#What are the 27 journals?
doaj.aa.trim.repl <- str_replace_all(doaj.aa.trim, pattern = " ", repl="") # delete ALL spaces on okk list
all.repl <- doaj.aa.repl %in% doaj.aa.trim.repl # get all values in ok that are in okk
missing <- doaj.aa.repl[!all.repl] # here are the 27 missing journals: all with colons, punctuation, etc.! Problem is, you don't know if these are messed up in the DOAJ or AA list--but probably the DOAJ list.
# So rather than go through and screw with everything, I'm going to write a second function that deletes all spaces, and looks just for these.


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
    Clean.Journal.Name <- str_replace_all(sbst$JournalName, pattern = " ", repl="") # Delete leading and trailing spaces
    Clean.Journal.Name <- toupper(Clean.Journal.Name) # Convert journal name to uppercase
    newdf <- data.frame("JournalName" = Clean.Journal.Name, "UnitArticles"=sbst$UnitArticles, stringsAsFactors = FALSE) # create dataframe including only journal name and unit articles and convert to upper case
    oa <- newdf$Journal %in% missing # create logical variable of journals matching DOAJ's list of open access journals as created in the above command
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

depts.trimmed <- oa.journals("Departments")

# ADVANCESINNATURALSCIENCES:NANOSCIENCEANDNANOTECHNOLOGY for Electrical Engineering is the only entry on this list.