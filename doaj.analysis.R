library(stringr)

#get a list of main and alternative DOAJ titles in the AA list
doaj.main <- function(doajlistdata) {
  doaj.main.titles <- as.character(doajlistdata$Title) # get list of DOAJ titles
  doaj.main.titles <- str_trim(factor(toupper(doaj.main.titles)), side = "both") # trim extra spaces on doaj list
  return(doaj.main.titles)
}
doaj.main.list <- doaj.main(doaj)

doaj.main.replaced <- 

doaj.alt.trimmed <- function(doajlistdata) {
  doaj.alt.titles <- as.character(doajlistdata$Title.Alternative) # get list of DOAJ titles
  doaj.alt.titles <- doaj.alt.titles[complete.cases(doaj.alt.titles)] #There are over 2,000 here, but running an intersect with AA only finds 30. Unfortunately, none of these are in this analysis.
  doaj.alt.titles.trimmed <- str_trim(factor(toupper(doaj.alt.titles)), side = "both") # trim extra spaces on doaj list
  return(doaj.alt.titles)
}
doaj.alt.list.trimmed <- doaj.alt(doaj)

doaj.alt.replaced <- function(doajlistdata) {
  doaj.alt.titles <- as.character(doajlistdata$Title.Alternative) # get list of DOAJ titles
  doaj.alt.titles <- doaj.alt.titles[complete.cases(doaj.alt.titles)] #There are over 2,000 here, but running an intersect with AA only finds 30. Unfortunately, none of these are in this analysis.
  doaj.alt.titles.replaced <- str_replace_all(factor(toupper(doaj.alt.titles)), side = "both", pattern = " ", repl="") # trim extra spaces on doaj list
  return(doaj.alt.titles)
}
doaj.alt.list.replaced <- doaj.alt.replaced(doaj)

clean.aa <- function(aadata) {
  # Get a list of all journals indexed by Academic Analytics. This is used later in an intersection function to analyze which DOAJ titles have punctuation problems inside the string
  # 
  # Arg: The raw Academic Analytics journal data
  # 
  # Returns: A character string of 14,586 Academic Analytics data, capitalized and with duplicates eliminated
  
  aa.titles <- data.frame("Title"=str_replace_all(factor(toupper(aadata$AAD.2011.Journal.List)), pattern = " ", repl = "")) # get list of AA titles
  #aa.titles <- data.frame("Title"=str_replace_all(aa.titles$Title, pattern = "[[:punct:]]", repl = ""))
  dupe <- duplicated(aa.titles) # logical vector of duplicates
  aa.list <- aa.titles[!dupe,] # return all AA journals as characters, in caps, without duplicates (14,586)
  aa.list <- as.character(aa.list) # convert from factor to character string
  #aa.list.dupes <- aa.titles[dupe] # return all duplicated journals from the AA list (203,883)
  return(aa.list)
}
aa.list.repl2 <- clean.aa(aa.journals)

clean.aa <- function(aadata) {
  # Get a list of all journals indexed by Academic Analytics. This is used later in an intersection function to analyze which DOAJ titles have punctuation problems inside the string
  # 
  # Arg: The raw Academic Analytics journal data
  # 
  # Returns: A character string of 14,586 Academic Analytics data, capitalized and with duplicates eliminated
  
  aa.titles <- data.frame("Title"=str_trim(factor(toupper(aadata$AAD.2011.Journal.List)))) # get list of AA titles
  dupe <- duplicated(aa.titles) # logical vector of duplicates
  aa.list <- aa.titles[!dupe,] # return all AA journals as characters, in caps, without duplicates (14,586)
  aa.list <- as.character(aa.list) # convert from factor to character string
  #aa.list.dupes <- aa.titles[dupe] # return all duplicated journals from the AA list (203,883)
  return(aa.list)
}
aa.list.trim <- clean.aa(aa.journals)




aa.repl <- str_replace_all(aa.list, pattern = " ", repl = "")

clean.doaj.main <- function(doajdata) {
  # Get a list of all DOAJ journals and their publishers with whitespace trimmed
  #
  # Arg: The raw DOAJ journal data
  #
  # Returns: A dataframe of DOAJ journals, capitalized, and their publishers, capitalized, with duplicates eliminated
  
  doaj.titles <- data.frame("Title"=str_replace_all(factor(toupper(doajdata$Title)), pattern = " ", repl=""), "Publisher"=str_trim(factor(toupper(doajdata$Publisher)), side = "both")) # get list of DOAJ titles
  doaj.titles <- data.frame("Title"=str_replace_all(factor(toupper(doaj.titles$Title)), pattern = "[[:punct:]]", repl=""), "Publisher"=doaj.titles$Publisher) # get list of DOAJ titles
  dupe <- duplicated(doaj.titles) # logical vector of duplicates
  doaj.list <- doaj.titles[!dupe,] # return all DOAJ titles as characters, in caps, without duplicates (9,786)
  doaj.list$Title <- as.character(doaj.list$Title) # Convert Title variable to character from factor
  return(doaj.list)
}
doaj.list.repl <- clean.doaj.main(doaj)



doaj.repl <- str_replace_all(doaj.main.list, pattern = " ", repl="")




# x=y so there are no punctuation issues with the DOAJ Alternative title list with respect to the AA list
x <- intersect(doaj.alt.list.trim$Title, aa.list)
y <- intersect(doaj.alt.list.repl$Title, aa.repl)


xx <- intersect(doaj.main.list, aa.list.trim)
z <- intersect(doaj.list.repl$Title, aa.list.repl)
zz <- intersect(doaj.main.list)



aa.doaj.main <- intersect(doaj.main.list, aa.list)
aa.doaj.alt <- intersect(doaj.alt.list, aa.list)

# Test to see if any 
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
  df <- data.frame(matrix(nrow=0, ncol=3)) # create empty data frame
  names(df) <- c("Journal", "ArticlesPublished", "Discipline") # name dataframe columns
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
    oa <- newdf$Journal %in% doaj.alt.list.trimmed # create logical variable of journals matching DOAJ's list of open access journals as created in the above command
    # run the first loop with the leading/trailing whitespace removed
    if (any(oa) == T) { # if there are any matches, 
      jrns <- subset(newdf, oa) # out of that subset, return the journals that are oa & the unit article count for that journal
      oadf <- data.frame("Title"=jrns$JournalName, jrns$UnitArticles, rep(discbasename,length(jrns$JournalName)), stringsAsFactors = F) # add to the dataframe
      #oadf <- merge(x = oadf, y = doaj.alt.list, by.x="Title", by.y="Title", all.x = TRUE) # join the publisher list to the newly created dataframe
      names(oadf) <- names(df) # change column names so rbind will work
      df <- rbind(df, oadf)
    } else { # if there are no T values (i.e. no OA publications)
      oadf <- data.frame(NA,NA,discbasename) # put NA values into dataframe
      names(oadf) <- names(df)
      df <- rbind(df, oadf)
    }
    # run a second loop with ALL whitespaces removed (using missing$Title)
    Replaced.Journal.Name <- str_replace_all(sbst$JournalName, pattern = " ", repl="") # Delete all spaces in department publication titles
    Replaced.Journal.Name <- toupper(Replaced.Journal.Name) # Convert journal name to uppercase
    newdf <- data.frame("JournalName" = Replaced.Journal.Name, "UnitArticles"=sbst$UnitArticles, stringsAsFactors = FALSE) # create dataframe including only journal name and unit articles and convert to upper case
    oa <- newdf$Journal %in% doaj.alt.list.replaced # create logical variable of journals matching the department's publications with the journals that have spacing issues
    if (any(oa) == T) { # if there are any matches, 
      jrns <- subset(newdf, oa) # out of that subset, return the journals that are oa & the unit article count for that journal
      oadf <- data.frame("Title"=jrns$JournalName, jrns$UnitArticles, rep(discbasename,length(jrns$JournalName)), stringsAsFactors = F) # add to the dataframe
      #oadf <- merge(x = oadf, y = missing, all.x = TRUE) # join the publisher list from the missing dataframe to the just-created dataframe
      names(oadf) <- names(df) # change column names so rbind will work
      df <- rbind(df, oadf)
    }
  }
  return(df)
}

deptsy <- disciplineOA("AADepartments")






#Tabulate by publisher
doaj.publisher.table <- as.data.frame(table(doaj.list$Publisher)) # Get the Freq for Type
names(doaj.publisher.table) <- c("Publisher", "Count")
doaj.publisher.table <- doaj.publisher.table[order(-doaj.publisher.table$Count),] #order by publisher
pubtop20 <- doaj.publisher.table[1:20,]

#plot complete publishers (ggplot)
pubtop20$Pubs.ordered <- reorder(pubtop20$Publisher, pubtop20$Count) #sort Publisher by Count
pth <- pth <- file.path(getwd(), "results", "2014-02-26", "plots", "DOAJ") # set a location for plots to be saved
doaj.pubs.plot <- ggplot(data=pubtop20) +
  geom_bar(aes(x=Pubs.ordered,y=Count),fill="yellow",color="black",stat="identity") +
  coord_flip() +
  geom_text(aes(x=Pubs.ordered, y=Count, label=Count), hjust = -0.5, size=6) + #set text labels
  ggtitle(label="Total Journal Counts Indexed in DOAJ by Publisher, Top 20") +
  ylab("Number of Journals Indexed in DOAJ") +
  xlab("Publisher") +
  theme(text = element_text(size=20))
ggsave("doaj.pubs.plot.png", path=pth, width=15, height=15) #save files 
