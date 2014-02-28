setwd("J:/Escience/Academic Analytics")

Totals <- function(directory) {
  dirct <- file.path(getwd(), "data", "2014-02-02", "RC", directory) # set path to folder with CSV files
  files <- list.files(dirct) # list files in directory
  filepath <- file.path(dirct, files) # path of files
  df <- data.frame(matrix(nrow=0, ncol=4)) # create empty data frame
  names(df) <- c("Author", "Publisher", "ArticleTitle", "Department") # name dataframe columns
  for (i in 1:length(files)) {
    department <- read.csv(file=filepath[i], na.strings="") # read first csv, use na.strings to put NA values in for blank values
    dp <- department[complete.cases(department$dc.type.en_US.),] # remove NA values from Type field
    if (any(dp$dc.type.en_US. == "Article")) { # if there are any values of Article in the Type field
      dp <- dp[dp$dc.type.en_US. == "Article", ] # create a subset of the df with type Article
      dp$dc.type.en_US. <- as.character(dp$dc.type.en_US.) # Convert that vector to character (rather than drop the factors)
      discbasename <- basename(filepath[i]) # get name of discipline from filename
      q <- nchar(discbasename) # the next three commands get rid of the .csv and extract the discipline name (this would have been unnecessary if there were a consist variable for department)
      q <- q-4
      discbasename <- substr(discbasename, 1, q)
      newdf <- data.frame(dp$dc.contributor.author, dp$dc.publisher.en_US., dp$dc.title.en_US.,  discbasename) # a new dataframe extracting values from that subset
      names(newdf) <- names(df) # change column names so rbind will work
      df <- rbind(df, newdf)
    }
    if (any(names(department) == "dc.type.en."))  { # If any of the variables are called "dc.type.en."
      dp <- department[complete.cases(department$dc.type.en.),] # remove NA values from Type field
      if (any(dp$dc.type.en. == "Article")) { # if there are any values of Article in this Type field
        dp <- dp[dp$dc.type.en. == "Article", ] # create a subset of the df with type Article
        dp$dc.type.en. <- as.character(dp$dc.type.en.) # Convert that vector to character (rather than drop the factors)
        discbasename <- basename(filepath[i]) # get name of discipline from filename
        q <- nchar(discbasename) # the next three commands get rid of the .csv and extract the discipline name (this would have been unnecessary if there were a consist variable for department)
        q <- q-4
        discbasename <- substr(discbasename, 1, q)
        newdf <- data.frame(dp$dc.contributor.author, dp$dc.publisher.en_US., dp$dc.title.en_US.,  discbasename) # a new dataframe extracting values from that subset
        names(newdf) <- names(df) # change column names so rbind will work
        df <- rbind(df, newdf)
      }
    }
}
  return(df)
}

depts.ir <- Totals("RCDepartments") # return a dataframe

library(ggplot2)
## plot total articles
ircounts <- as.data.frame(table(depts.ir$Department)) # get total counts of the Department factor and put it in a dataframe (it acts as a proxy for submissions)
names(ircounts) <- c("Department", "ArticlesInIR") # rename columns
ircounts$departments.ordered <- reorder(ircounts$Department, ircounts$ArticlesInIR) #sort Discipline by Articles Published
pth <- file.path("J:/Escience/Academic Analytics/results/2014-02-26/plots/RC") # create a path to save articles in
depts.ir.plot <- ggplot(data=ircounts) +
  geom_bar(aes(x=departments.ordered,y=ArticlesInIR),fill="green",color="black",stat="identity") +
  coord_flip() +
  geom_text(aes(x=Department, y=ArticlesInIR, label=ArticlesInIR), hjust = -0.5, size=6) + #set text labels
  ggtitle(label="Total Articles Deposited in ResearchCommons by Department") +
  ylab("Number of Articles Deposited") +
  xlab("Department") +
  theme(text = element_text(size=20))
print(depts.ir.plot)
ggsave(filename="IR Deposits.png", plot=depts.ir.plot, path=pth, height=10, width=10) #save files 


##plot publishers
depts.ir.list <- unique(depts.ir$Department) # create list of departments with oa publications
#Loop through depts.ir dataframe, creating graph of publishers for each department and saving it to file
for(i in seq(length(depts.ir.list))) { #looping through the depts (seq must be used because it is a list)
  sbst <- depts.ir[depts.ir$Department == depts.ir.list[i], ] # create a subset of depts where the discipline is equal to discipline list item i
  sbst <- data.frame(as.character(sbst$Publisher), stringsAsFactors = F)
  sbst <- as.data.frame(table(sbst))
  names(sbst) <- c("Publisher", "NumberItems")
  sbst$ordered <- reorder(sbst$Publisher, sbst$NumberItems) # create a variable in the sbst dataframe ordered by article counts
  graph <- ggplot(data=sbst) +
    geom_bar(aes(x=ordered, y=NumberItems), width=0.5, fill="blue", color="black", stat="identity") + # create barchart
    coord_flip() + # flip axis to create horizontal barchart
    geom_text(aes(x=Publisher, y=NumberItems, label=NumberItems, ymax=20), hjust = -1, size=6) + #set text labels
    scale_y_continuous(limits=c(0,40)) + # set limits for y axis (which is flipped, so represents Articles)
    ggtitle(label=(paste("Publishers of Items Deposited by",depts.ir.list[i]))) # give it a title
  ggsave(sprintf("%s.png", depts.ir.list[i]), path=pth, width=10, height=10) #save files 
}

#write CSVs
write.csv(depts.ir, file=file.path(getwd(), "results", "2014-02-26", "tables", "RC", "depts.ir.csv"))

