# This creates a series of PNGs and puts them in a folder.
# The JPGs are OA publications by discipline.
# Make sure you set the pth variable to the folder you want the JPGs in.

library(ggplot2) # Load ggplot package



##First create dataframe of complete cases & from that, a list of departments
  depts.compl <- depts[complete.cases(depts), ] # subset of depts with only complete cases (remove NAs)
  deptslist <- unique(depts.compl$Discipline) # create list of departments with oa publications
  pth <- file.path(getwd(), "results", "2014-03-08", "plots") # set a location for plots to be saved
#Loop through depts.compl dataframe, creating graph for each discipline and saving it to file
  for(i in seq(length(deptslist))) { #looping through the depts (seq must be used because it is a list)
      sbst <- depts.compl[depts.compl$Discipline == deptslist[i], ] # create a subset of depts where the discipline is equal to discipline list item i
      sbst$ordered <- reorder(sbst$Journal, sbst$ArticlesPublished) # create a variable in the sbst dataframe ordered by article counts
      graph <- ggplot(data=sbst) +
        geom_bar(aes(x=ordered, y=ArticlesPublished), width=0.5, fill="red", color="black", stat="identity") + # create barchart
        coord_flip() + # flip axis to create horizontal barchart
        geom_text(aes(x=Journal, y=ArticlesPublished, label=ArticlesPublished, ymax=20), hjust = -1, size=6) + #set text labels
        scale_y_continuous(limits=c(0,35)) + # set limits for y axis (which is flipped, so represents Articles)
        ggtitle(label=(paste("Open Access Journal Publications in UTA",deptslist[i], "Department, 2004-2011"))) # give it a title
      ggsave(sprintf("%s.png", deptslist[i]), path=pth, width=15, height=15) #save files 
  }
}

  
#this can be done discipline by discipline as well:
  elec <- subset(depts,Discipline == "Electrical Engineering") # subset of depts for discipline in question
  elec <- data.frame("Journal"=elec$Journal,"ArticlesPublished"=elec$ArticlesPublished, stringsAsFactors = F)
  elec$Journals.ordered <- reorder(elec$Journal, elec$ArticlesPublished)
    graph <- ggplot(data=elec) +
    geom_bar(aes(x=Journals.ordered, y=ArticlesPublished), fill="red", color="black", stat="identity") +
    coord_flip() +
    scale_y_continuous(limits=c(0,40)) +
    geom_text(aes(x=Journal, y=ArticlesPublished, label=ArticlesPublished, ymax=20), hjust = -1, size=6) +
    ggtitle(label=(paste("Open Access Journal Publications in UTA",elec$Discipline,"Department, 2004-2011")))
  print(graph)
