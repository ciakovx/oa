#get subset of depts with OA
depts.compl <- depts[complete.cases(depts),]

#get list of depts without OA
issna <- is.na(depts$Journal) #create logical vector of NA=TRUE
depts.incompl <- depts$Discipline[issna] #subset list of depts by that vector


#Create dataframe of total articles published by discipline
ArtCounts <- tapply(depts.compl$ArticlesPublished,depts.compl$Discipline,sum) #create table of sums of articles applied to the ArticlesPublished column
ArtCounts <- data.frame("ArticlesPublished"=ArtCounts[complete.cases(ArtCounts)]) #create dataframe of that table
ArtCounts <- data.frame("Discipline"=as.character(rownames(ArtCounts)), "ArticlesPublished"=as.integer(ArtCounts$ArticlesPublished)) #make the rownames of that df into a variable


library(ggplot2) # Load ggplot package


#plot complete depts (ggplot)
ArtCounts$Journals.ordered <- reorder(ArtCounts$Discipline, ArtCounts$ArticlesPublished) #sort Discipline by Articles Published
pth <- pth <- file.path(getwd(), "results", "2014-02-26", "plots") # set a location for plots to be saved
compl.depts.plot <- ggplot(data=ArtCounts) +
  geom_bar(aes(x=Journals.ordered,y=ArticlesPublished),fill="orange",color="black",stat="identity") +
  coord_flip() +
  geom_text(aes(x=Discipline, y=ArticlesPublished, label=ArticlesPublished), hjust = -0.5, size=6) + #set text labels
  ggtitle(label="Total Article Counts in Open Access Publications by Department, UTA 2004-2011") +
  ylab("Number of Articles Published") +
  xlab("Department") +
  theme(text = element_text(size=20))
ggsave("AllDepts.png", path=pth, width=15, height=15) #save files 


#write CSVs
write.csv(depts.compl, file=file.path(getwd(), "results", "2014-02-26", "tables", "depts.compl.csv"))
write.csv(depts.incompl, file=file.path(getwd(), "results", "2014-02-26", "tables", "depts.incompl.csv"))
write.csv(ArtCounts, file=file.path(getwd(), "results", "2014-02-26", "tables", "artcounts.csv"))