#get subset of depts with & without OA
depts.compl <- depts[complete.cases(depts),]
depts.incompl <- depts[!complete.cases(depts),]

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
print(compl.depts.plot)
ggsave("AllDepts.png", path=pth, width=15, height=15) #save files 


#plot top 20 publishers (ggplot)
#Tabulate by publisher
PubCounts <- tapply(depts.compl$ArticlesPublished,depts.compl$Publisher,sum) #create table of sums of articles applied to the ArticlesPublished column
PubCounts <- data.frame("ArticlesPublished"=PubCounts[complete.cases(PubCounts)]) #create dataframe of that table
PubCounts <- data.frame("Publisher"=as.character(rownames(PubCounts)), "ArticlesPublished"=as.integer(PubCounts$ArticlesPublished)) #make the rownames of that df into a variable

#plot complete publishers (ggplot)
PubCounts$Pubs.ordered <- reorder(PubCounts$Publisher, PubCounts$ArticlesPublished) #sort Publisher by Count
#pth <- pth <- file.path(getwd(), "results", "2014-02-26", "plots", "DOAJ") # set a location for plots to be saved
depts.pubs.plot <- ggplot(data=deptspubtop20) +
  geom_bar(aes(x=Pubs.ordered,y=Count),fill="yellow",color="black",stat="identity") +
  coord_flip() +
  geom_text(aes(x=Pubs.ordered, y=Count, label=Count), hjust = -0.5, size=6) + #set text labels
  ggtitle(label="Total Journal Counts Indexed in DOAJ by Publisher, Top 20") +
  ylab("Number of Journals Indexed in DOAJ") +
  xlab("Publisher") +
  theme(text = element_text(size=20))
print(depts.pubs.plot)
ggsave("doaj.pubs.plot.png", path=pth, width=15, height=15) #save files 











#write CSVs
write.csv(depts.compl.x, file=file.path(getwd(), "results", "2014-02-26", "tables", "depts.compl.x.csv"))
write.csv(depts.incompl, file=file.path(getwd(), "results", "2014-02-26", "tables", "depts.incompl.csv"))
write.csv(ArtCounts, file=file.path(getwd(), "results", "2014-02-26", "tables", "artcounts.csv"))