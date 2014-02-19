#get subset of depts with OA
depts.compl <- depts[complete.cases(depts),]

#get list of depts without OA
issna <- is.na(depts$Journal) #create logical vector of NA=TRUE
depts.incompl <- depts$Discipline[issna] #subset list of depts by that vector


#Create dataframe of total articles published by discipline
ArtCounts <- tapply(depts.compl$ArticlesPublished,depts.compl$Discipline,sum) #create table of sums of articles applied to the ArticlesPublished column
ArtCounts <- data.frame("ArticlesPublished"=ArtCounts[complete.cases(ArtCounts)]) #create dataframe of that table
ArtCounts <- data.frame("Discipline"=as.character(rownames(ArtCounts)), "ArticlesPublished"=as.integer(ArtCounts$ArticlesPublished)) #make the rownames of that df into a variable


#plot complete depts (ggplot)
ArtCounts$Journals.ordered <- reorder(ArtCounts$Discipline, ArtCounts$ArticlesPublished) #sort Discipline by Articles Published
pth <- pth <- file.path(getwd(), "Plots", "plots4") # set a location for plots to be saved
compl.depts.plot <- ggplot(data=ArtCounts) +
  geom_bar(aes(x=Journals.ordered,y=ArticlesPublished),fill="orange",color="black",stat="identity") +
  coord_flip() +
  geom_text(aes(x=Discipline, y=ArticlesPublished, label=ArticlesPublished), hjust = -0.5, size=6) + #set text labels
  ggtitle(label="Total Article Counts in Open Access Publications by Department, UTA 2004-2011")
ggsave("AllDepts.png", path=pth, width=15, height=15) #save files 


#plot depts color-coded by journal (ggplot)
# this needs some work, but I'm not going to do it. The colors are wrong. It's too stacked to look good anyway.
discplot <- ggplot(data=depts.compl) + 
  geom_bar(aes(x=Discipline, y=ArticlesPublished, fill=factor(depts.compl$Journal))) +
  coord_flip() + 
  scale_fill_discrete(breaks=depts.compl$Journal)
print(discplot)

#return df oa journals single discipline
function(discipline) {
  depts.compl <- depts[complete.cases(depts),] #subset of depts with only complete cases (remove NAs)
  sbst <- depts.compl[depts.compl$Discipline == discipline,]
  return(sbst)
}

