#get a list of DOAJ titles as character
doaj.titles <- data.frame("Title"=doaj$Title, "Publisher"=doaj$Publisher) # get list of DOAJ titles
doaj.titles$Title <- str_trim(factor(toupper(doaj.titles$Title)), side = "both") # trim extra spaces on doaj list
doaj.titles$Publisher <- str_trim(factor(toupper(doaj.titles$Publisher)), side = "both") # trim extra spaces on doaj list
dupe <- duplicated(doaj.titles) # logical vector of duplicates
doaj.list <- doaj.titles[!dupe,] # return all DOAJ titles as characters, in caps, without duplicates (9,786)
doaj.dupes <- doaj.titles[dupe,]





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
