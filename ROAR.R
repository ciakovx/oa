setwd("C:/Users/clarke/Desktop/OA Research Project/ROAR") #set working directory
roar <- read.csv(file=file.path(getwd(), "roarmap.csv"), na.strings="") # read in data


# count and plot mandate types by freq
roarna <- data.frame("type"=roar$type[complete.cases(roar$type)]) # get rid of NAs
roartypes <- data.frame(table(roarna$type)) # create table of counts
names(roartypes) <- c("type", "freq") # rename columns
roartypes$ordered <- reorder(roartypes$type, roartypes$freq) # insert vector of ordered counts
roartypes.plot <- ggplot(data=roartypes) +
  geom_bar(aes(x=ordered,y=freq),fill="black",color="black",stat="identity") +
  coord_flip() +
  geom_text(aes(x=ordered, y=freq, label=freq), hjust = -0.5, size=6) + #set text labels
  ggtitle(label="Types of Mandates in American Institutions as indexed by ROARMAP") +
  ylab("Frequency") +
  xlab("Type of Mandate") +
  theme(text = element_text(size=20)) 
ggsave("MandateTypes.png", width=15, height=15) #save files 


