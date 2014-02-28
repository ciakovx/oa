aa.titles <- data.frame(aa.journals$AAD.2011.Journal.List) # get list of AA titles
aa.titles <- factor(aa.titles$aa.journals.AAD.2011.Journal.List) # convert to factor
aa.titles <- str_trim(aa.titles, side = "both") # trim extra spaces on aa list
aa.titles <- toupper(aa.titles) # convert to upper case
dupe.b <- duplicated(aa.titles) # logical vector of duplicates
aa.list <- aa.titles[!dupe.b] # return all AA journals as characters, in caps, without duplicates (14,586)
aa.list.dupes <- aa.titles[dupe.b] # return all duplicated journals from the AA list (203,883)

scopus.titles <- data.frame(scopus$Title) # get list of Scopus titles
scopus.titles <- factor(scopus$Title) # convert to factor
scopus.titles <- str_trim(scopus.titles, side = "both") # trim extra spaces on doaj list
scopus.titles <- toupper(scopus.titles) # convert to upper case
dupe.c <- duplicated(scopus.titles) # logical vector of duplicates
scopus.list <- scopus.titles[!dupe.c] # return all Scopus journals as characters, in caps, without duplicates (34,275)
scopus.list.dupes <- scopus.titles[dupe.c] # return all duplicated journals from the Scopus list (156)



# Updated totals after applying trim
aa.scopus <- intersect(aa.list, scopus.list) # intersection of AA & Scopus (10,745) #the same: this proves it was DOAJ


aa.scopus2 <- aa.list %in% scopus.list
aa.scopus3 <- scopus.list %in% aa.list

w <- x %in% doaj.list


x <- aa.list[aa.scopus2] # Out of the 14,586 journals in AA, 10,809 are in Scopus 
x1 <- aa.list[!aa.scopus2] # And 3,777 are not

q <- merge(x, scopus$Publisher)


x <- as.data.frame(x)
names(x) <- "Journal"
for (i in seq(x)) {
  if (x$Journal == )
  df <- data.frame(x$Journal)
}

