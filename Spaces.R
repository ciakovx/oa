# Experimenting with finding spaces screwing up the matches



# Second attempt at finding intersection: deleting leading/trailing white space:
xxx <- str_trim(doaj.list, side = "both") # trim extra spaces on doaj list
yyy <- str_trim(aa.list, side = "both") # trim extra spaces on aa list
okk <- intersect(xxx,yyy) # length is 1199. compare this to ok, difference of 27. That means 27 journals have spacing issues OTHER THAN leading/trailing white space.

# Third attempt at finding intersection: deleting all spaces.
xx <- str_replace_all(doaj.list, pattern = " ", repl="") # delete ALL spaces on doaj list
yy <- str_replace_all(aa.list, pattern = " ", repl="") # delete ALL spaces on aa list
ok <- intersect(xx, yy) # length is 1226. This is your desired grand total.

aadoajstr <- str_replace_all(aa.doaj, pattern = " ", repl="") # This was your first final list. It was missing 109 titles.

#What are the 27 journals?
aa <- str_replace_all(okk, pattern = " ", repl="") # delete ALL spaces on okk list
aaa <- ok %in% aa # get all values in ok that are in okk
aaaa <- ok[!aaa] # here are the 27 missing journals: all with colons, punctuation, etc.! Problem is, you don't know if these are messed up in the DOAJ or AA list--but probably the DOAJ list.
# So rather than go through and screw with everything, I'm going to write a second function that deletes all spaces, and looks just for these.

