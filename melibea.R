setwd("J:/Escience/Academic Analytics") #set working directory
melibea <- read.csv(file=file.path(getwd(), "data", "2014-02-02", "Melibea", "MELIBEA.csv"), na.strings="") # read in data

pth <- file.path(getwd(), "results", "2014-02-26", "plots", "Melibea")

melibea.req.plot <- ggplot(data=melibea) + #plots request v. requirement
  geom_bar(aes(Policy))
print(melibea.req.plot)
ggsave("melibea.policy.plot.png", path=pth, width=10, height=10) #save files 


melibea.types.plot <- ggplot(data=melibea) + #plot by when deposit is required
  geom_bar ((aes(Policy, fill=When)))
print(melibea.types.plot)
ggsave("melibea.policy.when.png", path=pth, width=10, height=10) #save files 


melibea.types.plot <- ggplot(data=melibea) + #plot by where deposit is required
  geom_bar ((aes(Policy, fill=Where)))
print(melibea.types.plot)
ggsave("melibea.policy.where.png", path=pth, width=10, height=10) #save files 





melibea$Date.formatted <- as.Date(melibea$Effective.date, format="%m/%d/%Y") # coerce dates into date and out of factor format

mandate.dates <- data.frame(table(melibea$Date.formatted)) # dataframe of effective dates with frequencies (for time series plot)
names(mandate.dates) <- c("EffectiveDate", "freq")

melibea.dates.plot <- ggplot(data=melibea) #in process time series plot
  geom_line(aes(x=Date.formatted)) +
  scale_x_date(format = "%b-%Y") + xlab("") + ylab("Daily Views")
print(melibea.dates.plot)

