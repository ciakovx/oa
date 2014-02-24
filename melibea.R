setwd("C:/Users/clarke/Desktop/OA Research Project/ROAR") #set working directory
melibea <- read.csv(file=file.path(getwd(), "MELIBEA.csv"), na.strings="") # read in data




melibea.req.plot <- ggplot(data=melibea) + #plots request v. requirement
  geom_bar(aes(Policy))
print(melibea.req)


melibea.types.plot <- ggplot(data=melibea) + #plot by when deposit is required
  geom_bar ((aes(Policy, fill=When)))
print(melibea.types.plot)

melibea.types.plot <- ggplot(data=melibea) + #plot by where deposit is required
  geom_bar ((aes(Policy, fill=Where)))
print(melibea.types.plot)




melibea$Date.formatted <- as.Date(melibea$Effective.date, format="%m/%d/%Y") # coerce dates into date and out of factor format

mandate.dates <- data.frame(table(melibea$Date.formatted)) # dataframe of effective dates with frequencies (for time series plot)
names(mandate.dates) <- c("EffectiveDate", "freq")

melibea.dates.plot <- ggplot(data=melibea) #in process time series plot
  geom_line(aes(x=Date.formatted)) +
  scale_x_date(format = "%b-%Y") + xlab("") + ylab("Daily Views")
print(melibea.dates.plot)

