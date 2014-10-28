# Read in the data
library(xts)
raw <- read.csv("./afd_zeiten.csv", sep=";")
#converting into Date
raw$EINSPIELDATUM <- as.Date(raw$EINSPIELDATUM, format="%d.%m.%Y")

#Create a Datenframe for the whole period to include weekends
startdate <- min(raw$EINSPIELDATUM)
enddate <- max(raw$EINSPIELDATUM)
timeline = seq(as.Date(startdate), as.Date(enddate), "days")
df <- data.frame(timeline)
names(df) <- "EINSPIELDATUM"

#join the dates from the timeline with the sql submitted data to build a dataframe with 0 values
merged <- merge(x = df, y = raw, by = "EINSPIELDATUM", all.x=TRUE)

#impute all the NAs with the value before
merged$ANZ_AUFTRAEGE <-na.locf(merged$ANZ_AUFTRAEGE)

# Trying to setting up the right Frequency for later forecasting
zo <- xts(merged$ANZ_AUFTRAEGE, order.by=merged$EINSPIELDATUM)

#test without weekend
zo <- xts(raw$ANZ_AUFTRAEGE, order.by=raw$EINSPIELDATUM)
#works better than with weekend

#TODO: Try to Change the TimeSeries to get better Results

#Evaluation of Algorithms
library(forecast)
ts.arima <- forecast(auto.arima(zo[,1]),5)
ts.tbats <- forecast(tbats(zo[,1]),5)
ts.ets <- forecast(ets(zo[,1]), 5)
ts.arima$mean
ts.tbats$mean
ts.ets$mean
#1st two algos are good

# build forecast dataset
startdate <- max(raw$EINSPIELDATUM)
timeline = seq(as.Date(startdate+1), as.Date(startdate+7), "days")
df <- data.frame(timeline)
names(df) <- "EINSPIELDATUM"
df$arima <- NA
df$tbats <- NA
#get weekday
df$Tag <- weekdays(df$timeline)
#set weekend = 0
df[!df$Tag %in% c("Samstag", "Sonntag"),]$arima <- as.integer(ts.arima$mean)
df[!df$Tag %in% c("Samstag", "Sonntag"),]$tbats <- as.integer(ts.tbats$mean)
names(df) <- c("Einspieldatum", "Tag", "arima", "tbats")

#build historical window with weekend
final <- tail(raw, 15)
startdate <- min(final$EINSPIELDATUM)
enddate <- max(final$EINSPIELDATUM)
timeline = seq(as.Date(startdate), as.Date(enddate), "days")
final.frame <- data.frame(timeline)
names(final.frame) <- "EINSPIELDATUM"
final.frame$Tag <- weekdays(final.frame$EINSPIELDATUM)
final.frame$Anz_Auftraege <- NA
final.frame[!final.frame$Tag %in% c("Samstag", "Sonntag"),]$Anz_Auftraege <- final$ANZ_AUFTRAEGE
names(final.frame) <- c("Einspieldatum", "Tag", "Anz_Auftraege")

#make full join on date
total <- merge(x = final.frame, y = df, by = "Einspieldatum", all=TRUE)
total$Tag.x <- NULL
total$Tag.y <- NULL

#make a nice plot
library(reshape2)
library(scales)
library(ggplot2)
mdf <- melt(total, id.vars="Einspieldatum", value.name="Anz_Auftraege", variable.name="Quelle")
ggplot(data=mdf, aes(Einspieldatum, Anz_Auftraege, color=Quelle)) + 
  geom_line(size=1) +
  geom_point(size=3, fill="white") + 
  scale_x_date(labels = date_format("%d.%m.%Y"), breaks = date_breaks("days")) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("AFD Forecast") +
  theme(plot.title = element_text(lineheight=.8, face="bold"))
  ylab("Anzahl eingespielte Aufträge")