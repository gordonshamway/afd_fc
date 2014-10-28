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
df$ANZ_AUFTRAEGE <- 0
#get weekday
df$Tag <- weekdays(df$timeline)
#set weekend = 0
df[!df$Tag %in% c("Samstag", "Sonntag"),]$ANZ_AUFTRAEGE <- as.integer(ts.arima$mean)
names(df) <- c("Einspieldatum", "Tag", "Anz_Auftraege")

#build historical window with weekend
final <- tail(raw, 10)
startdate <- min(final$EINSPIELDATUM)
enddate <- max(final$EINSPIELDATUM)
timeline = seq(as.Date(startdate), as.Date(enddate), "days")
final.frame <- data.frame(timeline)
names(final.frame) <- "EINSPIELDATUM"
final.frame$Tag <- weekdays(final.frame$EINSPIELDATUM)
final.frame$Anz_Auftraege <- 0
final.frame[!final.frame$Tag %in% c("Samstag", "Sonntag"),]$Anz_Auftraege <- final$ANZ_AUFTRAEGE

#combine those two
names(final.frame) <- c("Einspieldatum", "Tag", "Anz_Auftraege")
total <- rbind(final.frame, df)


#make a nice plot
library(ggplot2)
ggplot(data=total, aes(x=Einspieldatum, y=Anz_Auftraege)) +
      geom_line() +
      geom_point() +
      ggtitle("AFD Forecast") +
      ylab("Anzahl eingespielte Aufträge")
