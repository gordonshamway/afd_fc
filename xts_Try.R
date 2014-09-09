# trying things with zoo

library(xts)
raw <- read.csv("./afd_zeiten.csv", sep=";")

#converting into Date
raw$EINSPIELDATUM <- as.Date(raw$EINSPIELDATUM, format="%d.%m.%Y")

#so carrying the last value forward will work with this command
#source: http://stackoverflow.com/questions/1252546/how-to-fill-empty-values-in-a-data-frame-with-neighbouring-values
-> na.locf(x)

startdate <- start(zo)
enddate <- end(zo)
timeline = seq(as.Date(startdate), as.Date(enddate), "days")
df <- data.frame(timeline)
names(df) <- "EINSPIELDATUM"
#join the dates from the timeline with the sql submitted data to build a dataframe with 0 values
merged <- merge(x = df, y = raw, by = "EINSPIELDATUM", all.x=TRUE)

#impute all the NAs with the value before
merged$ANZ_AUFTRAEGE <-na.locf(merged$ANZ_AUFTRAEGE)

zo <- xts(merged$ANZ_AUFTRAEGE, order.by=merged$EINSPIELDATUM)


#weil es auch mit xts nicht vernünftig ging jetzt wieder ein versuch mit 365 tagen, nachdem imputed wurde.
y = ts(merged$ANZ_AUFTRAEGE, start=c(2013, 244), frequency=364)

#create model
library(forecast)
fit <- auto.arima(y)
fc <- forecast(fit, h=5)
plot(fc, axes=TRUE,)
#DOESNT WORK PROPERLY EITHER


as.Date(247.105, origin="2014-01-01")
as.Date(255.865, origin="2014-01-01")
#with xts it is the best so far, except that the forecast always predicts for the whole time including
#weekends, so the only way to handle those situation is to impute the values for the missing 
# holidays and weekends