---
  title: "AFD_Forecast"
author: "Stefan Buchholz"
date: "Monday, June 23, 2014"
output: html_document
---
  
  einführendes blabal

```{r einlesen der Daten}
daten <- file.choose()
daten <- read.csv(daten, sep=";")
daten$EINSPIELDATUM <- as.Date(daten$EINSPIELDATUM, "%d.%m.%Y")

#Nochmal ohne Wochenenden
library(timeDate)
daten_ow <- daten[isWeekday(daten$EINSPIELDATUM),]
```

```{r Daten einmal plotten}
library(xts)
zoo_ts <- zoo(daten[,-1], order.by=daten$EINSPIELDATUM)
zoo_ts_ow <- zoo(daten_ow[,-1], order.by=daten_ow$EINSPIELDATUM)
plot(zoo_ts_ow)
#schönste Ausgabe
xts_ts <- xts(zoo_ts)
plot.xts(xts_ts)

as.weekly(zoo_ts_ow)

model <- auto.arima(xts_ts)
fc <- forecast(model, n=10)
plot(fc)

#Ausgabe ohne Wochenende
xts_ts_ow <-xts(zoo_ts_ow)
plot.xts(xts_ts_ow)
```

y <- msts(daten_ow, seasonal.periods=c(5,251.25), start=c(2013,9))
fit <- tbats(y)
fc <- forecast(fit,h=10)
plot(fc)

arima_model <- auto.arima(daten_ow)
arima_forecast <- forecast(arima_model, 10)

plot(forecast(daten_ow, robust=TRUE, h=20))
plot(forecast(ets(daten_ow, h=20)))
plot(forecast(auto.arima(daten_ow, h=20)))

plot(forecast(daten_ow))
plot(forecast(auto.arima(daten_ow)),xlim=c(200,300))
accuracy(forecast(auto.arima(daten_ow)))
plot(forecast(arima_model(daten_ow)))
daten_ow_ts <- ts(daten_ow, start=c(9,2013), frequency=5)

xts_daten <- xts(daten$ANZ_AUFTRAEGE, order.by=daten$EINSPIELDATUM)
wtl <- apply.weekly(xts_daten, sum)
test <- auto.arima(wtl)
plot(forecast(test, n=5))

names(wtl) <- "ANZ_AUFTRAEGE"

#mit taeglichen Veränderungen müsste ich die Wochenenden imputen, oder aber auf Wochen gehen um
#die X-Achse ordentlich beschriften zu können.
#es ging auch nicht mit wochentlichen daten, es scheint ein funktionsproblem zu sein.
require(lubridate)
y = ts(daten$ANZ_AUFTRAEGE, start=c(2013, yday("2013-09-02")), frequency=5)
plot(forecast(ets(y), 10), axes=FALSE)
axis(2)
a = seq(as.Date("2013-09-02"), by="weeks", length=55)
axis(1, at = decimal_date(a), labels = format(a, "%Y %b %d"), cex.axis=0.6)
abline(v = decimal_date(a), col='grey', lwd=0.5)
####neuer TEST !!!!
# Plot your axes.
# `at` is an approximation--there's probably a better way to do this, 
# but the logic is approximately 365.25 days in a year, and an origin
# date in R of `January 1, 1970`
axis(1, at = as.numeric(a)/251.25+1970, labels = a, cex.axis=0.6)
axis(2, cex.axis=0.6)


```{r Daten auftrennen}
testset = xts_ts["2014-06-16/2014-06-24"]
trainset = xts_ts["/2014-06-15"]

#Nun ohne Wochenende
testset_ow = xts_ts_ow["2014-06-16/2014-06-24"]
trainset_ow = xts_ts_ow["/2014-06-15"]

#Feiertage auch noch ausblenden
feiertage <- c("2012-11-01","2012-12-20","2013-01-01","2013-03-29","2013-04-01","2013-05-01","2013-05-09","2013-05-20","2013-05-30","2013-10-03","2013-11-01","2013-12-25","2013-12-26","2014-01-01","2014-04-18","2014-04-21","2014-05-01","2014-05-29","2014-06-09","2014-06-19")
betriebsruhe_2012 <- seq(as.Date("2012-12-21"),as.Date("2012-12-31"),"day")
betriebsruhe_2013 <- seq(as.Date("2013-12-21"),as.Date("2015-01-05"),"day")
feiertage <- as.Date(feiertage)

trainset_ow_of <- trainset_ow[ ! time(trainset_ow) %in% c(feiertage,betriebsruhe_2012, betriebsruhe_2013) ]
testset_ow_of <- testset_ow[ ! time(testset_ow) %in% c(feiertage, betriebsruhe_2012, betriebsruhe_2013)]
```

```{r vorhersagen}
library(forecast)
arima_model <- auto.arima(trainset)
arima_forecast <- forecast(arima_model, 10)

plot(forecast(trainset, robust=TRUE, h=20))
plot(forecast(ets(trainset, h=20)))
plot(forecast(auto.arima(trainset, h=20)))

#Und nun ohne Wochenende
plot(forecast(trainset_ow, robust=TRUE, h=20))
plot(forecast(ets(trainset_ow)))
plot(forecast(auto.arima(trainset_ow)))

#und ohne Feiertage
plot(forecast(trainset_ow_of))
plot(forecast(auto.arima(trainset_ow_of)),xlim=c(200,300))
accuracy(forecast(auto.arima(trainset_ow_of)))
plot(forecast(arima_model(trainset_ow_of)))

#Irgendwie gabs hier nicht so wirklich 100% brauchbare Ergebnisse
```

```{r Versuch mit Wochendaten}
#klappt nicht so
plot(weeklysum)
weeklysum <- apply.weekly(ts(daten_ow, start=c(2013,9),frequency=5), sum)
as.ts(weeklysum)
HoltWinters(as.ts(weeklysum))
plot(forecast(auto.arima(weeklysum)))
```
