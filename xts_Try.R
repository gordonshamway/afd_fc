# trying things with zoo

library(xts)
raw <- read.csv("./afd_zeiten.csv", sep=";")

#converting into Date
raw$EINSPIELDATUM <- as.Date(raw$EINSPIELDATUM, format="%d.%m.%Y")

#create zoo
zo <- xts(raw$ANZ_AUFTRAEGE, order.by=raw$EINSPIELDATUM)

#create model
library(forecast)
fit <- auto.arima(zo)
fc <- forecast(fit, h=10)
plot(fc, axes=TRUE,)
#DOESNT WORK PROPERLY EITHER

as.Date(250, origin="2014-01-01")
as.Date(259, origin="2014-01-01")
#with xts it is the best so far, except that the forecast always predicts for the whole time including
#weekends, so the only way to handle those situation is to impute the values for the missing 
# holidays and weekends

