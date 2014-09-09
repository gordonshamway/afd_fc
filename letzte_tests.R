#Einlesen
daten <- file.choose()
daten <- read.csv(daten, sep=";")

#Datum setzen
daten$EINSPIELDATUM <- as.Date(daten$EINSPIELDATUM, "%d.%m.%Y")

#versuche ein ordentliches ts object zu erstellen
test <- ts(daten$ANZ_AUFTRAEGE, start=c(2013, yday("2013-09-02")), frequency=365)

# hier mal mit xts
test <- xts(daten$ANZ_AUFTRAEGE, order.by=daten$EINSPIELDATUM)
names(test) <- "ANZ_AUFTRAEGE"

#ein Datum prüfen
as.Date(244, origin="2013-01-01")
as.Date(247, origin="2014-01-01")

y = ts(daten$ANZ_AUFTRAEGE, start=c(2013, 244), frequency=365)

#Ohne Wochenende
require(timeDate)
daten_ow <- daten[isWeekday(daten$EINSPIELDATUM),]
y = ts(daten_ow$ANZ_AUFTRAEGE, start=c(2013, 244), frequency=261)

# erst unten eine start-sequenz eingeben und oben dann justieren wann das startelement den
#richtigen Zeitraum ausgibt.
require(lubridate)
y = ts(daten$ANZ_AUFTRAEGE, start=c(2013, 244), frequency=365)
fit <- auto.arima(y) #or bats
fc <- forecast(fit, h=10)
plot(fc, axes=FALSE,)

#y-achse  
axis(2, cex.axis=0.6, las=2)
library(Hmisc)

#x-achse
abline(v = decimal_date(a), col='grey', lwd=0.5)
a = seq(from=as.Date("2013-09-02"), by="weeks", length=55)
axis(1, at = decimal_date(a), labels = format(a, "%d.%m.%Y"), cex.axis=0.6,las=2)



#--Produces a data.frame with the Source Data+Training Data, Fitted Values+Forecast Values, forecast data Confidence Intervals
funggcast<-function(dn,fcast){ 
  require(zoo) #needed for the 'as.yearmon()' function
  
  en<-max(time(fcast$mean)) #extract the max date used in the forecast
  
  #Extract Source and Training Data
  ds<-as.data.frame(window(dn,end=en))
  names(ds)<-'observed'
  ds$date<-as.Date(time(window(dn,end=en)))
  
  #Extract the Fitted Values (need to figure out how to grab confidence intervals)
  dfit<-as.data.frame(fcast$fitted)
  dfit$date<-as.Date(time(fcast$fitted))
  names(dfit)[1]<-'fitted'
  
  ds<-merge(ds,dfit,all.x=T) #Merge fitted values with source and training data
  
  #Exract the Forecast values and confidence intervals
  dfcastn<-as.data.frame(fcast)
  dfcastn$date<-as.Date(as.yearmon(row.names(dfcastn)))
  names(dfcastn)<-c('forecast','lo80','hi80','lo95','hi95','date')
  
  pd<-merge(ds,dfcastn,all.x=T) #final data.frame for use in ggplot
  return(pd)
}

library(forecast)

set.seed(1234)  
#y<-arima.sim(model=list(order=c(2,1,1),ar=c(0.5,.3),ma=0.3),n=144)
#y<-ts(y,freq=12,start=c(2000,1))
y = ts(daten$ANZ_AUFTRAEGE, start=c(2013, 244), frequency=365)

#-- Extract Training Data, Fit the Wrong Model, and Forecast
yt<-window(y,end=2014.10)
yfit <- auto.arima(yt)
#yfit<-Arima(yt,order=c(1,0,1))

yfor<-forecast(yfit)

#---Extract the Data for ggplot using funggcast()
pd<-funggcast(y,yfor) #y sind die daten und yfor ist der forecast

#---Plot in ggplot2 0.9
library(ggplot2)
library(scales)


p1a <- ggplot(data=pd,aes(x=date,y=observed)) 
p1a <- p1a+geom_line(col='red')
p1a <- p1a+geom_line(aes(y=fitted),col='blue')
#p1a <- p1a+geom_line(aes(y=forecast))+geom_ribbon(aes(ymin=lo95,ymax=hi95),alpha=.25)
p1a <- p1a+scale_x_date(name='',breaks='1 year',minor_breaks='1 month',labels=date_format("%b-%y"),expand=c(0,0))
p1a <- p1a+scale_y_continuous(name='Units of Y')
#p1a <- p1a+theme(axis.text.x=element_text(size=10),title='Arima Fit to Simulated Data\n (black=forecast, blue=fitted, red=data, shadow=95% conf. interval)')
p1a

