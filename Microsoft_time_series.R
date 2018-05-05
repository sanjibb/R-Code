setwd("~/documents/Timeseries")
data2b<-read.csv("Microsoft Final Pull.csv")
summary(data2b)

install.packages("forecast")
library(forecast)

install.packages("quantmod")
library(quantmod)
require(xts)




data2b$datetm<-as.Date(data2b$reviseddate1,"%m/%d/%y")
attach(data2b)

detach(data2b)
summary(data2b)

#data2bsorted<-data2b[order(data2b$revisedate1)]



period2012<-ts(data2b$Actual.Gross_sales,freq=365,start=2012)

periodtrain<-ts(data2b$Actual.Gross_sales,start=min(data2b$datetm),end = c(2013, 12),frequency=365)
periodtrain<-ts(data2b$Actual.Gross_sales,start=min(data2b$datetm),end=max(data2b$datetm)-489)
periodtrainarima<- auto.arima(x=periodtrain)

plot(periodtrainarima)
periodtrainarima
as.zoo(period2012)
plot(period2012)

chartSeries(period2012)
addBBands()
addMACD(32,50,12)
class(period2012)

a<-predict(periodtrainarima, n.ahead=365,se.fit=TRUE)
periodtrain<-ts(data2b$Actual.Gross_sales,start=min(data2b$datetm),end=max(data2b$datetm)-489)

forecastPR<- forecast(object=periodtrainarima,365)
forecastPR

plot(forecastPR)
install.packages("rugarch")
library(rugarch)

periodtrainstl<-ts(data2b$Actual.Gross_sales,start=min(data2b$datetm),end=max(data2b$datetm)-124)
b<-stl(period2012,t.window=15, s.window="periodic", robust=TRUE)

eeadj <- seasadj(b)
plot(naive(eeadj), xlab="New orders index",main="Naive forecasts of seasonally adjusted data")
fcast <- forecast(b, method="naive")
plot(fcast, ylab="New orders index")

fcast



plot(b)

forecastPRSTL<- forecast(object=b,365)




#Original Model

acf(periodtrain)
pacf(periodtrain)

#ARIMA Model
acf(periodtrainarima$residuals)
pacf(periodtrainarima$residuals)

coef(periodtrainarima)




#Forecast Accuracy RMSE 








fit <- stl(period2012, t.window=10, s.window="periodic", robust=TRUE)
eeadj <- seasadj(fit)
plot(naive(eeadj), xlab="New orders index",main="Naive forecasts of seasonally adjusted data")
fcast <- forecast(eeadj, method="naive")
plot(fcast, ylab="New orders index")



data2btrain<-data2b[which(data2b$datetm < '2014-01-01')]

summary(data2btrain)
max(data2btrain$Date)

install.packages("fpp")
library(fpp)
data2c<-data2b$Actual.Gross_sales
bts<-ts(data2c,freq=365,star=2011)

btsmean<-meanf(bts,20)
summary(btsmean)
btsnaive<-naive(bts,20)
summary(btsnaive)
rwf(bts,50)



head(elecequip)
summary(elecequip)
























head(data2c)
fit <- stl(data2c,"periodic")
head(data2b$Actual.Gross_sales)

plot(data2c)

b<-ts(data2c,freq=365,star=2011)
period2012<-ts(data2b$Actual.Gross_sales,freq=365,start=01/01/2015)

c<-naive(b,20)







summary(period2012)
plot(period2012)
plot(b)
seasonplot(b[,"Actual.Gross_sales"])
monthplot(b[,"Actual.Gross_sales"])

plot(b[,"Actual.Gross_sales"])

b$Actual.Gross_sales

plot(b$Actual.Gross_sales)

b$


a<-stl(b,t.window=5, s.window="per", robust=TRUE)

summary(a)


head(a)

plot(a)

boxplot(as.data.frame(b))


b[,"Actual.Gross_sales"]

stl()

install.packages("graphics")

require(graphics)

plot(stl(b,s.window = 7, t.window = 30, t.jump = 1))






