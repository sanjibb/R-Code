setwd("~/microsoft revenue")
data2b<-read.csv("Microsoft Final Pull.csv")
summary(data2b)

install.packages("forecast")
library(forecast)

install.packages("quantmod")
library(quantmod)
require(xts)

install.packages("zoo")
library(zoo)

zooser <- zoo(x=data2b, order.by=as.POSIXct(data2b$datetm, origin="2011-01-01"))
b<-window(zooser,start='2013-01-01', end='2013-12-31')
periodtrain2<-ts(b$Actual.Gross_sales,start=min(data2b$datetm),end=(max(data2b$datetm)))

plot(b)
data2b$datetm<-as.Date(data2b$reviseddate1,"%m/%d/%y")
attach(data2b)

detach(data2b)
summary(data2b)

data2bsorted<-data2b[order(data2b$revisedate1)]

period33<-ts(data2b$Actual.Gross_sales,freq=365,start=100,end=500)


head(period33)



period2012<-ts(data2b$Actual.Gross_sales,freq=365,start=2012)
periodtrain<-ts(data2b$Actual.Gross_sales,start=min(data2b$datetm),end=(max(data2b$datetm)-100))
periodtrain1<-ts(data2b$Actual.Gross_sales,start=min(data2b$datetm),end=(max(data2b$datetm)))










#Moving Average
periodtrainma <-ma(data2b$Actual.Gross_sales,order=4,centre=FALSE)
a<-periodtrainma
write.csv(a,file="Mydata.csv")

lag.plot(periodtrain, lags=15, do.lines=FALSE)
acf(periodtrain)



plot(log(data2b$Actual.Gross_sales),ylab="Transformed electricity demand",xlab="Year", main="Transformed monthly electricity demand")
title(main="Log",line=-1)


plot(periodtrainma)
acf(periodtrain)
pacf(periodtrain)





beerfit1 <- meanf(periodtrain,h=100)
beerfit2 <- rwf(periodtrain,h=100)
beerfit3 <- snaive(periodtrain,h=100)

plot(beerfit1, plot.conf=FALSE,
     main="Forecasts for quarterly beer production")
lines(beerfit2$mean,col=2)
lines(beerfit3$mean,col=3)
lines(periodtrain)
legend("topright", lty=1, col=c(4,2,3),
       legend=c("Mean method","Naive method","Seasonal naive method"))


beer3 <- window(periodtrain, start=14975)
accuracy(beerfit1)
accuracy(beerfit2)
accuracy(beerfit3)
accuracy(beerfit1, beer3)
accuracy(beerfit2, beer3)
accuracy(beerfit3, beer3)

fit <- stl(ts(periodtrain1,freq=365), t.window=5, s.window="periodic", robust=TRUE)
fitb <- stl(ts(periodtrain2,freq=12), t.window=5, s.window="periodic", robust=TRUE)

plot(fit)
plot(fitb)
eeadj <- seasadj(fit)
plot(naive(eeadj), xlab="New orders index",main="Naive forecasts of seasonally adjusted data")


fcast <- forecast(fit, method="naive")
plot(fcast, ylab="New orders index")

accuracy(fcast)
fcast





















periodtrainarima<- auto.arima(x=periodtrain)

as.zoo(period2012)
plot(period2012)

chartSeries(period2012)
addBBands()
addMACD(32,50,12)
class(period2012)



predict(periodtrainarima, n.ahead=,se.fit=TRUE)


forecastPR<- forecast(object=periodtrainarima)
plot(forecastPR)

#Original Model

acf(periodtrain)
pacf(periodtrain)

#ARIMA Model
acf(periodtrainarima$residuals)
pacf(periodtrainarima$residuals)

coef(periodtrainarima)




#Forecast Accuracy RMSE 








fit <- stl(period2012, t.window=15, s.window="periodic", robust=TRUE)
eeadj <- seasadj(fit)
plot(naive(eeadj), xlab="New orders index",main="Naive forecasts of seasonally adjusted data")
fcast <- forecast(fit, method="naive")
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


#simple exponential method


fit1 <- ses(periodtrain, alpha=0.2, initial="simple", h=3)
fit2 <- ses(periodtrain, alpha=0.6, initial="simple", h=3)
fit3 <- ses(periodtrain, h=3)
plot(fit1, plot.conf=FALSE, ylab="Oil (millions of tonnes)",
     xlab="Year", main="", fcol="white", type="o")
lines(fitted(fit1), col="blue", type="o")
lines(fitted(fit2), col="red", type="o")
lines(fitted(fit3), col="green", type="o")
lines(fit1$mean, col="blue", type="o")
lines(fit2$mean, col="red", type="o")
lines(fit3$mean, col="green", type="o")
legend("topleft",lty=1, col=c(1,"blue","red","green"), 
       c("data", expression(alpha == 0.2), expression(alpha == 0.6),
         expression(alpha == 0.89)),pch=1)

fcast <- forecast(fit1, method="naive")
plot(fcast)
accuracy(fit1)


#Holt Linear Trend Method

fit1 <- holt(air, alpha=0.8, beta=0.2, initial="simple", h=5) 
fit2 <- holt(air, alpha=0.8, beta=0.2, initial="simple", exponential=TRUE, h=5) 
# Results for first model:
fit1$model$state
fitted(fit1)
fit1$mean
fit3 <- holt(prodtrain, alpha=0.8, beta=0.2, damped=TRUE, initial="simple", h=5) 
plot(fit2, type="o", ylab="Air passengers in Australia (millions)", xlab="Year", 
     fcol="white", plot.conf=FALSE)
lines(fitted(fit1), col="blue") 
lines(fitted(fit2), col="red")
lines(fitted(fit3), col="green")
lines(fit1$mean, col="blue", type="o") 
lines(fit2$mean, col="red", type="o")
lines(fit3$mean, col="green", type="o")
legend("topleft", lty=1, col=c("black","blue","red","green"), 
       c("Data","Holt's linear trend","Exponential trend","Additive damped trend"))




decomp <- function(x,transform=TRUE)
{
  require(forecast)
  # Transform series
  if(transform & min(x,na.rm=TRUE) >= 0)
  {
    lambda <- BoxCox.lambda(na.contiguous(x))
    x <- BoxCox(x,lambda)
  }
  else
  {
    lambda <- NULL
    transform <- FALSE
  }
  # Seasonal data
  if(frequency(x)>1)
  {
    x.stl <- stl(x,s.window="periodic",na.action=na.contiguous)
    trend <- x.stl$time.series[,2]
    season <- x.stl$time.series[,1]
    remainder <- x - trend - season
  }
  else #Nonseasonal data
  {
    require(mgcv)
    tt <- 1:length(x)
    trend <- rep(NA,length(x))
    trend[!is.na(x)] <- fitted(gam(x ~ s(tt)))
    season <- NULL
    remainder <- x - trend
  }
  return(list(x=x,trend=trend,season=season,remainder=remainder,
              transform=transform,lambda=lambda))
}


decomp(periodtrain)

# f1 maps [0,infinity) to [0,1]
f1 <- function(x,a,b)
{
  eax <- exp(a*x)
  if (eax == Inf)
    f1eax <- 1
  else
    f1eax <- (eax-1)/(eax+b)
  return(f1eax)
}

# f2 maps [0,1] onto [0,1]
f2 <- function(x,a,b)
{
  eax <- exp(a*x)
  ea <- exp(a)
  return((eax-1)/(eax+b)*(ea+b)/(ea-1))
}

measures <- function(x)
{
  require(forecast)
  
  N <- length(x)
  freq <- find.freq(x)
  fx <- c(frequency=(exp((freq-1)/50)-1)/(1+exp((freq-1)/50)))
  x <- ts(x,f=freq)
  
  # Decomposition
  decomp.x <- decomp(x)
  
  # Adjust data
  if(freq > 1)
    fits <- decomp.x$trend + decomp.x$season
  else # Nonseasonal data
    fits <- decomp.x$trend
  adj.x <- decomp.x$x - fits + mean(decomp.x$trend, na.rm=TRUE)
  
  # Backtransformation of adjusted data
  if(decomp.x$transform)
    tadj.x <- InvBoxCox(adj.x,decomp.x$lambda)
  else
    tadj.x <- adj.x
  
  # Trend and seasonal measures
  v.adj <- var(adj.x, na.rm=TRUE)
  if(freq > 1)
  {
    detrend <- decomp.x$x - decomp.x$trend
    deseason <- decomp.x$x - decomp.x$season
    trend <- ifelse(var(deseason,na.rm=TRUE) < 1e-10, 0, 
                    max(0,min(1,1-v.adj/var(deseason,na.rm=TRUE))))
    season <- ifelse(var(detrend,na.rm=TRUE) < 1e-10, 0,
                     max(0,min(1,1-v.adj/var(detrend,na.rm=TRUE))))
  }
  else #Nonseasonal data
  {
    trend <- ifelse(var(decomp.x$x,na.rm=TRUE) < 1e-10, 0,
                    max(0,min(1,1-v.adj/var(decomp.x$x,na.rm=TRUE))))
    season <- 0
  }
  
  m <- c(fx,trend,season)
  
  # Measures on original data
  xbar <- mean(x,na.rm=TRUE)
  s <- sd(x,na.rm=TRUE)
  
  # Serial correlation
  Q <- Box.test(x,lag=10)$statistic/(N*10)
  fQ <- f2(Q,7.53,0.103)
  
  # Nonlinearity
  p <- terasvirta.test(na.contiguous(x))$statistic
  fp <- f1(p,0.069,2.304)
  
  # Skewness
  sk <- abs(mean((x-xbar)^3,na.rm=TRUE)/s^3)
  fs <- f1(sk,1.510,5.993)
  
  # Kurtosis
  k <- mean((x-xbar)^4,na.rm=TRUE)/s^4
  fk <- f1(k,2.273,11567)
  
  # Hurst=d+0.5 where d is fractional difference.
  H <- fracdiff(na.contiguous(x),0,0)$d + 0.5
  
  # Lyapunov Exponent
  if(freq > N-10)
    stop("Insufficient data")
  Ly <- numeric(N-freq)
  for(i in 1:(N-freq))
  {
    idx <- order(abs(x[i] - x))
    idx <- idx[idx < (N-freq)]
    j <- idx[2]
    Ly[i] <- log(abs((x[i+freq] - x[j+freq])/(x[i]-x[j])))/freq
    if(is.na(Ly[i]) | Ly[i]==Inf | Ly[i]==-Inf)
      Ly[i] <- NA
  }
  Lyap <- mean(Ly,na.rm=TRUE)
  fLyap <- exp(Lyap)/(1+exp(Lyap))
  
  m <- c(m,fQ,fp,fs,fk,H,fLyap)
  
  # Measures on adjusted data
  xbar <- mean(tadj.x, na.rm=TRUE)
  s <- sd(tadj.x, na.rm=TRUE)
  
  # Serial
  Q <- Box.test(adj.x,lag=10)$statistic/(N*10)
  fQ <- f2(Q,7.53,0.103)
  
  # Nonlinearity
  p <- terasvirta.test(na.contiguous(adj.x))$statistic
  fp <- f1(p,0.069,2.304)
  
  # Skewness
  sk <- abs(mean((tadj.x-xbar)^3,na.rm=TRUE)/s^3)
  fs <- f1(sk,1.510,5.993)
  
  # Kurtosis
  k <- mean((tadj.x-xbar)^4,na.rm=TRUE)/s^4
  fk <- f1(k,2.273,11567)
  
  m <- c(m,fQ,fp,fs,fk)
  names(m) <- c("frequency", "trend","seasonal",
                "autocorrelation","non-linear","skewness","kurtosis",
                "Hurst","Lyapunov",
                "dc autocorrelation","dc non-linear","dc skewness","dc kurtosis")
  
  return(m)
}


b<-measures(data2b$Actual.Gross_sales)

tsdisplay(diff(eeadj),main="")











