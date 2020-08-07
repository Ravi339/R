
############################## EXERCISE 4 #################################
#----------------------------- Milk DATA SET -----------------------------#
#-------------------------------------------------------------------------#

#install.packages("fpp", dependencies=TRUE)
#install.packages("portes",dependencies=TRUE)
#install.packages("parallel")
#install.packages("readxl")

library("fpp")
library("parallel")
library("portes")
library("readxl")

# Setting working directory
setwd("C:/Users/Ravi/Desktop/Forecasting/Assignments")
getwd()

# Reading Data and converting into time series
milk <- read_excel("milk.xlsx")
milk_ts <- ts(milk$Quantity,frequency=12,start=1997)

# Visualizing the data
plot(milk_ts, main="Monthly cow milk production",
     xlab="Year", ylab="Production", type="o")

tsdisplay(milk_ts, main="Monthly cow milk production",
          xlab="Year", ylab="Production")

# Splitting data into Train and test sets

milk_train <- window(milk_ts, end=c(2015,12))
milk_test <- window(milk_ts,start=c(2016,1))

#---------------------- ETS Model ---------------------------#

m <- c("AAA", "AAM", "AMA", "AMM","AAA", "AAM", "AMA", "AMM")
result <- matrix(data=NA, nrow=8, ncol=6)
for (i in 1:8){
  if (i<=4){
    model <- ets(milk_train, model=m[i], damped=FALSE, restrict=FALSE )
    result[i,6] <- FALSE
  }
  else {
    model <- ets(milk_train, model=m[i], damped=TRUE, restrict=FALSE)
    result[i,6] <- TRUE
  }
  f <- forecast(model, h=length(milk_test))
  a <- accuracy(f, milk_test)
  result[i,1] <- model$aicc
  result[i,2] <- a[1,6]
  result[i,3] <- a[2,6]
  result[i,4] <- a[1,2]
  result[i,5] <- a[2,2]
}

rownames(result) <- m
colnames(result) <- c("AICc", "MASE_train", "MASE_test", "RMSE_train", "RMSE_test", "Damping")
result
# Best 2 model from ETS is (AAA) damping and (AMM) damping

# Auto ETS model
etc_auto <- ets(milk_train)
forcast_etc_auto <- forecast(etc_auto,h=length(milk_test))
accuracy_etc_auto <- accuracy(forcast_etc_auto,milk_test)


#---------------------- ARIMA MODEL --------------------#

# seasonal rainbow plot to check seasonality and trend
seasonplot(milk_ts, year.labels=TRUE, year.labels.left=FALSE, ylab="Production of cow milk",
           col=rainbow(20), pch=19, xlab="Month", main="Production : seasonality plot")


tsdisplay(milk_ts, main="Yearly Production of cow milk",ylab="Production", xlab="Year")
# data need to be stationized 

# Analyzing number of difference needed for seasonal and non seasonal part to stationized the data

nsdiffs(milk_ts)
ndiffs(diff(milk_ts,12))

# Seasonal difference D = 1
# Non seasonal difference d = 0

tsdisplay(diff(milk_ts,12), main="Yearly Production of cow milk", ylab="Production", xlab="Year")

# Depending on ACF and PACF , we can start analysis with ARIMA(1,0,0)(0,1,0) /12 


# Function for executing ARIMA Model 
getinfo <- function(x,...){
  fit <- Arima(x,...)
  fc <- forecast(fit,h= length(milk_test))
  a <- accuracy(fc,milk_test)
  result <- matrix(NA, nrow=1, ncol=5)
  result[1,1] <- fit$aicc
  result[1,2] <- a[1,6]
  result[1,3] <- a[2,6]
  result[1,4] <- a[1,2]
  result[1,5] <- a[2,2]
  colnames(result) <- c("AICc", "MASE_train", "MASE_test", "RMSE_train", "RMSE_test")
  return(result)
}

# Function for executing ARIMA model with combination value of p and q
mat <- matrix(NA,nrow=18, ncol=5)
colnames(mat) <- c("AICc", "MASE_train", "MASE_test", "RMSE_train", "RMSE_test")
mat[1,]<-getinfo(milk_train,order=c(0,0,1),seasonal=c(0,1,0))
mat[2,]<-getinfo(milk_train,order=c(0,0,1),seasonal=c(0,1,1))
mat[3,]<-getinfo(milk_train,order=c(0,0,1),seasonal=c(0,1,2))
mat[4,]<-getinfo(milk_train,order=c(0,0,1),seasonal=c(1,1,0))
mat[5,]<-getinfo(milk_train,order=c(0,0,1),seasonal=c(2,1,0))
mat[6,]<-getinfo(milk_train,order=c(0,0,1),seasonal=c(1,1,1))
mat[7,]<-getinfo(milk_train,order=c(1,0,0),seasonal=c(0,1,0))
mat[8,]<-getinfo(milk_train,order=c(1,0,0),seasonal=c(0,1,1))
mat[9,]<-getinfo(milk_train,order=c(1,0,0),seasonal=c(0,1,2))
mat[10,]<-getinfo(milk_train,order=c(1,0,0),seasonal=c(1,1,0))
mat[11,]<-getinfo(milk_train,order=c(1,0,0),seasonal=c(2,1,0))
mat[12,]<-getinfo(milk_train,order=c(1,0,0),seasonal=c(1,1,1))
mat[13,]<-getinfo(milk_train,order=c(1,0,1),seasonal=c(0,1,0))
mat[14,]<-getinfo(milk_train,order=c(1,0,1),seasonal=c(0,1,1))
mat[15,]<-getinfo(milk_train,order=c(1,0,1),seasonal=c(0,1,2))
mat[16,]<-getinfo(milk_train,order=c(1,0,1),seasonal=c(1,1,0))
mat[17,]<-getinfo(milk_train,order=c(1,0,1),seasonal=c(2,1,0))
mat[18,]<-getinfo(milk_train,order=c(1,0,1),seasonal=c(1,1,1))

mat

# No 4 order=c(0,0,4),seasonal=c(0,1,4) is best on test
# No 16 order=c(3,0,2),seasonal=c(3,1,2) is best on train


# Auto ARIMA with d= 0 and D=1
arima_auto <- auto.arima(milk_train,stepwise = FALSE, D =1,d=0)
f <- forecast(arima_auto,h= length(milk_test))
acc <- accuracy(f,milk_test)
summary(arima_auto)
# Model Found ARIMA(1,0,1)(1,1,1)[12] with drift 
# Not giving good result


#------- Checking white noise test for All selected and considered models ------#


m1 <- Arima(milk_ts, order=c(1,0,0), seasonal=c(1,1,1))
tsdisplay(m1$residuals)
LjungBox(m1$residuals, lags=seq(4,24,4), order=length(m1$par))
# This Model failes in test

m2 <- Arima(milk_ts, order=c(1,0,1), seasonal=c(1,1,1))
tsdisplay(m2$residuals)
LjungBox(m2$residuals, lags=seq(4,24,4), order=length(m2$par))
# This Model failes in test


m3 <- ets(milk_ts)
tsdisplay(m3$residuals)
LjungBox(m3$residuals, lags=seq(4,24,4), order=length(m3$par))
# This Model failes in test


m4 <- ets(milk_ts,model="AAA",damped = TRUE)
tsdisplay(m4$residuals)
LjungBox(m4$residuals, lags=seq(4,24,4), order=length(m4$par))
# This Model pass in test


# Best model which also pass in test ETS( AAA) damping

Best_model <- ets(milk_ts,model="AAA",damped = TRUE)


best_model_forecast <- forecast(Best_model, h=13)
plot(best_model_forecast)
summary(Best_model)
