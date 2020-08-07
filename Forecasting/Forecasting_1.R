############################## EXERCISE 1 ###########################
#---------------------------dmc_ch DATA SET-------------------------#
#-------------------------------------------------------------------#


#install.packages("fpp", dependencies=TRUE)
#install.packages("portes",dependencies=TRUE)
#install.packages("parallel")
#install.packages("readxl")

library(fpp)
library("parallel")
library("portes")
library(readxl)

# working directory settings

setwd("C:/Users/Ravi/Desktop/Forecasting/Assignments")
getwd()


# Reading data and converting into time series
dmc <- read_excel("dmc_ch.xlsx")
dmc_ts <- ts(dmc$DMC_CH,frequency=1,start=1990)

# ploting and analysisng the data
plot(dmc_ts, main="Yearly Domestic Material Consumption",
     xlab="Year", ylab="Consumption", type="o")

tsdisplay(dmc_ts, main="Yearly Domestic Material Consumption",
          xlab="Year", ylab="Consumption")
# Non stationary data
# increasing and decreasing trend
# Non Seasonal data

# Dividing into train and test set
dmc_train <- window(dmc_ts, end=2010)
dmc_test <- window(dmc_ts, start=2011) 


#------------------- ETS Non seasonal Method ----------------------#
# first four model are without damping and rest four are with damping

m <- c("AAN", "AMN", "MAN", "MMN", "AAN", "AMN", "MAN", "MMN")
result <- matrix(data=NA, nrow=8, ncol=6)
for (i in 1:8){
  if (i<=4){
  model <- ets(dmc_train, model=m[i], damped=FALSE, restrict=FALSE )
  result[i,6] <- FALSE
  }
  else {
  model <- ets(dmc_train, model=m[i], damped=TRUE, restrict=FALSE)
  result[i,6] <- TRUE
  }
  f <- forecast(model, h=length(dmc_test))
  a <- accuracy(f, dmc_test)
  result[i,1] <- model$aicc
  result[i,2] <- a[1,6]
  result[i,3] <- a[2,6]
  result[i,4] <- a[1,2]
  result[i,5] <- a[2,2]
}

rownames(result) <- m
colnames(result) <- c("AICc", "MASE_train", "MASE_test", "RMSE_train", "RMSE_test", "Damping")
result

# ETS (AAN) damped gives best results for the test

# Random walk with drift

drift <- rwf(dmc_train, h=length(dmc_test), drift=TRUE)
accuracy(drift, dmc_test)[,c(2,6)]

# ETS Automatic

etc_auto <- ets(dmc_train)
forcast_etc_auto <- forecast(etc_auto,h=length(dmc_test))
accuracy_etc_auto <- accuracy(forcast_etc_auto,dmc_test)


############################## ARIMA MODEL ##################################
#---------------------------------------------------------------------------#

# Determination of difference number d

ndiffs(dmc_train, test="pp")
ndiffs(dmc_train, test="kpss")
#ndiffs(dmc_train, test="adf")
#nsdiffs(dmc_train, test="ch")
#nsdiffs(dmc_train, test="ocsb")

# comes to 0 & 1

#---------------- ARIMA with  d = 1 ----------------#

tsdisplay(diff(dmc_train), main="Yearly Domestic Material Consumption",
          xlab="Year", ylab="Consumption")

# depending on ACF and PACF ARIMA(0,1,0) is initial expected model for d = 1

result <- matrix(data=NA, nrow=9, ncol=8)
modelnames <- vector(mode="character", length=9)
k <- 0
for (i in 0:2){
  for (j in 0:2){
    k <- k+1
    m <- Arima(dmc_train, order=c(i,1,j))
    f <- forecast(m, h=length(dmc_test))
    a <- accuracy(f, dmc_test)
    result[k,1] <- m$arma[1]
    result[k,2] <- m$arma[6]
    result[k,3] <- m$arma[2]
    result[k,4] <- m$aicc
    result[k,5] <- round(a[1,6],4)
    result[k,6] <- round(a[2,6],4)
    result[k,7] <- a[1,2]
    result[k,8] <- a[2,2]
    modelnames[k] <- paste0("m",k)
  }
}

rownames(result) <- modelnames
colnames(result) <- c("p", "d", "q", "AICc", "MASE_train", "MASE_test", "RMSE_train", "RMSE_test")
result

# ARIMA(2,1,2) is best for train with d = 1
# ARIMA(2,1,1) is best for test with d = 1

#------------------ ARIMA with d = 0 ---------------------#

tsdisplay(dmc_train, main="Yearly Domestic Material Consumption",
          xlab="Year", ylab="Consumption")


# depending on ACF and PACF ARIMA(1,0,1) is initial expected model
result <- matrix(data=NA, nrow=9, ncol=8)
modelnames <- vector(mode="character", length=9)
k <- 0
for (i in 0:2){
  for (j in 0:2){
    k <- k+1
    m <- Arima(dmc_train, order=c(i,0,j))
    f <- forecast(m, h=length(dmc_test))
    a <- accuracy(f, dmc_test)
    result[k,1] <- m$arma[1]
    result[k,2] <- m$arma[6]
    result[k,3] <- m$arma[2]
    result[k,4] <- m$aicc
    result[k,5] <- round(a[1,6],4)
    result[k,6] <- round(a[2,6],4)
    result[k,7] <- a[1,2]
    result[k,8] <- a[2,2]
    modelnames[k] <- paste0("m",k)
  }
}

rownames(result) <- modelnames
colnames(result) <- c("p", "d", "q", "AICc", "MASE_train", "MASE_test", "RMSE_train", "RMSE_test")
result

# ARIMA(1,0,0) is best for test with d=0
# ARIMA(2,0,2) is best for train with d=0


# Auto ARIMA with d = 1
arima_auto <- auto.arima(dmc_train,stepwise = FALSE,d=1)
f <- forecast(arima_auto,h= length(dmc_test))
arima_auto_accuracy <- accuracy(f,dmc_test)
summary(arima_auto)
# Auto model ARIMA(0,1,0)  This not giving good result


# Auto ARIMA with d = 0
arima_auto <- auto.arima(dmc_train,stepwise = FALSE,d=0)
f <- forecast(arima_auto,h= length(dmc_test))
arima_auto_accuracy <- accuracy(f,dmc_test)
summary(arima_auto)
# Auto Model ARIMA(1,0,0) This is not giving good result

#--------------White Noise Check and forecasting for best model ------#

# Best Model for forecasting is ARIMA(2,1,1) with d = 1

best_model <- Arima(dmc_ts, order=c(2,1,1))
tsdisplay(best_model$residuals)
LjungBox(best_model$residuals, lags=seq(1,20,1), order=length(best_model$par))


best_model_forecast <- forecast(best_model, h=4)
plot(best_model_forecast)
summary(best_model)


