############################## EXERCISE 3 #################################
#----------------------------WindPower DATA SET ###########################
#-------------------------------------------------------------------------#


#install.packages("fpp", dependencies=TRUE)
#install.packages("portes",dependencies=TRUE)
#install.packages("parallel")
#install.packages("readxl")

library(fpp)
library("parallel")
library("portes")
library(readxl)

# Working Library Setup

setwd("C:/Users/Ravi/Desktop/Forecasting/Assignments")
getwd()

# Reading Data and converting into time series
wind <- read_excel("WindPower.xlsx")
wind_ts <- ts(wind$TOE,frequency=1,start=1990)

# Ploting data and analysing it
# No seasonality
# Data is unstable 
plot(wind_ts, main=" CONSUMPTION OF WIND POWER ",xlab=" YEAR", ylab="CONSUMPTION", type="o")

tsdisplay(wind_ts, main = "YEARLY CONSUMPTION OF WIND POWER", xlab= "Year", ylab="Consumption")



# Dividing into train and test set

wind_train <- window(wind_ts, end=2010)
wind_test <- window(wind_ts, start=2011)

#------------------------------ Analysing with ETS Model-------------------------------#

# first four model are without damping and rest four are with damping

m <- c("AAN", "MAN", "MMN", "AAN", "MAN", "MMN")
result <- matrix(data=NA, nrow=6, ncol=6)
for (i in 1:6){
  if (i<=3){
    model <- ets(wind_train, model=m[i], damped=FALSE, restrict=FALSE )
    result[i,6] <- FALSE
  }
  else {
    model <- ets(wind_train, model=m[i], damped=TRUE, restrict=FALSE)
    result[i,6] <- TRUE
  }
  f <- forecast(model, h=length(wind_test))
  a <- accuracy(f, wind_test)
  result[i,1] <- model$aicc
  result[i,2] <- a[1,6]
  result[i,3] <- a[2,6]
  result[i,4] <- a[1,2]
  result[i,5] <- a[2,2]
}

rownames(result) <- m
colnames(result) <- c("AICc", "MASE_train", "MASE_test", "RMSE_train", "RMSE_test", "Damping")
result

# Result analysis of model is mentioned in report

# Random walk model with drift

drift <- rwf(wind_train, h=length(wind_test), drift=TRUE)
accuracy(drift, wind_test)[,c(2,6)]

# ETS Automatic modeling

etc_auto <- ets(wind_train)
forcast_etc_auto <- forecast(etc_auto,h=length(wind_test))
accuracy_etc_auto <- accuracy(forcast_etc_auto,wind_test)[,c(2,6)]



############################## ARIMA MODEL ##################################
#---------------------------------------------------------------------------#

# Transform data lambda = 0

BoxCox.lambda(wind_ts)


# Determination of difference number d = 2 

ndiffs(wind_train, test="pp")
ndiffs(wind_train, test="kpss")
#ndiffs(dmc_train, test="adf")
#nsdiffs(dmc_train, test="ch")
#nsdiffs(dmc_train, test="ocsb")

# Differene number comes to 2 to make data stable

#---------------- ARIMA with  d = 2 ----------------#

tsdisplay(diff(diff(wind_train)), main="Yearly wind power Consumption",
          xlab="Year", ylab="Consumption")

# ARIMA(2,2,1) intial guess
# Trying other combinations too

result <- matrix(data=NA, nrow=16, ncol=8)
modelnames <- vector(mode="character", length=9)
k <- 0
for (i in 0:3){
  for (j in 0:3){
    k <- k+1
    m <- Arima(wind_train, order=c(i,2,j),lambda = 0)
    f <- forecast(m, h=length(wind_test))
    a <- accuracy(f, wind_test)
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


# Auto ARIMA with d = 2
arima_auto <- auto.arima(wind_train,stepwise = FALSE,d=2)
f <- forecast(arima_auto,h= length(wind_test))
arima_auto_accuracy <- accuracy(f,wind_test)
summary(arima_auto)
# Auto Model ARIMA(1,2,0) This is not giving good result


#--------------- BEST MODEL ----------------------#

# After ANalyzing all results of ETS and ARIMA Model 
# Best Model is ARIMA(3,2,1)

Best_model <- Arima(wind_ts,order=c(3,2,1),seasonal = FALSE,lambda = 0)
tsdisplay(Best_model$residuals)
LjungBox(Best_model$residuals, lags=seq(1,20,1), order=length(Best_model$par))


best_model_forecast <- forecast(Best_model,h=5)
plot(best_model_forecast)
summary(Best_model)