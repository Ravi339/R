############################## EXERCISE 2 #################################
#-------------------------- TouristTrips DATA SET ------------------------#
#-------------------------------------------------------------------------#

#install.packages("fpp", dependencies=TRUE)
#install.packages("portes",dependencies=TRUE)
#install.packages("parallel")
#install.packages("readxl")

library(fpp)
library("parallel")
library("portes")
library(readxl)

# Working library setup
setwd("C:/Users/Ravi/Desktop/Forecasting/Assignments")
getwd()


# Reading the data
trip <- read_excel("TouristTrips.xlsx")
trip_ts <- ts(trip$`1 night or over`,frequency=12,start=2012)

# Visualizing the data
plot(trip_ts, main=" TRIPS BY MONTH ",xlab=" TIME", ylab="TRIPS", type="o")

tsdisplay(trip_ts, main="TRIPS BY MONTH",
          xlab="Time", ylab="Trips")

# Splitting data into test and train set
trip_train <- window(trip_ts, end=c(2014,12))
trip_test <- window(trip_ts, start=c(2015,1))

#--------------------------- ETS seasonal models --------------------#
# first four model are without damping and rest four are with damping

m <- c("AAA", "AAM", "AMA", "AMM","AAA", "AAM", "AMA", "AMM")
result <- matrix(data=NA, nrow=8, ncol=6)
for (i in 1:8){
  if (i<=4){
    model <- ets(trip_train, model=m[i], damped=FALSE, restrict=FALSE )
    result[i,6] <- FALSE
  }
  else {
    model <- ets(trip_train, model=m[i], damped=TRUE, restrict=FALSE)
    result[i,6] <- TRUE
  }
  f <- forecast(model, h=length(trip_test))
  a <- accuracy(f, trip_test)
  result[i,1] <- model$aicc
  result[i,2] <- a[1,6]
  result[i,3] <- a[2,6]
  result[i,4] <- a[1,2]
  result[i,5] <- a[2,2]
}

rownames(result) <- m
colnames(result) <- c("AICc", "MASE_train", "MASE_test", "RMSE_train", "RMSE_test", "Damping")
result

# Auto ETC MEthod
etc_auto <- ets(trip_train)
forcast_etc_auto <- forecast(etc_auto,h=length(trip_test))
accuracy_etc_auto <- accuracy(forcast_etc_auto,trip_test)

#---------------------- ARIMA MODEL --------------------#

# Seasonal plot for checking seasonality and trend
seasonplot(trip_ts, year.labels=TRUE, year.labels.left=FALSE, ylab="Number of trips",
           col=rainbow(20), pch=19, xlab="Month", main="Trips : seasonality plot")


tsdisplay(trip_ts, main=" trips by month",ylab="number of trips", xlab="Year")


# Deciding on number of difference needed for seasonal and non seasonal part
nsdiffs(trip_ts)
ndiffs(diff(trip_ts,12))

# Needed Seasonal difference =  1
# Needed Non season difference = 0

tsdisplay(diff(trip_ts,12), main="seasonal differenced trips by month",
          ylab="number of trips", xlab="Year")

# one seasonal difference stablized the data

# Based on the ACF and PACF pattern of the seasonal diferenced data, ARIMA(1,0,2)(0,1,2)


# Function for utilizing various ARIMA Model

getinfo <- function(x,...){
fit <- Arima(x,...)
fc <- forecast(fit,h= length(trip_test))
a <- accuracy(fc,trip_test)
result <- matrix(NA, nrow=1, ncol=5)
result[1,1] <- fit$aicc
result[1,2] <- a[1,6]
result[1,3] <- a[2,6]
result[1,4] <- a[1,2]
result[1,5] <- a[2,2]
colnames(result) <- c("AICc", "MASE_train", "MASE_test", "RMSE_train", "RMSE_test")
return(result)
}

# Call of the ARIMA Model Function
# Tried various model with p,d,q  close to our initial prediction from ACF and PACF

mat <- matrix(NA,nrow=8, ncol=5)
colnames(mat) <- c("AICc", "MASE_train", "MASE_test", "RMSE_train", "RMSE_test")
mat[1,]<-getinfo(trip_train,order=c(1,0,2),seasonal=c(0,1,2))
mat[2,]<-getinfo(trip_train,order=c(1,0,1),seasonal=c(0,1,1))
mat[3,]<-getinfo(trip_train,order=c(1,0,2),seasonal=c(0,1,3))
mat[4,]<-getinfo(trip_train,order=c(1,0,1),seasonal=c(0,1,3))
mat[5,]<-getinfo(trip_train,order=c(2,0,2),seasonal=c(0,1,2))
mat[6,]<-getinfo(trip_train,order=c(2,0,1),seasonal=c(0,1,1))
mat[7,]<-getinfo(trip_train,order=c(2,0,2),seasonal=c(0,1,2))
mat[8,]<-getinfo(trip_train,order=c(2,0,1),seasonal=c(0,1,1))
mat



# Auto ARIMA with D = 1 and d=0
arima_auto <- auto.arima(trip_train,stepwise = FALSE,D=1,d=0)
f <- forecast(arima_auto,h= length(trip_test))
arima_auto_accuracy <- accuracy(f,trip_test)
summary(arima_auto)
# Auto model ARIMA(1,0,0)(1,1,0)[12]  This not giving good result


#--------------White Noise Check and forecasting for best model ------#

# AFTER Analizing all results from ATS and ARIMA
# Best Model for forecasting is ARIMA(1,0,1),(0,1,1) /12


Best_model <- Arima(trip_ts, order=c(1,0,1), seasonal=c(0,1,1))
tsdisplay(Best_model$residuals)
LjungBox(Best_model$residuals, lags=seq(4,24,4), order=length(Best_model$par))


best_model_forecast <- forecast(Best_model, h=12)
plot(best_model_forecast)
summary(Best_model)







