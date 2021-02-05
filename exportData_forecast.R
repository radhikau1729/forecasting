
##########
##importing required libraries
#########
library(dplyr)
library(forecast)
library(fpp2)
library(TSstudio)
library(seasonal)

#####################
##Reading export csv file
####################
export.data <- read.csv("C:\\Users\\Radhika.upadhyay\\Downloads\\Exim.csv")


##############################################
##Time series object
#############################################
export.ts <- ts(export.data$Export,frequency = 12,start=c(1997,1),end=c(2020,3))
autoplot(export.ts)


#x11 is able to catch seasonality

x11.decompose <- export.ts %>% seas(x11="")
autoplot(x11.decompose)

#####################################
#Analysis of seasonality
####################################
seasonal(x11.decompose)
seasonal(x11.decompose) %>% ggsubseriesplot()+ylab("Seasonal")

autoplot(stl(export.ts,s.window = "periodic"))
seasonal(stl(export.ts,s.window = "periodic")) %>% ggsubseriesplot()+ylab("Seasonal")


###########################################
##Data Partioning
##########################################
data.partition = ts_split(export.ts,sample.out=60)
test.data <- data.partition$test
train.data <- data.partition$train


####Forecasting using stlf#####
stlf.ft <- stlf(train.data,h=60,method="rwdrift")
stlf.ft
autoplot(stlf.ft)+autolayer(test.data,series="test data")
checkresiduals(stlf.ft)
forecast::accuracy(stlf.ft,test.data)


##########################
#Forecasting using holt winter's multiplicative method
#########################


hw.forecast <- hw(train.data,h=60,seasonal = "multiplicative")
autoplot(hw.forecast)+autolayer(test.data,series="test data")
checkresiduals(hw.forecast)
forecast::accuracy(hw1,test.data)


######################################
#Forecasting using linear regression
#####################################

lm.model <- tslm(train.data~trend+I(season))
lm.forecasting <- forecast(lm.model,h=60)
lm.forecasting
autoplot(lm.forecasting)+autolayer(test.data,series="test data")
checkresiduals(lm.model)

forecast::accuracy(lm.forecasting,test.data)


###########################################################
#Forecasting using linear regression with exponential trend
###########################################################

lm.model.expo <- tslm(train.data~trend+I(trend^2)+I(season))
lm.forecasting.expo <- forecast(lm.model.expo,h=60)
lm.forecasting.expo
autoplot(lm.forecasting.expo)+autolayer(test.data,series="test data")
checkresiduals(lm.model.expo)

forecast::accuracy(lm.forecasting.expo,test.data)


###########################
###Forecast using Arima model
##########################

arima.model <- auto.arima(train.data,d=1,D=1,stepwise = FALSE,approximation = FALSE,trace=TRUE)
arima.forecasting <- forecast(arima.model,h=60)
autoplot(arima.forecasting)+autolayer(test.data,series="test data")
checkresiduals(arima.model)
forecast::accuracy(arima.forecasting,test_data)


########################################
##Final Forecasting using ARIMA models
#######################################

arima.model <- auto.arima(export.ts,d=1,D=1,stepwise = FALSE,approximation = FALSE,trace=TRUE)

arima.final.ft <- forecast(arima.model,h=24)
autoplot(arima.final.ft)
forecast::accuracy(arima.final.ft)
checkresiduals(arima.final.ft)

arima.final.ft



