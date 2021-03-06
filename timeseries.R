# Date: 06/28/18
# Objective: Create a preliminary timeseries analysis for predicting flares
setwd("/Users/cassandrabayer/Desktop/Flaredown Prediction/Flaredown-Prediction")
source("Controller.R")
# Sample timeseries -------------------------------------------------------------------------------------
## Choosing one user for multiple conditions
timeseriesDt <- fd[user_id == "QEVuQwEAlNMIH8RXhjZvx6HzoW8iXQ=="][order(checkin_date)]
timeseriesDt[trackable_type %in% c("Condition", "Symptom"), N := .N, by = trackable_name]
timeseriesDt <- timeseriesDt[N>800]
timeseriesDt[, N := NULL]

timeseriesDt <- dcast(timeseriesDt, checkin_date ~ trackable_name, value.var = "trackable_value",
                      fun.aggregate = mean)
timeseriesDt <-  timeseriesDt[complete.cases(timeseriesDt)]
timeseriesDt[, lapply(.SD, as.integer), .SDcols = names(timeseriesDt[, 2:ncol(timeseriesDt)])]
timeseriesDt <- timeseriesDt[order(checkin_date)]

timeseriesDt2 <-timeseriesDt[, checkin_date := as.integer(checkin_date)]

## by day, month
mytsDay <- ts(timeseriesDt[,2:ncol(timeseriesDt)], start = c(2015, 8, 30), frequency = 365,end = c(2018, 3, 5))
mytsMonth <- ts(timeseriesDt2[[1]],  start = c(2015, 8, 30), frequency = 12,end = c(2018, 3, 5))
mytsMonth2 <- ts(timeseriesDt2[, 2:ncol(timeseriesDt)],  start = c(2015, 8, 30), frequency = 12,end = c(2018, 3, 5))

autoplot(mytsDay, facets = T) 
autoplot(mytsDay, facets = F)


# Futzing around with seasonality -----------------------------------------------------------------------
## 1 decomposing into trend, pattern, season
decomp <- decompose(mytsDay$`ulcerative colitis`)
autoplot(decomp)
plot(decomp$seasonal)

## 2 seasonal trends
fit <- stl(mytsMonth, s.window="period")
plot(fit)
monthplot(mytsMonth)
seasonplot(mytsMonth)
seasonplot(mytsMonth2)

## 3
test <- msts(myts, seasonal.periods=c(48,336))

## 4 Detecting seasonality
test <- ts(timeseriesDt$`ulcerative colitis`, frequency = 12)
fit <- tbats(test)
seasonal <- !is.null(fit$seasonal)
seasonplot(test)
# Data is not seasonal



# one user, one condition -------------------------------------------------------------------------------

## Choosing one user for multiple conditions
timeseriesDt <- fd[user_id == "QEVuQwEAlNMIH8RXhjZvx6HzoW8iXQ==" & 
                     trackable_name == "ulcerative colitis", .(checkin_date, trackable_value)][order(checkin_date)]

## by day, month
mytsDay <- ts(timeseriesDt[,2], start = c(2015, 8, 30), frequency = 365,end = c(2018, 3, 5))
mytsMonth <- ts(timeseriesDt[,2], start = c(2015, 8), frequency = 12,end = c(2018, 3))

autoplot(mytsDay, facets = F)
autoplot(mytsMonth, facets = T)


# Futzing around with seasonality -----------------------------------------------------------------------
## 1 decomposing into trend, pattern, season
decomp <- decompose(mytsDay)
autoplot(decomp)
plot(decomp$seasonal)

## 2 seasonal trends
fit <- stl(mytsMonth, s.window="period")
plot(fit)
monthplot(mytsMonth)
seasonplot(mytsMonth)

##2a. deseasonalize
plot(seasadj(fit))

##2b. test whether the data is seasonal
library(tseries)
adf.test(mytsMonth) # pvalue is less than .05, the data is stationary

## 3
test <- msts(myts, seasonal.periods=c(48,336))

## 4 Detecting seasonality
test <- ts(timeseriesDt$`ulcerative colitis`, frequency = 12)
fit <- tbats(test)
seasonal <- !is.null(fit$seasonal)
seasonplot(test)
# Data is not seasonal


# Takeways ----------------------------------------------------------------------------------------------
# 1. COnvert data into a univariate timeseries
# 2. Use autoplot to get a sense of patterns
# 3. Test for seasonality. If no seasonality, the data is stationary
