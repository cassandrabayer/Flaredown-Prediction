# Date: 06/28/2018
# Objective: Test run with ARIMA modeling


# About ARIMA -------------------------------------------------------------------------------------------
# - ARIMA assumes non-seasonal data (or, if you have seasonal data, you can de-seasonalize it)

# Pre-processing ----------------------------------------------------------------------------------------
setwd("/Users/cassandrabayer/Desktop/Flaredown Prediction/Flaredown-Prediction")
source("Controller.R")
arimaFD <- fd[user_id == "QEVuQwEAlNMIH8RXhjZvx6HzoW8iXQ=="][order(checkin_date)]
arimaFD[trackable_type %in% c("Condition", "Symptom"), N := .N, by = trackable_name]
arimaFD <- arimaFD[N>500]


# Initial inspection ------------------------------------------------------------------------------------
ggplot(arimaFD, aes(checkin_date, trackable_value)) + 
  geom_point() + 
  scale_x_date('day') + 
  ylab("Trackable Value over the Months") + geom_smooth()

# let's try smoothing it out
valueTs <- ts(arimaFD[, c('trackable_value')])
arimaFD[, cleanTrackVal := tsclean(trackable_value)]
ggplot() +
  geom_point(data = arimaFD, 
            aes(x = checkin_date, y = cleanTrackVal)) + 
  ylab('Trackable Value over the Months') + geom_smooth()

## out of curiosity, I want to do it by month to get a continuious value
arimaFDMonth <- arimaFD[, month := months(checkin_date)]
arimaFDMonthCondition <- arimaFDMonth[, .(valByMonth = mean(trackable_value, na.rm = T)),
                             by = .(month, trackable_name)]
arimaFDMonth <- arimaFDMonthCondition[, .(valByMonth = mean(valByMonth, na.rm = T)),
                                      by = month]
### all conditions
ggplot() +
  geom_point(data = arimaFDMonthCondition, 
             aes(x = month, y = valByMonth)) + 
  ylab('Trackable Value over the Months') + geom_smooth()

### one condition
ggplot() +
  geom_(data = arimaFDMonth, 
             aes(x = month, y = valByMonth)) + 
  ylab('Trackable Value over the Months') + geom_smooth()

## Doesn't look much better-- let's calculate the moving average
arimaFD[, movingAvgWk := ma(cleanTrackVal, order = 7)]
arimaFD[, movingAvgMonth := ma(cleanTrackVal, order = 30)]

ggplot() +
  geom_line(data = arimaFD[!is.na(movingAvgMonth) & !is.na(movingAvgWk)], aes(x = checkin_date, y = trackable_value, colour = "Counts")) +
  geom_line(data = arimaFD[!is.na(movingAvgMonth) & !is.na(movingAvgWk)], aes(x = checkin_date, y = movingAvgWk,   colour = "Weekly Moving Average"))  +
  geom_line(data = arimaFD[!is.na(movingAvgMonth) & !is.na(movingAvgWk)], aes(x = checkin_date, y = movingAvgMonth, colour = "Monthly Moving Average"))  +
  ylab('Trackable Value')

## Values for moving average look weird. Let's clean up and decompose 
decompFd <-  ts(na.omit(arimaFD$movingAvgWk), frequency=52)
decomp <- stl(decompFd, s.window="periodic")
deseasonal <- seasadj(decomp)
plot(decomp)

# test for stationary-ism
adf.test(decompFd, alternative = "stationary")

# Get acf
acf(decompFd, main = '')
Pacf(decompFd, main = '')
