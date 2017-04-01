#I want to predict inter-day electricity load. My data are electricity loads for 11 months, sampled in 30 minute intervals. I also got the weather-specific data from a meteorological station (temperature, relative humidity, wind direction, wind speed, sunlight). From this, I want to predict the electricity load until the end of the day.

#I can run my algorithm until 10:00 of the present day and after that it should give the prediction of loads in 30 minute intervals. So, it should tell the load at 10:30, 11:00, 11:30 and so on until 24:00.

#My first attempt was to create a linear model in R.

BP.TS <- ts(Buying.power, frequency = 48)
a <- data.frame(
  Time, BP.TS, Weekday, Pressure, Temperature, RelHumidity, AvgWindSpeed, AvgWindDirection, MaxWindSpeed, MaxWindDirection, SunLightTime,
  m, Buying.2dayago, AfterHolidayAndBPYesterday8, MovingAvgLast7DaysMidnightTemp
)
a <- a[(6*48+1):nrow(a),]

start = 9716
steps.ahead = 21
par(mfrow=c(5,2))
for (i in 1:10) {
  train <- a[1:(start+(i-1)*48),]
  test <- a[((i-1)*48+start+1):((i-1)*48+start+steps.ahead),]
  summary(reg <- lm(log(BP.TS)~., data=train, na.action=NULL))
  pred <- exp(predict(reg, test))
  
  plot(test$BP.TS, type="o")
  lines(pred, col=2)
  cat("MAE", mean(abs(test$BP.TS - pred)), "\n")
}


##This is not very succesful. Now I try to model the data with ARIMA.
##I used auto.arima() from the forecast package. These are the results I got:

> auto.arima(BP.TS)
Series: BP.TS 
ARIMA(2,0,1)(1,1,2)[48]                    

Call: auto.arima(x = BP.TS) 

Coefficients:
  ar1      ar2     ma1    sar1     sma1    sma2
1.1816  -0.2627  -0.554  0.4381  -1.2415  0.3051
s.e.  0.0356   0.0286   0.033  0.0952   0.0982  0.0863

sigma^2 estimated as 256118:  log likelihood = -118939.7
AIC = 237893.5   AICc = 237893.5   BIC = 237947

##Now if I try something like:

reg = arima(train$BP.TS, order=c(2,0,1), xreg=cbind(
  train$Time, 
  train$Weekday, 
  train$Pressure, 
  train$Temperature, 
  train$RelHumidity, 
  train$AvgWindSpeed, 
  train$AvgWindDirection, 
  train$MaxWindSpeed, 
  train$MaxWindDirection, 
  train$SunLightTime,
  train$Buying.2dayago,
  train$MovingAvgLastNDaysLoad,
  train$X1, train$X2, train$X3, train$X4, train$X5, train$X6, train$X7, train$X8, train$X9, 
  train$X11, train$X12, train$X13, train$X14, train$X15, train$X16, train$X17, train$X18, 
  train$MovingAvgLast7DaysMidnightTemp
))

p <- predict(reg, n.ahead=21, newxreg=cbind(
  test$Time, 
  test$Weekday, 
  test$Pressure, 
  test$Temperature, 
  test$RelHumidity, 
  test$AvgWindSpeed, 
  test$AvgWindDirection, 
  test$MaxWindSpeed, 
  test$MaxWindDirection, 
  test$SunLightTime,
  test$Buying.2dayago,
  test$MovingAvgLastNDaysLoad,
  test$X1, test$X2, test$X3, test$X4, test$X5, test$X6, test$X7, test$X8, test$X9, 
  test$X11, test$X12, test$X13, test$X14, test$X15, test$X16, test$X17, test$X18, 
  test$MovingAvgLast7DaysMidnightTemp
))

plot(test$BP.TS, type="o", ylim=c(6300,8300))
par(new=T)
plot(p$pred, col=2, ylim=c(6300,8300))
cat("MAE", mean(p$se), "\n")

#I get even worse results.
#Why? I ran out of ideas, so please help.
#If there is additional information I need to give, please ask.

##============================
####example
library(forecast)
Mwh = c(16.3,16.8,15.5,18.2,15.2,17.5,19.8,19.0,17.5,16.0,19.6,18.0)
temp = c(29.3,21.7,23.7,10.4,29.7,11.9,9.0,23.4,17.8,30.0,8.6,11.8)
d = data.frame(Mwh, temp)
fit = lm(Mwh~temp, data=d)
fcast=forecast(fit, newdata=35)
##================================
