###dshw method from the R forecast package to predict electricity consumption. 

##I tried to use it on the taylor dataset available in the package.
####I used the parameters of the model reported in the paper (Table 2)

###Taylor, J.W. (2003) Short-term electricity demand forecasting using double seasonal exponential smoothing. Journal of the Operational Research Society, 54, 799-805.

###but when I evaluated the MAPE error for the next 48 half-hours I got different values than reported in the paper (the bottom curve in Figure 4).

###Here is my code and comparison of MAPE errors. What am I doing wrong? How did Taylor calculated the MAPE error? Why the Figure caption says results for the 4-week post sample period when there is only 48 half-hours in the Figure? Thanks for help.

library("forecast")

# first 8 weeks as training set 
train <- msts(taylor[1:2688], seasonal.periods=c(48,336), ts.frequency=48)
# the rest - 4 week is test set - starts on 57th day
test <- msts(taylor[2689:4032], seasonal.periods=c(48,336), ts.frequency=48, start=57)

model <- dshw(train, alpha=0.01, beta=0.00, gamma=0.21, omega=0.24, phi=0.92)

# plot of MAPE errors for different horizonts
MAPE <- c()
for(i in 1:48) {
  MAPE <- c(MAPE, accuracy(f=model,x=test[1:i])[2,5])
}
plot(MAPE~c(1:48), ylim=c(0,3))
