#Modeling Residential Electricity Usage with R
#January 30, 2013
#By Lloyd Spencer
#========================================================================
  
#  Residential power usage is satisfying to model as it can be forecast fairly accurately 
  #with the right inputs.  Partly as a consequence of deregulation there is now more data 
  #more available than before.
  #As in prior postings I will use reproducible R code each step of the way.
  
#  For this posting I will be using data from Commonwealth Edison [ComEd] in Chicago, IL. 
#ComEd makes available historical usage data for different rate classes.  
  #In this example I use rate class C23.
#First we must download the data and fix column and header names:
  
library(xlsx)
library(downloader)
# load historical electric usage data from ComEd website
download(‘https://www.comed.com/Documents/customer-service/rates-pricing/retail-electricity-metering/Historical-Load-Profiles.xls’,’ComEd.xls’,mode=’wb’)
ComEd<-read.xlsx(file=’ComEd.xls’,sheetName=’C23′)
# edit row and column names
dimnames(ComEd)[2][[1]]<-{c(‘Date’,1:24)}
dimnames(ComEd)[1][[1]]<-substr(ComEd[,1],5,15)
ComEd[,1]<-as.Date(substr(ComEd[,1],5,15),’%m/%d/%Y’)

#Next we hypothesize some explanatory variables. 
#Presumably electricity usage is influenced by day of the week, 
#time of the year and the passage of time. 
#Therefore we construct a set of explanatory variables 
#and use them to predict usage for a particular hour of the day [16] as follows:

# construct time related explanatory variables
times<-as.numeric(ComEd[,1]-ComEd[1,1])/365.25
weekdayInd<-!(weekdays(ComEd[,1])==”Saturday”|weekdays(ComEd[,1])==”Sunday”)
timcols<-cbind(weekday=weekdayInd,
               Time=times,S1=sin(2*pi*times),C1=cos(2*pi*times),S2=sin(4*pi*times),C2=cos(4*pi*times),S3=sin(6*pi*times),C3=cos(6*pi*times))
lm1<-lm(ComEd[,16]~timcols)
summary(lm1)

#While all the input variables are highly predictive,
#something seems to be missing in overall predictive accuracy:

Call:
  lm(formula = ComEd[, 16] ~ timcols)
Residuals:#
  #Min       1Q   Median       3Q      Max
#-1.11876 -0.16639 -0.01833  0.12886  1.64360
#Coefficients:
 # Estimate Std. Error t value Pr(>|t|)   
#(Intercept)     1.30253    0.02577  50.545  < 2e-16 ***
#  timcolsweekday -0.08813    0.02226  -3.959 7.93e-05 ***
 # timcolsTime     0.06901    0.00959   7.196 1.03e-12 ***
  # timcolsS1       0.37507    0.01450  25.863  < 2e-16 ***
#  timcolsC1      -0.17110    0.01424 -12.016  < 2e-16 ***
 # timcolsS2      -0.23303    0.01425 -16.351  < 2e-16 ***
#  timcolsC2      -0.37622    0.01439 -26.136  < 2e-16 ***
 # timcolsS3      -0.05689    0.01434  -3.967 7.65e-05 ***
#  timcolsC3       0.13164    0.01424   9.246  < 2e-16 ***
 # —

# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Residual standard error: 0.3679 on 1331 degrees of freedom
#Multiple R-squared: 0.6122,    Adjusted R-squared: 0.6099
#F-statistic: 262.7 on 8 and 1331 DF,  p-value: < 2.2e-16
#A plot of the residuals clearly shows something else is going on:
 # plot(lm1$residuals)


# As you may have already concluded there is seasonal heteroscedasticity in the data,
# beyond what we have fitted with the seasonal variables.
# Let’s load some temperature data for the Chicago area and see how it relates to this data.
##First we load the weather data from the NOAA database.
# It is stored in annual fixed width files which we stitch together:

# Load weather data from NOAA ftp site
KORDtemps<-NULL
for (yearNum in 2009:2013)
{
  ftpString<-paste(‘ftp://ftp.ncdc.noaa.gov/pub/data/gsod/’,yearNum,’/725300-94846-‘,yearNum,’.op.gz’,sep=”)
  fileString<-paste(‘KORD’,yearNum,’.gz’,sep=”)
  download.file(ftpString,fileString,mode=’wb’)
  temp2<-read.fwf(fileString,c(6,6,10,8,3,8,3,8,3,8,3,7,3,7,3,7,7,8,1,7,1,6,1,6,8),
                  skip=1,
                  col.names=c(‘STN’,’WBAN’,’YEARMODA’,’TEMP’,’TEMPCOUNT’,’DEWP’,’DEWPCOUNT’,’SLP’,’SLPCOUNT’,’STP’,’STPCOUNT’,’VISIB’,’VISIBCOUNT’,’WDSP’,’WDSPCOUNT’,’MAXSPD’,’GUST’,’MAX’,’MAXFLAG’,’MIN’,’MINFLAG’,’PRCP’,’PRCPFLAG’,’SNDP’,’FRSHTT’))
  KORDtemps<-rbind(KORDtemps,temp2)
}
# Change missing data to NAs
KORDtemps[KORDtemps==9999.9]<-NA
KORDtemps<-cbind(date=as.Date(as.character(KORDtemps[,3]),’%Y%m%d’),KORDtemps)
head(KORDtemps)
# create cross reference date index
dateInd<-sapply(ComEd[,1], function(x) which(KORDtemps[,1]==x))


#Next we plot the residuals from our regression vs. the prevailing max temperature 
#for the day:
  plot(KORDtemps[dateInd,”MAX”],lm1$residuals)

#  As expected there is something going on relating temperature to residuals.
#  In my next blog posting I will discuss approaches to fitting this weather data to the demand model…

  
  
#part 2
  
  #As hinted at in my last post, temperature is the missing variable to make sense of Residential electrical usage. 
  # Fortunately there are some reasonable freely available historical weather databases, most notably the one provided by NOAA.
  # As covered in the last posting, we can download this data for a particular location, in this case Chicago O’Hare airport, KORD.
 # Next we have to find a suitable model for the usage pattern we observed in the last posting.  Usage is lowest at around 70F, 
  # and rises slightly as temperatures fall but rises significantly as temperatures rise.  
 # Operationally we can imagine that as temperatures rise beyond what is comfortable, 
  # more and more cooling devices turn on to combat the heat. 
  # However, eventually all the air conditioning equipment is on at full capacity and incremental demand drops off.
  # Based on this, a logistic function [http://en.wikipedia.org/wiki/Logistic_function] seems appropriate.
 # Furthermore, depending on the time of day, the high temperature and the low temperature may both be influential predictors.
  # Continuing from the code presented in the last posting, we can fit a logistic function in temperature to the usage data
  #for each hour of the day, taking into account whether it is a weekday, the time of the year and the temperature function:
  
  
  allResults=NULL   # create the variable for results for each hour
  startList=list(intercept=1.5,weekday=-.02,Time=0,S1=.04,C1=-.03,S2=-.02,
                 C2=-.04,S3=.001,C3=.01,maxT=87,maxS=4,maxScalar=1,
                 maxSlope=0,minSlope=0) 
  # initial variable value
  for (hourInd in 1:24){  # iterate through hours; create a model for each hour
    test2<-nls(ComEd[,(hourInd+1)] ~ intercept 
               + timcols%*%c(weekday,Time,S1,C1,S2,C2,S3,C3)
               + maxSlope*KORDtemps[dateInd,”MAX”] 
               + minSlope*KORDtemps[dateInd,”MIN”]
               + maxScalar*plogis(KORDtemps[dateInd,”MAX”], 
                                  location = maxT, scale = maxS),
               start=startList,
               control=nls.control(maxiter = 500, tol = 1e-05, 
                                   minFactor = 1/10000,printEval = FALSE, warnOnly = TRUE))
    # using nonlinear least squares to find the best result
    
    allResults=rbind(allResults,coef(test2))  # combine all results
    
    startList=list(intercept=coef(test2)[1],weekday=coef(test2)[2],
                   Time=coef(test2)[3],S1=coef(test2)[4],
                   C1=coef(test2)[5],S2=coef(test2)[6],C2=coef(test2)[7],
                   S3=coef(test2)[8],C3=coef(test2)[9],maxT=coef(test2)[10],
                   maxS=coef(test2)[11],maxScalar=coef(test2)[12],
                   maxSlope=coef(test2)[13],minSlope=coef(test2)[14])
    # update starting value for next iteration to solution from previous hour
    # although not always necessary this reduces computational time
  }
  Then we can examine the regression results to see the average response function in usage based on Max temperature:
    maxResponse = mean(allResults[,’maxSlope’]) * KORDtemps[dateInd,”MAX”] 
  + mean(allResults[,’maxScalar’])*plogis(KORDtemps[dateInd,”MAX”],
                                          location = mean(allResults[,’maxT’]), 
                                          scale = mean(allResults[,’maxS’]), log = FALSE)
  
  plot(KORDtemps[dateInd,”MAX”],maxResponse)
  
 # In terms of fit we can revisit the chart from the previous blog posting 
  # by plotting the observed data for a particular hour, 
  # in this case midday, overlaid with the fitted data for the same inputs.
  
  
  # We can see that the fitted data (in black) overlay the actual data (in red) 
  #well but there are still differences for very hot days. 
  # So to summarize we can model residential power usage pretty accurately 
  # to first order based on knowledge of the high temperature for the day
  # as well as the time of the year, the day of the week and the hour of the day.  
  # As expected, as temperatures rise, usage grows dramatically over a high temperature of 80F.
  # Stay cool!
  
  
  #################
  
  #Before doing any exercises in R, load the fpp package using library(fpp).
  
  #We will reconcile the forecasts for the infant deaths data.
  #The fol- lowing code can be used. Check that you understand what each step is doing.
  #You will probably need to read the help files for some functions.
  
  library(hts)
  plot(infantgts)
  smatrix(infantgts)
  
  # Forecast 10-step-ahead and reconcile the forecasts
  infantforecast <- forecast(infantgts, h=10)
  
  # plot the forecasts including the last ten historical years
  plot(infantforecast, include=10)
  
  # Create a matrix of all aggregated time series
  allts_infant <- aggts(infantgts)
  
  # Forecast all series using ARIMA models
  allf <- matrix(, nrow=10, ncol=ncol(allts_infant))
  for(i in 1:ncol(allts_infant))
    allf[,i] <- forecast(auto.arima(allts_infant[,i]), h=10)$mean
  allf <- ts(allf, start=2004)
  # combine the forecasts with the group matrix to get a gts object
  y.f <- combinef(allf, groups = infantgts$groups)
  # set up training and testing samples
  data <- window(infantgts, start=1933, end=1993)
  test <- window(infantgts, start=1994, end=2003)
  # Compute forecasts on training data
  forecast <- forecast(data, h=10)
  # calculate ME, RMSE, MAE, MAPE, MPE and MASE
  accuracy.gts(forecast, test)
  
  
  