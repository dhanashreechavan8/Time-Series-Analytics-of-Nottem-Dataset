#Install necessary packages
install.packages("forecast")

#Load required packages
library(forecast)

#Load the data
data(nottem)
nottem
class(nottem)

#Check the frequency of the time series
frequency(nottem)

#Check for missing values
sum(is.na(nottem))

#Check the cycle
cycle(nottem)

#Plot the time series
plot(nottem,xlab="Date", ylab = "Average Temperatures (Fahrenheit)",
     main="Average Monthly Temperatures at Nottingham castle : 1920 - 1939")

#Check for rise in temperatures in particular months using boxplot
boxplot(nottem~cycle(nottem),xlab="Date", 
        ylab = "Average Temperatures" ,
        main ="Monthly Temperatures from 1920 to 1939")

#Monthly plot
monthplot(nottem, xlab="Date",
          ylab = "Average Temperatures" ,
          main ="Yearly Temperatures for each month from 1920 to 1939")

#decompose the time series into its components
decomposedNottem <- decompose(nottem)
plot(decomposedNottem)

#Adjust seasonal component
adjustDecomposedNottem <- nottem - decomposedNottem$seasonal

#Plot adjusted time series
plot(adjustDecomposedNottem,xlab="Date", ylab = "Average Temperatures (Fahrenheit)",
     main="Seasonally adjusted series")

#Durbin-Watson test for checking auto-corelation

#Install necessary packages
install.packages("lmtest")

#Load necessary packages
library(lmtest)

#Stored temprature in one dataframe
temp=as.data.frame(nottem)

#Stored year and month in another dataframe
yearmonth=as.yearmon(time(nottem))

yearmonth=as.data.frame(yearmonth)

#Combined Year and Month dataframe with temprature
Result=cbind(yearmonth,temp)

#Result of manipulated data frame
head(Result,12)

#Fit regression model for year month with temprature
test <- lm(Result$x ~ Result$yearmonth)
summary(test)

#Perform Durbin-Waton test to check auto-corelation
dwtest(test)

#Convert the data in time series object
myts <- ts(nottem, start=c(1920,1), end=c(1939,12), frequency=12)

#Check data is stationary
adf.test(nottem) 

#To search best paramters for arima without searching it manually used auto.arima
auto.arima(myts)

#auto.arima chooses the values of p,d,q with the lowest AIC value

#(p,d,q) values are: (1,0,2) and seasonal (1,1,2)

#Fit ARIMA model
reg <- Arima(myts,order=c(1,0,2),seasonal=c(1,1,2))

summary(reg)

#Check for auto-corelation in residuals in the model
acf(reg$residuals)

#Check for normality of the model
qqnorm(reg$residuals)

#forecast using the model for next 12 months
fc <- forecast(myts,model=reg,  h=12)
fc

#Plot historical data with forcast
autoplot(fc)

#Plot forecast with less historical data
autoplot(fc, include=100)


  
