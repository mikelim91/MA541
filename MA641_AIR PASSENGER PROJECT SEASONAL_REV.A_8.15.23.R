suppressPackageStartupMessages(library(xts))
install.packages("forecast")
install.packages("CADFtest", type = "source")
install.packages("TSA")
install.packages("locfit")
install.packages("curl", type = "binary")
install.packages("patchwork")
install.packages("gridExtra")
install.packages("ggpubr")
install.packages("rugarch")
install.packages("parallel")


library(forecast)
library(tidyverse)
library(quantmod)
library(TTR)
library(TSA)
library(xts)
library(tsibble)
library(ggplot2)
library(dplyr)
library(lubridate)
library(CADFtest)
library(patchwork)
library(gridExtra)
library(ggpubr)
library(rugarch)

apass <- airpassengers
apass


colSums(is.na(apass))



#Quick look into dataframe 
glimpse(apass)

# Pre-processing/Data-cleaning

#convert char string column to date type YYYY-MM-DD 

date_strings <- apass$Month
date_objects <- as.Date(paste(date_strings, "-01", sep = ""), format = "%Y-%m-%d")
date_objects
apass$Month <- date_objects

# Print the datetime object
print(date_objects)

#Quick look into dataframe 
glimpse(apass)

# Datacleaning 

## Number of NA values is 0
colSums(is.na(apass))


colnames(apass)



## air passenger Data Time Series
apass_ts <- ts(apass$Passengers, frequency = 12, start = c(1950,1))
glimpse(apass_ts)

apass_ts


# apassenger plot not stationary because its seasonal
plot(apass_ts, type = 'o', col = 'blue', main = 'Air Passenger from 1950-1962', ylab = 'Number of Passengers')
points(y = apass_ts, x = time(apass_ts) , pch = as.vector(season(apass_ts)))

# max lag is 12 because of monthly
CADFtest(apass_ts, type = "trend", max.lag.y = 12)



apass_diff <- diff(apass_ts)
plot(apass_diff, type = 'o', col = 'blue', main = 'Passengers from 1950-1962', ylab = 'Number of Passenger')
CADFtest(apass_diff)
adf.test(apass_diff)

plot(apass_diff)


## 1. Trimmed Data 1950-1960

apass_trim <- ts(apass$Passengers, frequency = 12, start = c(1950,1), end = c(1960,1))

plot(apass_trim, type = 'o', col = 'blue', main = 'Passengers from 1950-1960', ylab = 'Number of Passenger')

# Plot of original plot 1950-1962
plot(apass$Passengers, type = 'o', col = 'blue', main = 'Passengers from 1950-1962', ylab = 'Number of Passenger')
points(y = apass , x = time(apass), pch = as.vector(season(apass)))
CADFtest(apass, type = "trend", max.lag.y = 12)


## 2. Difference of trimmed data
apass_trim_diff <- diff(apass_trim, lag =12)

plot(apass_trim_diff, type = 'o', col = 'blue', main = 'Passengers from 1950-1960', ylab = 'Number of Passenger')
points(y = apass_trim , x = time(apass_trim), pch = as.vector(season(apass_trim)))
CADFtest(apass_trim_diff, type = "none", max.lag.y = 12)



# original difference plot and ADF test seasonal if you look at the repetiive pattern of months
plot(apass_diff, type = 'o', col = 'blue', main = 'First Difference Passengers from 1950-1962', ylab = 'Number of Passenger')
points(y = apass_diff , x = time(apass_diff), pch = as.vector(season(apass_diff)))
CADFtest(apass_diff, type = "none", max.lag.y = 12)



## 3. Seasonal difference of trimmed data -> plot is more stationary after second differencing
sdiff_apass <- diff(apass_trim_diff)

# Seasonal difference plot and ADF test
plot(sdiff_apass, type = 'o', col = 'blue', main = 'Seasonal Diference Passengers from 1950-1960', ylab = 'Number of Passenger')
points(y = sdiff_apass , x = time(sdiff_apass), pch = as.vector(season(sdiff_apass)))
CADFtest(sdiff_apass, type = "none", max.lag.y = 12)


## P & Q found after stationary

acf_pass <- acf(apass$Passengers, lag.max = 36)
plot(acf_pass, main = c('ACF for Passenger 1950-1962'))


pacf_pass <- pacf(apass$Passengers, lag.max = 36)
plot(pacf_pass, main = c('PACF for Passenger 1950-1962'))

arima(apass$Passengers) # AIC 1788.37 
arima(diff(apass$Passengers)) # AIC 1413.28


arima(subset_apass$Passengers) #AIC 1465.06
arima(diff(subset_apass$Passengers, differences = 1)) # AIC 1177.76 1 difference
arima(diff(subset_apass$Passengers, differences = 2)) # AIC 1210.56 2 difference 

eacf(diff(subset_apass$Passengers, differences = 1))
eacf(diff(subset_apass$Passengers, differences = 2))


### 8.20.23 non-trimmed data


## First seasonal difference
apass_fdiff <- diff(apass$Passengers, lag = 12)
plot(apass_fdiff, type = 'o', col = 'blue', main = 'First Seasonal Diference Passengers from 1950-1962', ylab = 'Number of Passenger')
CADFtest(apass_fdiff, type = 'none', max.lag.y = 12)


acf(apass_fdiff, main = 'First Seasonal Diference ACF Passengers from 1950-1962', ylab = 'Number of Passenger')
pacf(apass_fdiff, main = 'First Seasonal Diference PACF Passengers from 1950-1962', ylab = 'Number of Passenger')
eacf(apass_fdiff)



## Second seasonal difference
apass_sdiff <- diff(diff(apass$Passengers), lag = 12)
plot(apass_sdiff, type = 'o', col = 'blue', main = 'Second Seasonal Diference Passengers from 1950-1962', ylab = 'Number of Passenger')
CADFtest(apass_sdiff, type = 'none', max.lag.y = 12)


acf(apass_sdiff, main = 'Second Seasonal Diference ACF Passengers from 1950-1962', ylab = 'Number of Passenger')
pacf(apass_sdiff, main = 'Second Seasonal Diference PACF Passengers from 1950-1962', ylab = 'Number of Passenger')
eacf(apass_sdiff)


## ADF Full data comparison 
CADFtest(apass_fdiff, type = 'none', max.lag.y = 12)
CADFtest(apass_sdiff, type = 'none', max.lag.y = 12)


### 8.20.23 trimmed data


## First seasonal trimmed difference
apasstrim_fdiff <- diff(subset_apass$Passengers, lag = 12)
plot(apasstrim_fdiff, type = 'o', col = 'blue', main = 'First Seasonal Trimmed Diference Passengers from 1950-1960', ylab = 'Number of Passenger')
CADFtest(apasstrim_fdiff, type = 'none', max.lag.y = 12)
acf(apasstrim_fdiff, main = 'First Seasonal Trimmed Difference ACF Passengers from 1950-1960', ylab = 'Number of Passenger')
pacf(apasstrim_fdiff, main = 'First Seasonal Trimmed Difference PACF Passengers from 1950-1960', ylab = 'Number of Passenger')



## Second seasonal trimmed difference
apasstrim_sdiff <- diff(diff(subset_apass$Passengers), lag = 12)
plot(apasstrim_sdiff, type = 'o', col = 'blue', main = 'Second Seasonal Trimmed Diference Passengers from 1950-1960', ylab = 'Number of Passenger')
CADFtest(apasstrim_sdiff, type = 'none', max.lag.y = 12)
acf(apasstrim_sdiff, main = 'Second Seasonal Trimmed Difference ACF Passengers from 1950-1960', ylab = 'Number of Passenger')
pacf(apasstrim_sdiff, main = 'Second Seasonal Trimmed Difference PACF Passengers from 1950-1960', ylab = 'Number of Passenger')

eacf(apasstrim_sdiff)
eacf(apasstrim_fdiff)

auto.arima(apasstrim_fdiff)

auto.arima(apasstrim_sdiff)
eacf(apasstrim_sdiff)

arima(apass_trim)
adf.test(apass_trim)
arima(apasstrim_fdiff)
adf.test(apasstrim_fdiff)
arima(apasstrim_sdiff)
adf.test(apasstrim_sdiff)

apass_diff <- diff(apass$Passengers)
plot(apass_diff, type = 'o', col = 'blue',main = 'First Difference of Air Passengers from 1950-1960')

eacf(apasstrim_sdiff)

tsdiag(arima(apasstrim_sdiff), order = c(0,1,2), method = 'ML',gof = 10)


arima(apasstrim_fdiff)
tsdiag(arima(apasstrim_fdiff), order = c(0,1,1), method = 'ML',gof = 10)
arima(apasstrim_sdiff)


# tsdiag(arima(apasstrim_sdiff), order = c(0,2,1), method = 'ML',gof = 10)


## QQ plot looks normal but p-value form shapiro wilk is above 0.05 which means that that residuals are some what normally distributed
qqnorm(rstandard(arima(apasstrim_sdiff), order = c(0,1,1)))
qqline(rstandard(arima(apasstrim_sdiff), order = c(0,1,1)))
shapiro.test(rstandard(arima(apasstrim_sdiff), order = c(0,1,1))) #  residuals almost look normally distributed




## ljung-box 
LB.test(arima(apass_sdiff,order = c(0,1,1)),lag = 30)
runs(arima(apass_sdiff,order = c(0,1,1))) ## not working




apass$Passengers

## forecasting final
history_s <- window(apass$Passengers, start = c(144,1))
future_s <- window(apass$Passengers, end = c(145,1))
mod_s = Arima(history_s, order = c(0,1,1), method = 'ML') # 8.20 good
fc3 = forecast(mod_s, h = 10, level = c(95)) # 8.20 good
plot(fc3, type = 'b')
points(x =(133:144),y = future_s, pch = 4)


apass$Month

history2 = window(MSFT$Volume, start = 1260)
length(history2)
future2 = window(MSFT$Volume, end = 1261)
length(future2)


mod = Arima(history2, order = c(1,1,3), method = 'ML') # 8.20 good
fc2 = forecast(mod, h = 10, level = c(95)) # 8.20 good
plot(fc2, type = 'b')
points(x =(1261:1512),y = future2, pch = 4)




history3 = window(apass$Passengers, start = c(1959,1))
window(apass$Passengers, end = c(1959,1))

apass

length(history3)
future3 = window(MSFT$Volume, end = 1261)
length(future3)


mod = Arima(history2, order = c(1,1,3), method = 'ML') # 8.20 good
fc2 = forecast(mod, h = 10, level = c(95)) # 8.20 good
plot(fc2, type = 'b')
points(x =(1261:1512),y = future2, pch = 4)





##forecast debugging issue 
future3 = window(ts(apass$Passengers), start = c(1959,1))
history3 = window(ts(apass$Passengers), end = c(1960,12))
forecast(Arima(future2, order= c(0,1,2)),h = 10, level = c(95))
plot(forecast(Arima(history2, order= c(0,1,2)),h = 10, level = c(95)))







tsdiag(apasstrim_fdiff, order = c(0,1,1), method = 'ML'),gof = 10)

tsdiag(arima(diff(apass$Passengers), order = c(0,2,1), method = 'ML'),gof = 10)


plot(apass_trim)



acf(apass$Passengers)


acf(ts(apass_trim))

pacf(ts(apass_trim))


acf(apass_trim$Passengers, lag.max = 36) # 3 month period 


pacf(apass$Passengers, lag.max = 36) # 3 month period 

eacf(apass_diff) # possible (0,1) (1,0) (2,1)
auto.arima(apass_diff) #(2,0,1)(0,1,0) chose these pq, PQ values (dont rely on auto.arima )

#SARIMA pdq(0,1,2) PDQ(0,1,1)

?auto.arima


# differenced_pass <- diff(apass$Passengers, differences = 1)  # Adjust differences parameter as needed

## residual analysis
tsdiag(arima(diff(apass$Passengers), order = c(4,1,2), method = 'ML'),gof = 10) #residuals


# arima(diff(apass$Passengers), order = c(4,1,2))
# arima(diff(apass$Passengers), order = c(4,1,2), method = 'ML')
# tsdiag(arima(diff(apass$Passengers), order = c(4,1,2), method = 'ML'),gof = 10) #residuals


# residual low p-value residual not correlated  
hist(rstandard(arima(ts(apass$Passengers), order = c(4,1,2)))) # looks skewed right tail
runs(rstandard(arima(ts(apass$Passengers), order = c(4,1,2))))

# qq plot doesnt look normal but shapiro wilk test says residuals are normally distributed 
# after standardized data 
options(repr.plot.width = 10)
qqnorm(rstandard(arima(diff(apass$Passengers), order = c(4,1,2), method = 'ML')))
qqline(rstandard(arima(diff(apass$Passengers), order = c(4,1,2), method = 'ML')))
shapiro.test(rstandard(arima(diff(apass$Passengers), order = c(4,1,2), method = 'ML'))) # it is normal because p-value is greater
LB.test(arima(ts(apass$Passengers), order = c(4,1,2))) # low =-value not normally distributed


##forecast debugging issue 
future3 = window(ts(apass$Passengers), start = c(1959,1))
history3 = window(ts(apass$Passengers), end = c(1960,12))
forecast(Arima(future2, order= c(0,1,2)),h = 10, level = c(95))
plot(forecast(Arima(history2, order= c(0,1,2)),h = 10, level = c(95)))






arima(apass_trim$)


# Creating Subset of MSFT 

# Initialize new start and end date (ignore 2020 because of Covid)
start_date_apass <- as.Date("1950-01-01")
end_date_apass <- as.Date("1960-01-01")

# Subset of MSFT 
subset_apass <- apass %>%
  filter(Month >= start_date_apass, Month <= end_date_apass)
subset_apass


acf(diff(subset_apass$Passengers, differences = 1))

plot(subset_apass$Passengers, )
plot(diff(subset_apass$Passengers))
plot(diff(subset_apass$Passengers, differences = 2))

CADFtest(diff(subset_apass$Passengers, differences = 2))
CADFtest(diff(subset_apass$Passengers, differences = 1))

adf.test(diff(subset_apass$Passengers, differences = 2))
adf.test(diff(subset_apass$Passengers, differences = 1))


arima(subset_apass$Passengers)



apass

apass[length(apass)]


tail(apass, n=12) # extract last date



# Create data frames for plotting
diff_open_df <- data.frame(Date = apass$Month[-1], Differenced = differenced_pass)

diff_open_df


ggplot(data = diff_open_df, aes(x = Date, y = Differenced)) +
  geom_line() +
  labs(title = "Differenced Passengers")


acf(diff_open_df$Differenced)
pacf(diff_open_df$Differenced)
eacf(diff_open_df$Differenced)
auto.arima(diff_open_df$Differenced)
auto.arima(apass$Passengers)
















future2 = window(ts(apass$Passengers), start = 1955)
future2
history2
Arima(history2, order= c(0,1,2))
forecast(Arima(history2, order= c(0,1,2)),h = 10, level = c(95))
plot(forecast(Arima(history2, order= c(0,1,2)),h = 10, level = c(95)))











##forecast
history = window(ts(apass$Open), end = 2015)
future = window(ts(apass$Open), start = start_date)
future
history
Arima(history, order= c(0,1,2))
forecast(Arima(history, order= c(0,1,2)),h = 10, level = c(95))
plot(forecast(Arima(history, order= c(0,1,2)),h = 10, level = c(95)))



arima(ts(subset_apass$Open), order = c(1,1,0))$res

arima(ts(subset_apass$Open), order = c(1,1,0))$res^2
plot(arima(ts(subset_apass$Open), order = c(1,1,0))$res^2)
acf(arima(ts(subset_apass$Open), order = c(1,1,0))$res^2)
pacf(arima(ts(subset_apass$Open), order = c(1,1,0))$res^2)
eacf(arima(ts(subset_apass$Open), order = c(1,1,0))$res^2)
garch(arima(ts(subset_apass$Open), order





# Create differenced plots for each attribute
diff_open_plot <- ggplot(data = diff_open_df, aes(x = Date, y = Differenced)) +
  geom_line() +
  labs(title = "Differenced Open Prices")





# # Combine subplots using patchwork
# combined_apasss <- open_apass + close_apass + high_apass + low_apass + volume_apass
# 
# # Display the combined subplots
# combined_apasss


# Creating Subset of apass 

# # Initialize new start and end date (ignore 2020 because of Covid)
# start_date <- as.Date("2015-04-01")
# end_date <- as.Date("2019-12-31")

# # Subset of apass 
# subset_apass <- apass %>%
#   filter(Date >= start_date, Date <= end_date)


# # Time series for Open
# ts(subset_apass$Open)
# plot(x = subset_apass$Date,ts(subset_apass$Open), type = "l")
# subset_open_apass 


# Plot for Open
# open_plot2 <- ggplot(data = subset_apass, aes(x = Date, y = Open)) +
#   geom_line() +
#   labs(x = "Date", y = "Open", title = "Microsoft Stock Open Price Over Time 2")
# 
# # Plot for Close
# close_plot2 <- ggplot(data = subset_apass, aes(x = Date, y = Close)) +
#   geom_line() +
#   labs(x = "Date", y = "Close", title = "Microsoft Stock Close Price Over Time 2")
# 
# # Plot for High
# high_plot2 <- ggplot(data = subset_apass, aes(x = Date, y = High)) +
#   geom_line() +
#   labs(x = "Date", y = "High", title = "Microsoft Stock High Price Over Time 2")
# 
# # Plot for Low
# low_plot2 <- ggplot(data = subset_apass, aes(x = Date, y = Low)) +
#   geom_line() +
#   labs(x = "Date", y = "Low", title = "Microsoft Stock Low Price Over Time 2")
# 
# # Plot for Volume
# volume_plot2 <- ggplot(data = subset_apass, aes(x = Date, y = Volume)) +
#   geom_line() +
#   labs(x = "Date", y = "Volume", title = "Microsoft Stock Volume Over Time 2")
# 
# 
# # Combine the plots into a single plot with subplots
# combined_plots2 <- open_plot2 + close_plot2 + high_plot2 + low_plot2 + volume_plot2
# 
# # Display the combined subplots
# combined_plots2



# # ADF Test Example
# 
# 
# adf.test(subset_apass$Volume)
# acf(subset_apass$Volume)
# pacf(subset_apass$Volume)
# auto.arima(subset_apass$Volume)


# Perform ADF test
adf.test($Month)
print(paste("ADF Test for", attr, ":", adf_result$p.value))

# ACF plot
acf_plot <- acf(airpassengers[[attr]])
plot(acf_plot, main = paste("ACF for", attr))

# PACF plot
pacf_plot <- pacf(airpassengers[[attr]])
plot(pacf_plot, main = paste("PACF for", attr))





# List of attributes
attributes <- c("Passengers")

for (attr in attributes) {
  # Create a time series plot
  plot <- ggplot(data = airpassengers, aes(x = Date, y = .data[[attr]])) +
    geom_line() +
    labs(x = "Date", y = attr, title = paste("Microsoft Stock", attr, "Price Over Time"))
  
  # Display the plot
  print(plot)
  
  # Perform ADF test
  adf_result <- adf.test(airpassengers[[attr]])
  print(paste("ADF Test for", attr, ":", adf_result$p.value))
  
  # ACF plot
  acf_plot <- acf(airpassengers[[attr]])
  plot(acf_plot, main = paste("ACF for", attr))
  
  # PACF plot
  pacf_plot <- pacf(airpassengers[[attr]])
  plot(pacf_plot, main = paste("PACF for", attr))
  
  # Auto ARIMA model
  auto_arima_model <- auto.arima(airpassengers[[attr]])
  print(paste("Auto ARIMA Model for", attr, ":", auto_arima_model$arma))
}

auto.arima(airpassengers$Passengers)




# # Create a data frame for plotting
# diff_open_df <- data.frame(Date = subset_apass$Date[-1], DifferencedOpen = differenced_open)


# Apply differencing to make the time series stationary for each attribute
differenced_open <- diff(subset_apass$Open, differences = 1)  # Adjust differences parameter as needed
differenced_close <- diff(subset_apass$Close, differences = 1)
differenced_high <- diff(subset_apass$High, differences = 1)
differenced_low <- diff(subset_apass$Low, differences = 1)

# Create data frames for plotting
diff_open_df <- data.frame(Date = subset_apass$Date[-1], Differenced = differenced_open)
diff_close_df <- data.frame(Date = subset_apass$Date[-1], Differenced = differenced_close)
diff_high_df <- data.frame(Date = subset_apass$Date[-1], Differenced = differenced_high)
diff_low_df <- data.frame(Date = subset_apass$Date[-1], Differenced = differenced_low)

# Create differenced plots for each attribute
diff_open_plot <- ggplot(data = diff_open_df, aes(x = Date, y = Differenced)) +
  geom_line() +
  labs(title = "Differenced Open Prices")

diff_close_plot <- ggplot(data = diff_close_df, aes(x = Date, y = Differenced)) +
  geom_line() +
  labs(title = "Differenced Close Prices")

diff_high_plot <- ggplot(data = diff_high_df, aes(x = Date, y = Differenced)) +
  geom_line() +
  labs(title = "Differenced High Prices")

diff_low_plot <- ggplot(data = diff_low_df, aes(x = Date, y = Differenced)) +
  geom_line() +
  labs(title = "Differenced Low Prices")

# Combine the plots using patchwork
combined_plot3 <- diff_open_plot + diff_close_plot + diff_high_plot + diff_low_plot

# Display the combined plot
print(combined_plot3)





## diffence non-stationary, perform adf, acf, pacf and autoarima 

for (attr in attributes) {
  # Apply differencing to make the time series stationary for each attribute
  differenced_series <- diff(subset_apass[[attr]], differences = 1)  # Adjust differences parameter as needed
  
  # Create a data frame for plotting
  diff_df <- data.frame(Date = subset_apass$Date[-1], Differenced = differenced_series)
  
  # Create the differenced plot
  diff_plot <- ggplot(data = diff_df, aes(x = Date, y = Differenced)) +
    geom_line() +
    labs(title = paste("Differenced", attr, "Prices"))
  
  # Display the differenced plot
  print(diff_plot)
  
  # Perform ADF test
  adf_result <- adf.test(differenced_series)
  print(paste("ADF Test for", attr, ":", adf_result$p.value))
  
  # ACF plot
  acf_plot <- acf(differenced_series, plot = FALSE)
  plot(acf_plot, main = paste("ACF for", attr))
  
  # PACF plot
  pacf_plot <- pacf(differenced_series, plot = FALSE)
  plot(pacf_plot, main = paste("PACF for", attr))
  
  # Auto ARIMA model
  auto_arima_model <- auto.arima(differenced_series)
  print(paste("Auto ARIMA Model for", attr, ":", auto_arima_model$arma))
}

eacf(diff_df)



acf(diff(subset_apass$Open))
pacf(diff(subset_apass$Open))

eacf(diff(subset_apass$Open))

auto.arima(diff(subset_apass$Open))


arima(diff(subset_apass$Open), order = c(0,1,0))
arima(diff(subset_apass$Open), order = c(1,1,0))
arima(diff(subset_apass$Open), order = c(1,1,0))

eacf(subset_apass$Open)
arima(subset_apass$Open, order = c(1,1,0)) # AIC 3813 better model
arima(subset_apass$Open, order = c(3,1,0)) # AIC 3817
acf(subset_apass$Open)

eacf(ts(subset_apass$Open))
arima(ts(subset_apass$Open), order = c(1,1,0)) # AIC 3813 better model
arima(ts(subset_apass$Open), order = c(3,1,0)) # AIC 3817
acf(ts(subset_apass$Open))

eacf(diff(subset_apass$Open)) #0,1 1,0 2,0 3,0 5,0 6,0
arima(diff(subset_apass$Open), order = c(1,1,0)) # AIC 3813 better model
arima(diff(subset_apass$Open), order = c(3,1,0)) # AIC 3817
acf(diff(subset_apass$Open))
axis(1, at = seq(0,30))
pacf(diff(subset_apass$Open))
auto.arima(diff(subset_apass$Open))
arima(diff(subset_apass$Open), order = c(0,0,1))
arima(diff(subset_apass$Open), order = c(1,0,0))
arima(diff(subset_apass$Open), order = c(3,0,0))

acf(subset_apass$Open)
axis(1, at = seq(0,30,))

auto.arima(subset_apass$Open)
eacf(subset_apass$Open)

arima(diff(subset_apass$Open), order = c(1,1,0))
arima(diff(subset_apass$Open), order = c(1,1,0))
arima(diff(subset_apass$Open), order = c(1,1,0), method = 'ML')
tsdiag(arima(diff(subset_apass$Open), order = c(1,1,0), method = 'ML'),gof = 10) #residuals

options(repr.plot.width = 10)
qqnorm(rstandard(arima(diff(subset_apass$Open), order = c(1,1,0), method = 'ML')))
qqline(rstandard(arima(diff(subset_apass$Open), order = c(1,1,0), method = 'ML')))
shapiro.test(rstandard(arima(diff(subset_apass$Open), order = c(1,1,0))))

options(repr.plot.width = 10)
qqnorm(rstandard(arima(ts(subset_apass$Open), order = c(1,1,0)), method = 'ML'))
qqline(rstandard(arima(ts(subset_apass$Open), order = c(1,1,0)), method = 'ML'))
shapiro.test(rstandard(arima(ts(subset_apass$Open), order = c(1,1,0)), method = 'ML'))

hist(rstandard(arima(ts(subset_apass$Open), order = c(1,1,0))))
LB.test(arima(ts(subset_apass$Open), order = c(1,1,0)))
LB.test(arima(ts(subset_apass$Open), order = c(1,1,0)))
runs(rstandard(arima(ts(subset_apass$Open), order = c(1,1,0))))


##forecast
history = window(ts(subset_apass$Open), end = end_date)
future = window(ts(subset_apass$Open), start = start_date)
future
history
Arima(history, order= c(0,1,2))
forecast(Arima(history, order= c(0,1,2)),h = 10, level = c(95))
plot(forecast(Arima(history, order= c(0,1,2)),h = 10, level = c(95)))



##forecast
history = window(ts(apass$Open), end = 2015)
future = window(ts(apass$Open), start = start_date)
future
history
Arima(history, order= c(0,1,2))
forecast(Arima(history, order= c(0,1,2)),h = 10, level = c(95))
plot(forecast(Arima(history, order= c(0,1,2)),h = 10, level = c(95)))



arima(ts(subset_apass$Open), order = c(1,1,0))$res

arima(ts(subset_apass$Open), order = c(1,1,0))$res^2
plot(arima(ts(subset_apass$Open), order = c(1,1,0))$res^2)
acf(arima(ts(subset_apass$Open), order = c(1,1,0))$res^2)
pacf(arima(ts(subset_apass$Open), order = c(1,1,0))$res^2)
eacf(arima(ts(subset_apass$Open), order = c(1,1,0))$res^2)
garch(arima(ts(subset_apass$Open), order = c(1,1,0))$res^2)


CADFtest(diff(subset_apass$Open), type = "none")


glimpse(diff_df)




# Fit an ARCH model
arch_model <- g

garch(diff_df)


ugarchspec(variance.model = list(model = "sGARCH"), mean.model = list(armaOrder = c(0, 0)))

ugarchfit(spec = garch_spec, data = returns_ts)
summary(garch_model)
summary(arch_model)
# Load the rugarch package
library(rugarch)

# Convert data to a time series object
diff_open <- ts(diff_open_df)

# Specify the GARCH model specification
garch_spec <- ugarchspec(variance.model = list(model = "sGARCH"), mean.model = list(armaOrder = c(0, 0)))

# Fit the GARCH model
garch_model <- ugarchfit(spec = dif, data = diff_open)
summary(garch_model)



##GARCH shapiro wilk test, QQ, forecasting , ljung box




install.packages("rugarch")
# Load the rugarch package
library(rugarch)

# Simulated financial returns data (replace this with your actual data)
set.seed(123)
returns <- rnorm(1000)

# Convert data to time series
returns_ts <- ts(returns)

# Specify the model orders
p <- 1  # Lag order for ARCH and GARCH terms
q <- 1  # Lag order for MA term

# ARCH Model
arch_spec <- ugarchspec(variance.model = list(model = "ARCH", lag = p))
arch_fit <- ugarchfit(spec = arch_spec, data = returns_ts)

# GARCH Model
garch_spec <- ugarchspec(variance.model = list(model = "GARCH", garchOrder = c(p, q)))
garch_fit <- ugarchfit(spec = garch_spec, data = returns_ts)

# Summarize the models
summary(arch_fit)
summary(garch_fit)
library(lmtest)
Box.test(residuals, lag = 9, type = "Ljung-Box")
residuals <- residuals(model)
model <- arima(data, order = c(3, 0, 0))

















arch_model <- arch(returns, order = 1)
summary(arch_model)







auto.arima(apass$Open)
auto.arima(differenced_open)




auto.arima(differenced_close)
auto.arima(differenced_open)

auto.arima(subset_apass$Open)

## anotherway to difference 
CADFtest(diff(subset_apass$Open), type = "none", max.lag.y = 8)

plot(diff(subset_apass$Open), type = "l",
     main = "difference rate over time",
     ylab = "difference rate", cex = 1.5, xaxp = c(1951,2019,68))

acf(diff(subset_apass$Open))
pacf(diff(subset_apass$Open))

eacf(diff(subset_apass$Open))


open_arima_model <- auto.arima(subset_apass$Open)
print(open_arima_model)

auto.arima(differenced_open)

auto.arima(subset_apass$Open, allowdrift = T)
auto.arima(subset_apass$Open, allowdrift = F)


arima(subset_apass$Open, order = c(4,1,2))


?auto.arima

arima(differenced_open, order = c(4,1,2))

?eacf











# Looking at Monthly Volume 
subset_apass_monthly <- subset_apass %>%
  group_by(YearMonth = floor_date(Date, unit = "month")) %>%
  summarise(MonthlyVolume = sum(Volume))

plot(x = subset_apass_monthly$YearMonth, y = subset_apass_monthly$MonthlyVolume, type = "l",
     xlab = "Date", ylab = "Volume", main = "Microsoft Stock Volume Over Months (Subset)")

# ACF & PACF
acf(apass$Close)


# ADF Test
adf.test(subset_apass$Volume)


# Daily return percentage
((subset_apass$Open - subset_apass$Close) / subset_apass$Open) * 100

plot(x = subset_apass$Date,ts_volume_monthly, type = "l")

subset_apass_monthly

acf(apass$Volume,50)
pacf(apass$Volume,50)
plot(apass$Volume, type = "l")

adf.test(apass$Volume) # p=value less than 0.05 so reject null hypothesis

adf.test(apass$Close)



acf(ts_volume_monthly)
pacf(ts_volume_monthly)

ts_volume_monthly <- ts(subset_apass$Volume, frequency = 12)

adf.test(diff(ts_volume))
plot(diff(ts_volume))

arima(diff(ts_volume))


acf(diff(ts_volume))
pacf(diff(ts_volume))


checkresiduals(arima(diff(ts_volume)))


acf(diff(ts_volume, lag.max= 20))


pacf(diff(ts_volume, lag.max= 20))




apass <- apass %>% column_to_rownames(., var = 'Date')
head(apass)
colnames(apass)

plot(apass$Volume, type = "l")
plot(x = apass$Date, y =apass$Volume, type = "l")
plot(x = apass$Date, y =apass$Close, type = "l")

plot(apass$Date, apass$Volume, type  = 'l')

my_data <- data.frame(dates = seq(as.Date("2015-01-04"),  # Create example data frame
                                  by = "day",
                                  length.out = 10),
                      values = 1:10)
data.frame(dates = seq(as.Date("2015-01-04"),  # Create example data frame
                       by = "day",
                       length.out = 10),
           values = 1:10)


apass$DateColumn <- as.Date(apass$DateColumn)





subset_apass <- apass %>%
  filter(DateColumn >= start_date, DateColumn <= end_date)

subset_apass_monthly <- subset_apass %>%
  group_by(YearMonth = floor_date(DateColumn, unit = "month")) %>%
  summarise(MonthlyVolume = sum(Volume))


plot(x = subset_apass_monthly$YearMonth, y = subset_apass_monthly$MonthlyVolume, type = "l",
     xlab = "Date", ylab = "Volume", main = "Microsoft Stock Volume Over Months (Subset)")

install.packages("forecast")
install.packages("TSA")
install.packages("quantmod")






pacf(apass$Volume,20)
pacf(apass$Volume,100)

head(airpassengers)
plot(airpassengers$Passengers )

acf(airpassengers$Passengers,100)
pacf(airpassengers$Passengers,200)

library(dplyr)
library(lubridate)

colnames(apass)


apass$DateColumn <- as.Date(apass$DateColumn)

apass %>%
  group_by(YearMonth = floor_date(Date, unit = "month")) %>%
  summarise(MonthlyVolume = sum(Volume))


acf(apass$Volume)
pacf(apass$Volume)



head(airpassengers)

acf(apass$Volume,200)
##
head(airpassengers)
apass <- airpassengers
plot(x = apass$Month, y = apass$Passengers)
apass$Month


ts_volume <- ts(apass2$Volume, frequency =12)

arima(diff(ts_volume))
checkresiduals(arima(diff(ts_volume)))

colnames(apass2)

apass2[1]
adf.test(apass$Volume) # p=value less than 0.05 so reject null hypothesis

adf.test(apass$Close)

ts_volume <- ts(apass2$Volume, frequency =12)

arima(diff(ts_volume))
checkresiduals(arima(diff(ts_volume)))


plot(x = apass$Date, y =apass$Open, type = "l")
plot(x = apass$Date, y =apass$Volume, type = "l")
plot(x = apass$Date, y =apass$Close, type = "l")


acf(apass$Volume,200)
pacf(apass$Volume,200)

head(airpassengers)
apass <- airpassengers
plot(x = apass$Month, y = apass$Passengers)
apass$Month

