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




MSFT <- msft

#Quick look into dataframe 
glimpse(MSFT)

# Pre-processing/Data-cleaning

# Convert char string Date column to date type YYYY-MM-DD 
MSFT$Date <- as.Date(MSFT$Date) 
MSFT$Date

# Quick look into dataframe 
glimpse(MSFT)

# Number of NA values is 0
colSums(is.na(MSFT))


# Time series for Open, Close, High, Low and Volume & daily return pctg

open_plot <- ggplot(data = MSFT, aes(x = Date, y = Open)) +
  geom_line() +
  labs(x = "Date", y = "Open Price", title = "Microsoft Stock Open Price Over Time")

close_plot <- ggplot(data = MSFT, aes(x = Date, y = Close)) +
  geom_line() +
  labs(x = "Date", y = "Close Price", title = "Microsoft Stock Close Price Over Time")

high_plot <- ggplot(data = MSFT, aes(x = Date, y = High)) +
  geom_line() +
  labs(x = "Date", y = "High Price", title = "Microsoft Stock High Price Over Time")

low_plot <- ggplot(data = MSFT, aes(x = Date, y = Low)) +
  geom_line() +
  labs(x = "Date", y = "Low Price", title = "Microsoft Stock Low Price Over Time")

volume_plot <- ggplot(data = MSFT, aes(x = Date, y = Volume)) +
  geom_line() +
  labs(x = "Date", y = "Volume", title = "Microsoft Stock Volume Over Time")

## New feature created daily return pct
daily_return <- ((MSFT$Open - MSFT$Close) / MSFT$Open ) * 100

# plot of daily return of stocks
dailyreturn_plot <- ggplot(data = MSFT, aes(x = Date, y = daily_return)) +
  geom_line() +
  labs(x = "Date", y = "daily_return", title = "Microsoft Stock Daily Return Over Time")

combined_plots <- open_plot + close_plot + high_plot + low_plot + volume_plot + dailyreturn_plot

# Display the combined subplots
combined_plots


# Creating Subset of MSFT 

# Initialize new start and end date (ignore 2020 because of Covid)
start_date <- as.Date("2015-04-01")
end_date <- as.Date("2019-12-31")

# Subset of MSFT 
subset_MSFT <- MSFT %>%
  filter(Date >= start_date, Date <= end_date)

# daily return of subset of MSFT
daily_return_subset_MSFT <- ((subset_MSFT$Open - subset_MSFT$Close) / subset_MSFT$Open ) * 100

# # Time series for Open
# ts(subset_MSFT$Open)
# plot(x = subset_MSFT$Date,ts(subset_MSFT$Open), type = "l")
# subset_open_plot 
  
# Plot of subset of MSFT 2015-2019  
# Plot for Open
open_plot2 <- ggplot(data = subset_MSFT, aes(x = Date, y = Open)) +
  geom_line() +
  labs(x = "Date", y = "Open", title = "Microsoft Stock Open Price Over Time 2")

# Plot for Close
close_plot2 <- ggplot(data = subset_MSFT, aes(x = Date, y = Close)) +
  geom_line() +
  labs(x = "Date", y = "Close", title = "Microsoft Stock Close Price Over Time 2")

# Plot for High
high_plot2 <- ggplot(data = subset_MSFT, aes(x = Date, y = High)) +
  geom_line() +
  labs(x = "Date", y = "High", title = "Microsoft Stock High Price Over Time 2")

# Plot for Low
low_plot2 <- ggplot(data = subset_MSFT, aes(x = Date, y = Low)) +
  geom_line() +
  labs(x = "Date", y = "Low", title = "Microsoft Stock Low Price Over Time 2")

# Plot for Volume
volume_plot2 <- ggplot(data = subset_MSFT, aes(x = Date, y = Volume)) +
  geom_line() +
  labs(x = "Date", y = "Volume", title = "Microsoft Stock Volume Over Time 2")

# Plot for Daily Return
dailyreturn_plot2 <- ggplot(data = subset_MSFT, aes(x = Date, y = daily_return_subset_MSFT)) +
  geom_line() +
  labs(x = "Date", y = "Volume", title = "Microsoft Stock Volume Over Time 2")


# Combine the plots of time frame 2015 - 2019
combined_plots2 <- open_plot2 + close_plot2 + high_plot2 + low_plot2 + volume_plot2 + dailyreturn_plot2

# Display the combined subplots of time frame 2015 - 2019
combined_plots2


# ADF Test Example


# adf.test(subset_MSFT$Volume)
# acf(subset_MSFT$Volume)
# pacf(subset_MSFT$Volume)
# auto.arima(subset_MSFT$Volume)

# Returns ADF, ACF, PACF of subset of MSFT 2015-2019
# List of attributes
attributes <- c("Open", "Close", "High", "Low", "Volume")

for (attr in attributes) {
  # Create a time series plot
  plot <- ggplot(data = subset_MSFT, aes(x = Date, y = .data[[attr]])) +
    geom_line() +
    labs(x = "Date", y = attr, title = paste("Microsoft Stock", attr, "Price Over Time"))
  
  # Display the plot
  print(plot)
  
  # Perform ADF test
  adf_result <- adf.test(subset_MSFT[[attr]])
  print(paste("ADF Test for", attr, ":", adf_result$p.value))
  
  # ACF plot
  acf_plot <- acf(subset_MSFT[[attr]])
  plot(acf_plot, main = paste("ACF for", attr))
  
  # PACF plot
  pacf_plot <- pacf(subset_MSFT[[attr]])
  plot(pacf_plot, main = paste("PACF for", attr))
  
  # Auto ARIMA model
  auto_arima_model <- auto.arima(subset_MSFT[[attr]])
  print(paste("Auto ARIMA Model for", attr, ":", auto_arima_model$arma))
}

# auto.arima(subset_MSFT$Volume)
acf(MSFT$Volume, lag.max = 1511)
acf(subset_MSFT$Volume, lag.max = 1200, title = "ACF for Volume")
pacf(subset_MSFT$Volume, lag.max = 251 )


# # Create a data frame for plotting
# diff_open_df <- data.frame(Date = subset_MSFT$Date[-1], DifferencedOpen = differenced_open)


# Apply differencing to make the time series stationary for each attribute
differenced_open <- diff(subset_MSFT$Open, differences = 1)  # Adjust differences parameter as needed
differenced_close <- diff(subset_MSFT$Close, differences = 1)
differenced_high <- diff(subset_MSFT$High, differences = 1)
differenced_low <- diff(subset_MSFT$Low, differences = 1)

# Create data frames for plotting
diff_open_df <- data.frame(Date = subset_MSFT$Date[-1], Differenced = differenced_open)
diff_close_df <- data.frame(Date = subset_MSFT$Date[-1], Differenced = differenced_close)
diff_high_df <- data.frame(Date = subset_MSFT$Date[-1], Differenced = differenced_high)
diff_low_df <- data.frame(Date = subset_MSFT$Date[-1], Differenced = differenced_low)

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

# Plot for Volume
volume_plot2 <- ggplot(data = subset_MSFT, aes(x = Date, y = Volume)) +
  geom_line() +
  labs(x = "Date", y = "Volume", title = "Microsoft Stock Volume Over Time")


# Combine the plots using patchwork
combined_plot3 <- diff_open_plot + diff_close_plot + diff_high_plot + diff_low_plot + volume_plot2

# Display the combined plot
print(combined_plot3)

# # Differenced Daily return no need to since its stationary 
# differenced_daily_return <- diff(daily_return_subset_MSFT, differences = 1)
# differenced_daily_return
# 
# plot(daily_return_subset_MSFT, type = 'l')
# plot(differenced_daily_return, type = 'l')


# ## Original dataset ADF test
# CADFtest(subset_MSFT)
# 
# 
# 
# auto.arima((subset_MSFT$Open - subset_MSFT$Close / subset_MSFT$Open ) * 100)
# 
# ## Differenced dataset ADF test
# CADFtest((subset_MSFT$Open - subset_MSFT$Close / subset_MSFT$Open ) * 100,allowdrift = F)
# 
# CADFtest(differenced_daily_return,allowdrift = F)
# 
# # p-value > 0.05 non-stationary
# auto.arima((subset_MSFT$Open - subset_MSFT$Close / subset_MSFT$Open ) * 100,allowdrift = F)
# 
# # p-value less than 0.05 stationary 
# auto.arima(differenced_daily_return,allowdrift = F) 
# 
# eacf(differenced_daily_return)





## diffence non-stationary, perform adf, acf, pacf and autoarima 

for (attr in attributes) {
  # Apply differencing to make the time series stationary for each attribute
  differenced_series <- diff(subset_MSFT[[attr]], differences = 1)  # Adjust differences parameter as needed
  
  # Create a data frame for plotting
  diff_df <- data.frame(Date = subset_MSFT$Date[-1], Differenced = differenced_series)
  
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



# eacf(diff_df)
# 
# 
# 
# acf(diff(subset_MSFT$Open))
# pacf(diff(subset_MSFT$Open))
# 
# eacf(diff(subset_MSFT$Open))
# 
# auto.arima(diff(subset_MSFT$Open))


# arima(diff(subset_MSFT$Open), order = c(0,1,0))
# arima(diff(subset_MSFT$Open), order = c(1,1,0))
# arima(diff(subset_MSFT$Open), order = c(1,1,0))


# eacf(ts(subset_MSFT$Open))
# arima(ts(subset_MSFT$Open), order = c(1,1,0)) # AIC 3813 better model
# arima(ts(subset_MSFT$Open), order = c(3,1,0)) # AIC 3817
# acf(ts(subset_MSFT$Open))

# # ACF & PACF Of Original Subset
# 
# acf(subset_MSFT)
# # pacf(subset_MSFT)
# 
# auto.arima(subset_MSFT$Open,allowdrift = F )

eacf(diff(subset_MSFT$Volume))



#
# 8.20.23: IMA (0,1,2) ARIMA(1,1,2) (1,1,1) (1,1,3) 
IMA2 <- arima(subset_MSFT$Volume , order = c(0,1,2), method = 'ML')
IMA2 #42190.06
IMA2$coef[1] # ma1 -0.5583
IMA2$coef[2] # ma2
IMA2$coef[1] / 0.0283 # if this is less than 1.96 then consider IMA(1,2) with theta = 0
#yt-y_t-1 = et -0.5583et-1 + 0.2227et-2


SubIMA2 <- arima(subset_MSFT$Volume, order = c(0,1,2), method = 'ML', fixed = c(0,NA))
SubIMA2 # 8.20.23: AIC 42488.73

ARIMA12 <- arima(subset_MSFT$Volume , order = c(1,1,2), method = 'ML')
ARIMA12 #AIC 42142.37
ARIMA12$coef[1] # ma1 0.702341 
ARIMA12$coef[1]/0.0533 # 13.177 is greater than 1.96 then AR component is significant 
ARIMA12$coef[2]/0.0684 # abs(-66.60926 ) > 1.96 signifcant 
#yt-yt-1 = 0.702341yt-1 + et + 1.2806et-1 - 0.29574et-2

ARIMA11 <- arima(subset_MSFT$Volume , order = c(1,1,1), method = 'ML')
ARIMA11 #AIC 42156.76
ARIMA11$coef[1] # ar1 0.4332 
ARIMA11$coef[1]/0.0325 # 13.32951 is greater than 1.96 then AR component is significant 
ARIMA11$coef[2]/0.0143 # abs(-18.722) > 1.96 signifcant 
#yt-yt-1 = 0.4332 yt-1 + et + 0.9525et-1 

ARIMA13 <- arima(subset_MSFT$Volume , order = c(1,1,3), method = 'ML')
ARIMA13 #AIC 42142.11
ARIMA13$coef[1] # ar1 0.7681   
ARIMA13$coef[1]/0.0564 # 13.61821 is greater than 1.96 then AR component is significant 
ARIMA13$coef[2]/0.0640   # abs(-20.88465 ) > 1.96 signifcant 
#yt-yt-1 = 0.7681yt-1 + et + 1.3366et-1 + 1.3366et-1 -0.2888et-2 - 0.0585et-3

## Choosing BEST AIC
aics = cbind(c(round(ARIMA12$aic,2),round(ARIMA11$aic,2),round(ARIMA13$aic,2)))
models = cbind(c('ARIMA(1,1,1)','ARIMA(1,1,2)','ARIMA(1,1,3)'))
results <- cbind(models,aics)
colnames(results)  =c('Model', ' AIC')
results = data.frame(results)
results

results[order(results[,2]),]
## residual analysis
SubArima13 <- arima(subset_MSFT$Volume, order = c(1,1,3), method = 'ML')

tsdiag(SubArima13, gof = 10, omit.initial = F, lwd = 2)

## QQ plot looks normal but p-value form shapiro wilk is low which means that that residuals are not normally distributed
qqnorm(rstandard(SubArima13))
qqline(rstandard(SubArima13))
shapiro.test(rstandard(SubArima13)) # low p-value means residuals are not normally distributed

## ljung-box 
LB.test(SubArima13,lag = 30)
runs(SubArima13) ## not working

## qq final


qqnorm(rstandard(arima(diff(subset_MSFT$Open), order = c(0,1,1), method = 'ML')))
qqline(rstandard(arima(diff(subset_MSFT$Open), order = c(0,1,1), method = 'ML')))
shapiro.test(rstandard(arima(diff(subset_MSFT$Open), order = c(0,1,1))))

## forecasting final

history2 = window(MSFT$Volume, start = 1260)
length(history2)
future2 = window(MSFT$Volume, end = 1261)
length(future2)


mod = Arima(history2, order = c(1,1,3), method = 'ML') # 8.20 good
fc2 = forecast(mod, h = 10, level = c(95)) # 8.20 good
plot(fc2, type = 'b')
points(x =(1261:1512),y = future2, pch = 4)


## GARCH Model

res_SubArima13 = SubArima13$res
sq_res_ARIMA13 <- res_SubArima13^2
par(mfcol=c(3,1))
plot(sq_res_ARIMA13, main = 'Squared Residuals')
acf_sq <- acf(sq_res_ARIMA13, main = 'ACF Squared Residuals', lag.max = 40)
pacf_sq <- pacf(sq_res_ARIMA13, main = 'PACF Squared Residuals', lag.max = 40)
eacf(sq_res_ARIMA13) # a lot of white noise which means GARCH is not suitable for model for time series











window(subset_MSFT$Volume, start = 1)
window(subset_MSFT$Volume, start = 1197)
length(window(subset_MSFT$Volume, start = 1197))
points(x =(1:1198),y = window(subset_MSFT$Volume, start = 1197), pch = 4)


Box.test(SubArima13$residuals, lag = 30, type = "Ljung-Box")



IMA2$

IMA2$ma1
IMA2_t


typeof(ts(subset_MSFT$Volume))
ts(subset_MSFT$Volume)

auto.arima(diff(subset_MSFT$Open))

# EACF for Volume price after difference
eacf(subset_MSFT$Volume)
auto.arima(subset_MSFT$Volume)
adf.test(subset_MSFT$Volume)
?auto.arima()

eacf(subset_MSFT$Volume)
pacf(subset_MSFT$Volume)
acf(arima(subset_MSFT))

eacf(diff(subset_MSFT$Volume))

acf(diff(subset_MSFT$Volume))

pacf(diff(subset_MSFT$Volume))

adf.test(diff(subset_MSFT$Volume))

eacf(diff(subset_MSFT$Open)) # EACF after possible (0,1) (1,0) 

CADFtest(diff(subset_MSFT$Volume))



adf.test(subset_MSFT$Volume)
adf.test(diff(subset_MSFT$Volume))
arima(subset_MSFT$Volume) #42671.29 
arima(diff(subset_MSFT$Volume)) # 42496.58 
eacf(subset_MSFT$Volume)
eacf(diff(subset_MSFT$Volume))

tsdiag(arima(diff(subset_MSFT$Volume), order = c(1,1,1), method = 'ML'),gof = 10) #ARIMA (1,1,1)
arima(subset_MSFT$Volume, order = c(1,0,1), method = 'ML')
arima(subset_MSFT$Volume, order = c(1,1,1), method = 'ML') # 42156
arima(subset_MSFT$Volume, order = c(1,1,1), method = 'ML')$aic
arima(diff(subset_MSFT$Volume, order = c(1,1,1), method = 'ML'))$aic
# Fit ARIMA model without differencing
arima_model_1 <- arima(subset_MSFT$Volume, order = c(1,0,1), method = 'ML')

# Fit ARIMA model with first-order differencing
diffed_volume <- diff(subset_MSFT$Volume)
arima_model_2 <- arima(diffed_volume, order = c(1,1,1), method = 'ML')

# Get AIC values
AIC_1 <- arima_model_1$aic
AIC_2 <- arima_model_2$aic

# Print AIC values for comparison
print(paste("AIC for ARIMA(1,0,1):", AIC_1))
print(paste("AIC for ARIMA(1,1,1) with differencing:", AIC_2))




tsdiag(arima(diff(subset_MSFT$Volume), order = c(0,1,2), method = 'ML'),gof = 10) # IMA(1,2)
arima(subset_MSFT$Volume, order = c(0,1,2), method = 'ML') # 42190
arima(subset_MSFT$Volume, order = c(0,1,2), method = 'ML')$aic
arima(diff(subset_MSFT$Volume, order = c(0,1,2), method = 'ML'))$aic

tsdiag(arima(diff(subset_MSFT$Volume), order = c(1,1,2), method = 'ML'),gof = 10) #ARIMA (1,1,2) 
arima(subset_MSFT$Volume, order = c(1,1,2), method = 'ML') # 42190
arima(subset_MSFT$Volume, order = c(1,1,2), method = 'ML')$aic
arima(diff(subset_MSFT$Volume, order = c(1,1,2), method = 'ML'))$aic

tsdiag(arima(diff(subset_MSFT$Volume), order = c(2,1,2), method = 'ML'),gof = 10) #ARIMA (2,1,2)
arima(subset_MSFT$Volume, order = c(2,1,2), method = 'ML') # 42142.35


tsdiag(arima(diff(subset_MSFT$Volume), order = c(0,1,3), method = 'ML'),gof = 10) #IMA (0,1,3) 

tsdiag(arima(diff(subset_MSFT$Volume), order = c(1,1,3), method = 'ML'),gof = 10) #ARIMA (1,1,3) ## good
arima(subset_MSFT$Volume, order = c(1,1,3), method = 'ML') #ARIMA (1,1,3) ## good


tsdiag(arima(diff(subset_MSFT$Volume), order = c(1,1,3), method = 'ML'),gof = 10) 


tsdiag(arima(diff(subset_MSFT$Volume), order = c(1,1,3), method = 'ML'),gof = 10) # 8.19 new order (1,3)

tsdiag(arima(subset_MSFT$Volume, order = c(1,1,3), method = 'ML'),gof = 30)
tsdiag(arima(subset_MSFT$Volume, order = c(1,1,3), method = 'ML'),gof = 30)

tsdiag(arima(subset_MSFT$Volume, order = c(1,1,3), method = 'ML'),gof = 10)

tsdiag(arima(subset_MSFT$Volume, order = c(0,1,1), method = 'ML'),gof = 10)

arima(subset_MSFT$Volume, order = c(1, 1, 3),method = "ML")
pacf(subset_MSFT$Volume)

shapiro.test(diff(subset_MSFT$Volume))
shapiro.test(subset_MSFT$Volume)


shapiro.test(arima(diff(subset_MSFT$Volume), order = c(1,1,1)))

arima(diff(subset_MSFT$Open), order = c(0,1,1)) # AIC 3829 (better model)
arima(diff(subset_MSFT$Open), order = c(1,1,0)) # AIC 4353
auto.arima(diff(subset_MSFT$Open)) # p& q value is (0,1) double check

acf(diff(subset_MSFT$Open))
axis(1, at = seq(0,30))

pacf(diff(subset_MSFT$Open))
axis(1, at = seq(0,30))

arima(subset_MSFT$Volume, order = c(1,0,3), method = 'ML')
arima(diff(subset_MSFT$Volume, order = c(1,1,3), method = 'ML'))






# residual plot for Open price ACF, Ljung-box
# not normally distributed


hist(rstandard(arima(diff(subset_MSFT$Open), order = c(0,1,1))))
LB.test(arima(diff(subset_MSFT$Open), order = c(0,1,1)))

tsdiag(arima(diff(subset_MSFT$Open), order = c(0,1,1), method = 'ML'),gof = 10) #residuals 



# Shapiro wilk test
shapiro.test(rstandard(arima(diff(subset_MSFT$Open), order = c(0,1,1))))

## QQ plot looks normal but p-value form shapiro wilk is low which means that that residuals are not normally distributed
qqnorm(rstandard(arima(diff(subset_MSFT$Open), order = c(0,1,1), method = 'ML')))
qqline(rstandard(arima(diff(subset_MSFT$Open), order = c(0,1,1), method = 'ML')))
shapiro.test(rstandard(arima(diff(subset_MSFT$Open), order = c(0,1,1))))



arima(subset_MSFT$Volume)

arima(diff(subset_MSFT$Volume))




auto.arima(diff(subset_MSFT$Volume)) # AIC 42144 (1,0,3)

runs(rstandard(arima(diff(subset_MSFT$Open), order = c(0,1,1))))

?rstandard.Arima



# auto.arima(diff(subset_MSFT$Open))
# arima(diff(subset_MSFT$Open), order = c(0,0,1))
# arima(diff(subset_MSFT$Open), order = c(1,0,0))
# arima(diff(subset_MSFT$Open), order = c(3,0,0))

# acf(subset_MSFT$Open)
# axis(1, at = seq(0,30,))

# auto.arima(subset_MSFT$Open)
# eacf(subset_MSFT$Open)

arima(diff(subset_MSFT$Open), order = c(1,1,0))
arima(diff(subset_MSFT$Open), order = c(1,1,0))
arima(diff(subset_MSFT$Open), order = c(1,1,0), method = 'ML')
tsdiag(arima(diff(subset_MSFT$Open), order = c(1,0,0), method = 'ML'),gof = 10) #residuals


## QQ plot 
options(repr.plot.width = 10) 
qqnorm(rstandard(arima(diff(subset_MSFT$Open), order = c(1,1,0), method = 'ML'))) # r standard normalizes data and pulls out residuals
qqline(rstandard(arima(diff(subset_MSFT$Open), order = c(1,1,0), method = 'ML')))
shapiro.test(rstandard(arima(diff(subset_MSFT$Open), order = c(1,1,0)))) # low p-value resiuals not normally distributed 

## shapiro wilk and Ljung Box

hist(rstandard(arima(ts(subset_MSFT$Open), order = c(1,1,0))))
LB.test(arima(ts(subset_MSFT$Open), order = c(1,1,0)))



arima(subset_MSFT$Volume, order = c(1,1,3), method = 'ML') ## 8.20.23 GOOD ONE
tsdiag(arima(subset_MSFT$Volume, order = c(1,1,3), method = 'ML') ) ## 8.20.23 GOOD ONE
qqnorm(rstandard(arima(subset_MSFT$Volume, order = c(1,1,3), method = 'ML'))) # 8.20.23 GOOD ONE
qqline(rstandard(arima(subset_MSFT$Volume, order = c(1,1,3), method = 'ML'))) # 8.20.23 GOOD ONE

shapiro.test(rstandard(arima(subset_MSFT$Volume, order = c(1,1,3), method = 'ML'))) # 8.20.23 GOOD ONE
hist(rstandard(arima(subset_MSFT$Volume, order = c(1,1,3)))) # 8.20.23 GOOD ONE
LB.test(arima(subset_MSFT$Volume, order = c(1,1,3))) # 8.20.23 GOOD ONE

window(subset_MSFT$Volume, end = as.Date("2015-04-01"))
window(MSFT$Volume, start = as.Date("2020-01-01"))
MSFT$Volume[:-1]
history2 = window(MSFT$Volume, end = as.Date("2015-04-01"))

tail(ts(subset_MSFT$Volume))
ts(msft$Volume, frequency = 1, start =start_date)
ts(msft$Volume, frequency = 1, start =1260)



series = ts(msft$Volume, frequency = 1, start =c(2015,1))
series

series2 = ts(msft$Volume, frequency = 1, start =c(2020,1))
series2
window(MSFT$Volume,frequency = 1, start = 1260)
window(MSFT$Volume,frequency = 1, end = 1511)
MSFT$Volume


history2 = window(MSFT$Volume, start = 1260)
future2 = window(MSFT$Volume, end = 1511)

mod = Arima(history2, order = c(1,1,3), method = 'ML') # 8.20 good
fc2 = forecast(mod, h = 10, level = c(95)) # 8.20 good
plot(fc2, type = 'b')
points(x =(1261:1512),y = future2, pch = 4)

arima(MSFT$Volume, order = c(1,1,3), method = 'ML')
arima(subset_MSFT$Volume, order = c(1,1,3), method = 'ML')
window(subset_MSFT$Volume, start = 1198)



garch(arima(MSFT$Volume, order = c(1,1,3))$res^2)
garch(arima(MSFT$Volume, order = c(1,1,3))$res^2)




plot(ts(MSFT$Volume))

history2
window(MSFT$Volume, end = 1260)

length(window(MSFT$Volume,frequency = 1, end = 1511))
length(window(MSFT$Volume,frequency = 1, start = 1260))

future2

history2 = window(MSFT$Volume, start = as.Date("2019-12-31"))
subset_MSFT

end_date
start_date


LB.test(arima(ts(subset_MSFT$Open), order = c(1,1,0)))
runs(rstandard(arima(ts(subset_MSFT$Open), order = c(1,1,0))))
# residuals arenot normal distributed with low p value from shapiro wilk
# Ljung box test has low p-values residuals are not correlated



# options(repr.plot.width = 10)
# qqnorm(rstandard(arima(ts(subset_MSFT$Open), order = c(1,0,0)), method = 'ML'))
# qqline(rstandard(arima(ts(subset_MSFT$Open), order = c(1,0,0)), method = 'ML'))
# shapiro.test(rstandard(arima(ts(subset_MSFT$Open), order = c(1,1,0)), method = 'ML'))

hist(rstandard(arima(ts(subset_MSFT$Open), order = c(1,1,0))))
LB.test(arima(ts(subset_MSFT$Open), order = c(1,1,0)))
runs(rstandard(arima(ts(subset_MSFT$Open), order = c(1,1,0))))


##forecast debugging issue 
history = window(ts(MSFT$Open), end = end_date)
future = window(ts(MSFT$Open), start = start_date)
future
history
Arima(history, order= c(0,1,2))
forecast(Arima(history, order= c(0,1,2)),h = 10, level = c(95))
plot(forecast(Arima(history, order= c(0,1,2)),h = 10, level = c(95)))

# Another model to use GARCH to track periods of high volatility for large forecasting models

arima(ts(subset_MSFT$Open), order = c(0,0,1))$res

arima(ts(subset_MSFT$Open), order = c(0,0,1))$res^2
plot(arima(ts(subset_MSFT$Open), order = c(0,0,1))$res^2)
acf(arima(ts(subset_MSFT$Open), order = c(0,0,1))$res^2)
pacf(arima(ts(subset_MSFT$Open), order = c(0,0,1))$res^2)
eacf(arima(ts(subset_MSFT$Open), order = c(0,0,1))$res^2)
garch(arima(ts(subset_MSFT$Open), order = c(0,0,1))$res^2)






##forecast
history = window(ts(subset_MSFT$Open), end = end_date)
future = window(ts(subset_MSFT$Open), start = start_date)
future
history
Arima(history, order= c(0,1,2))
forecast(Arima(history, order= c(0,1,2)),h = 10, level = c(95))
plot(forecast(Arima(history, order= c(0,1,2)),h = 10, level = c(95)))


?forecast
##forecast
history = window(ts(MSFT$Volume), end = 2015)
future = window(ts(MSFT$Volume), start = start_date)
future
history
Arima(history, order= c(0,1,2))
forecast(Arima(history, order= c(0,1,2)),h = 10, level = c(95))



plot(forecast(Arima(history, order= c(0,1,2)),h = 10, level = c(95)))


##forecast
history = window(ts(subset_MSFT$Open), end = 2015)
future = window(ts(subset_MSFT$Open), start = start_date)
future
history
Arima(history, order= c(1,1,3))
forecast(Arima(future, order= c(1,1,3)),h = 10, level = c(95))



plot(forecast(Arima(future, order= c(1,1,3)),h = 10, level = c(95)))







# Choose GARCH to track periods of high volatility for large forecasting models

arima(ts(subset_MSFT$Open), order = c(0,0,1))$res

arima(ts(subset_MSFT$Open), order = c(0,0,1))$res^2
plot(arima(ts(subset_MSFT$Open), order = c(0,0,1))$res^2)
acf(arima(ts(subset_MSFT$Open), order = c(0,0,1))$res^2)
pacf(arima(ts(subset_MSFT$Open), order = c(0,0,1))$res^2)
eacf(arima(ts(subset_MSFT$Open), order = c(0,0,1))$res^2)


garch(arima(ts(subset_MSFT$Open), order = c(0,0,1))$res^2) # use arima GARCH model

window(ts(subset_MSFT), start  = 2019)
window(ts(subset_MSFT), end  = 2020)


window(ts(subset_MSFT), start  = 2017)






CADFtest(diff(subset_MSFT$Open), type = "none")


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







auto.arima(MSFT$Open)
auto.arima(differenced_open)




auto.arima(differenced_close)
auto.arima(differenced_open)

auto.arima(subset_MSFT$Open)

## anotherway to difference 
CADFtest(diff(subset_MSFT$Open), type = "none", max.lag.y = 8)
  
plot(diff(subset_MSFT$Open), type = "l",
       main = "difference rate over time",
       ylab = "difference rate", cex = 1.5, xaxp = c(1951,2019,68))

acf(diff(subset_MSFT$Open))
pacf(diff(subset_MSFT$Open))

eacf(diff(subset_MSFT$Open))


open_arima_model <- auto.arima(subset_MSFT$Open)
print(open_arima_model)

auto.arima(differenced_open)

auto.arima(subset_MSFT$Open, allowdrift = T)
auto.arima(subset_MSFT$Open, allowdrift = F)


arima(subset_MSFT$Open, order = c(4,1,2))


?auto.arima

arima(differenced_open, order = c(4,1,2))

?eacf











# Looking at Monthly Volume 
subset_MSFT_monthly <- subset_MSFT %>%
  group_by(YearMonth = floor_date(Date, unit = "month")) %>%
  summarise(MonthlyVolume = sum(Volume))

plot(x = subset_MSFT_monthly$YearMonth, y = subset_MSFT_monthly$MonthlyVolume, type = "l",
     xlab = "Date", ylab = "Volume", main = "Microsoft Stock Volume Over Months (Subset)")

# ACF & PACF
acf(MSFT$Close)


# ADF Test
adf.test(subset_MSFT$Volume)


# Daily return percentage
((subset_MSFT$Open - subset_MSFT$Close) / subset_MSFT$Open) * 100

plot(x = subset_MSFT$Date,ts_volume_monthly, type = "l")

subset_MSFT_monthly

acf(MSFT$Volume,50)
pacf(MSFT$Volume,50)
plot(MSFT$Volume, type = "l")

adf.test(MSFT$Volume) # p=value less than 0.05 so reject null hypothesis

adf.test(MSFT$Close)



acf(ts_volume_monthly)
pacf(ts_volume_monthly)

ts_volume_monthly <- ts(subset_MSFT$Volume, frequency = 12)

adf.test(diff(ts_volume))
plot(diff(ts_volume))

arima(diff(ts_volume))


acf(diff(ts_volume))
pacf(diff(ts_volume))


checkresiduals(arima(diff(ts_volume)))


acf(diff(ts_volume, lag.max= 20))


pacf(diff(ts_volume, lag.max= 20))




MSFT <- MSFT %>% column_to_rownames(., var = 'Date')
head(MSFT)
colnames(MSFT)

plot(MSFT$Volume, type = "l")
plot(x = MSFT$Date, y =MSFT$Volume, type = "l")
plot(x = MSFT$Date, y =MSFT$Close, type = "l")

plot(msft$Date, msft$Volume, type  = 'l')

my_data <- data.frame(dates = seq(as.Date("2015-01-04"),  # Create example data frame
                                  by = "day",
                                  length.out = 10),
                      values = 1:10)
data.frame(dates = seq(as.Date("2015-01-04"),  # Create example data frame
                       by = "day",
                       length.out = 10),
           values = 1:10)


MSFT$DateColumn <- as.Date(MSFT$DateColumn)





subset_MSFT <- MSFT %>%
  filter(DateColumn >= start_date, DateColumn <= end_date)

subset_MSFT_monthly <- subset_MSFT %>%
  group_by(YearMonth = floor_date(DateColumn, unit = "month")) %>%
  summarise(MonthlyVolume = sum(Volume))


plot(x = subset_MSFT_monthly$YearMonth, y = subset_MSFT_monthly$MonthlyVolume, type = "l",
     xlab = "Date", ylab = "Volume", main = "Microsoft Stock Volume Over Months (Subset)")

install.packages("forecast")
install.packages("TSA")
install.packages("quantmod")






pacf(MSFT$Volume,20)
 pacf(MSFT$Volume,100)

head(airpassengers)
plot(airpassengers$Passengers )

acf(airpassengers$Passengers,100)
pacf(airpassengers$Passengers,200)

library(dplyr)
library(lubridate)

colnames(MSFT)


MSFT$DateColumn <- as.Date(MSFT$DateColumn)

MSFT %>%
  group_by(YearMonth = floor_date(Date, unit = "month")) %>%
  summarise(MonthlyVolume = sum(Volume))


acf(MSFT$Volume)
pacf(MSFT$Volume)



head(airpassengers)

acf(MSFT$Volume,200)
##
head(airpassengers)
apass <- airpassengers
plot(x = apass$Month, y = apass$Passengers)
apass$Month


ts_volume <- ts(msft2$Volume, frequency =12)

arima(diff(ts_volume))
checkresiduals(arima(diff(ts_volume)))

colnames(msft2)

msft2[1]
adf.test(msft$Volume) # p=value less than 0.05 so reject null hypothesis

adf.test(msft$Close)

ts_volume <- ts(msft2$Volume, frequency =12)

arima(diff(ts_volume))
checkresiduals(arima(diff(ts_volume)))


plot(x = msft$Date, y =msft$Open, type = "l")
plot(x = msft$Date, y =msft$Volume, type = "l")
plot(x = msft$Date, y =msft$Close, type = "l")


acf(msft$Volume,200)
pacf(msft$Volume,200)

head(airpassengers)
apass <- airpassengers
plot(x = apass$Month, y = apass$Passengers)
apass$Month

