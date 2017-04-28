# Time series tutorial script

# Set working directory ----
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))	

# Packages ----
library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)
library(forecast)	

# Load data ----
# Quarterly time series, `ts` class
austres <- datasets::austres	

# Monthly time series, `dataframe` class
milk <- read.csv("monthly_milk.csv", stringsAsFactors = FALSE)
colnames(milk)[colnames(milk) == 'Monthly.milk.production..pounds.per.cow..Jan.62...Dec.75'] <- 'milk_prod'
milk$Month <- paste(milk$Month,"-01", sep = "")
milk$Month_time <- paste(milk$Month, "12:20:30")	

# Exploring data types ----
# austres
austres
typeof(austres)
class(austres)
# Plot
plot.ts(austres)
plot(austres)	

# milk
milk$Month
class(milk)
typeof(milk$Month)
class(milk$Month)
head(milk)
# Plot
plot.ts(milk$milk_prod)
plot(milk$milk_prod)	

# Coercing a data frame to `ts` class ----
milk_ts <- ts(milk$milk_prod, start = 1962, end = 1975, freq = 12)	# Specify start and end year, freq (monthly = 12)
class(milk_ts)
plot.ts(milk_ts)
milk_ts	

# and back to a data frame
milk_df <- data.frame(milk_ts = c(milk_ts), time = c(time(milk_ts)))
milk_df	

# Changing a factor to a date / time ----
milk$Month_date <- as.Date(milk$Month)
milk$Month_date_test <- as.Date(milk$Month, format = "%Y-%m-%d")

class(milk$Month_date)
typeof(milk$Month_date)
milk$Month_date

# POSIXct time and date double
milk$Month_time_date <- as.POSIXct(milk$Month_time, format = "%Y-%m-%d %H:%M:%S")	
class(milk$Month_time_date)
typeof(milk$Month_time_date)
milk$Month_time_date

# Changing a date format ----
milk$Month_date_alt <- format(milk$Month_date, format = "%Y/%B/%d")
class(milk$Month_date_alt)
head(milk$Month_date_alt)
milk$Month_date_alt_fix <- as.Date(milk$Month_date_alt, format = "%Y/%B/%d")

# Plotting with ggplot2 ----
# With `Date` class
ggplot(milk, aes(x = milk$Month_date, y = milk_prod)) +
	geom_line() +
	scale_x_date(date_labels = "%Y", date_breaks = "1 year")	# Specify the label and breaks for x axis, takes `Date` class only	

ggplot(milk, aes(x = Month_date, y = milk_prod)) +
	geom_line() +
	scale_x_date(date_labels = "%m-%Y", date_breaks = "6 month")	# Specify the label and breaks for x axis, takes `Date` class only	

ggplot(milk, aes(x = Month_date, y = milk_prod)) +
	geom_line() +
	scale_x_date(date_labels = "%m-%Y", date_breaks = "6 month", limits = c(as.Date("1960-06-01"), NA))

ggplot(milk, aes(x = Month_date, y = milk_prod)) +
	geom_line() +
	scale_x_date(date_labels = "%m-%Y", date_breaks = "1 day")

ggplot(milk, aes(x = Month_date, y = milk_prod)) +
	geom_line()		

# With `POSIXct POSIXt` double class
ggplot(milk, aes(x = Month_time_date, y = milk_prod)) +
	geom_line() +
	scale_x_datetime(labels = date_format("%Y-%b"), date_breaks = "6 month")	# Specify the label and breaks for x axis, takes `Date` class only	

# Decomposition ----
milk_stl <- stl(milk_ts, s.window="period")
plot(milk_stl)
monthplot(milk_ts)
seasonplot(milk_ts)	

# Month, season, and trend plots with ggplot
# Trend plot
ggplot(milk, aes(x = Month_date, y = milk_prod)) +
	geom_smooth(method = "loess", se = FALSE, span = 0.6)

# Seasonal plot
# Extract month and year and store in new column
milk$year <- format(milk$Month_date, "%Y")
milk$month_num <- format(milk$Month_date, "%m")

ggplot(milk, aes(x = month_num, y = milk_prod, group = year)) + 
	geom_line(aes(colour = year))

# Forecasting ----
milk_fc_ets <- ets(milk_ts)
milk_fc_ets	
milk_fc_arima <- auto.arima(milk_ts)
milk_fc_arima
