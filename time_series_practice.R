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
# colnames(milk)[colnames(milk) == 'Month_prod_per_cow_kg'] <- 'milk_prod_per_cow_per_kg'
# colnames(milk)[colnames(milk) == 'Month'] <- 'month'
colnames(milk)[colnames(milk) == 'milk_prod_per_cow_kg'] <- 'milk_prod'

# milk$month <- paste(milk$month,"-01", sep = "")
# write.csv(milk, "monthly_milk.csv", row.names = FALSE)
milk$month_time <- paste(milk$month, "12:20:30")	

# Daily time series
daily_milk <- read.csv("daily_milk.csv", stringsAsFactors = FALSE)
daily_milk
colnames(daily_milk)[colnames(daily_milk) == 'milk_prod_per_cow_kg'] <- 'milk_prod'
class(daily_milk$date_time)

daily_milk$date_time_posix <- as.POSIXct(daily_milk$date_time, format = "%Y-%m-%d %H:%M:%S")	
class(daily_milk$date_time_posix)
daily_milk

# Exploring data types ----
# austres
austres
typeof(austres)
class(austres)
# Plot
plot.ts(austres)
plot(austres)	

# milk
milk$month
class(milk)
typeof(milk$month)
class(milk$month)
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
milk$month_date <- as.Date(milk$month)
milk$month_date_test <- as.Date(milk$month, format = "%Y-%m-%d")

class(milk$month_date)
typeof(milk$month_date)
milk$month_date



ggplot(daily_milk, aes(x = date_time_posix, y = milk_prod)) + 
	geom_line() + 
	scale_x_datetime(date_labels = "%p-%d", date_breaks = "36 hour")

# POSIXct time and date double
milk$month_time_date <- as.POSIXct(milk$month_time, format = "%Y-%m-%d %H:%M:%S")	
class(milk$month_time_date)
typeof(milk$month_time_date)
milk$month_time_date

# Changing a date format ----
milk$month_date_alt <- format(milk$month_date, format = "%Y/%B/%d")
class(milk$month_date_alt)
head(milk$month_date_alt)
milk$month_date_alt_fix <- as.Date(milk$month_date_alt, format = "%Y/%B/%d")

# Plotting with ggplot2 ----
# With `Date` class
ggplot(milk, aes(x = milk$month_date, y = milk_prod)) +
	geom_line() +
	scale_x_date(date_labels = "%Y", date_breaks = "1 year")	# Specify the label and breaks for x axis, takes `Date` class only	

ggplot(milk, aes(x = month_date, y = milk_prod)) +
	geom_line() +
	scale_x_date(date_labels = "%m-%Y", date_breaks = "6 month")	# Specify the label and breaks for x axis, takes `Date` class only	

ggplot(milk, aes(x = month_date, y = milk_prod)) +
	geom_line() +
	scale_x_date(date_labels = "%m-%Y", date_breaks = "6 month", limits = c(as.Date("1960-06-01"), NA))

ggplot(milk, aes(x = month_date, y = milk_prod)) +
	geom_line()		

# With `POSIXct POSIXt` double class
ggplot(milk, aes(x = month_time_date, y = milk_prod)) +
	geom_line() +
	scale_x_datetime(labels = date_format("%Y-%b"), date_breaks = "6 month")	# Specify the label and breaks for x axis, takes `Date` class only	
class(milk$month_time_date)
# Decomposition ----
milk_stl <- stl(milk_ts, s.window = "period")
class(milk_stl)
plot(milk_stl)
monthplot(milk_ts)
seasonplot(milk_ts)	

# month, season, and trend plots with ggplot
# Trend plot
ggplot(milk, aes(x = month_date, y = milk_prod)) +
	geom_smooth(method = "loess", se = FALSE, span = 0.6)

# Seasonal plot
# Extract month and year and store in new column
milk$year <- format(milk$month_date, "%Y")
milk$month_num <- format(milk$month_date, "%m")
milk$year
unique(milk$year)
ggplot(milk, aes(x = month_num, y = milk_prod, group = year)) + 
	geom_line(aes(colour = year))

# Forecasting ----
milk_fc_ets <- ets(milk_ts)
milk_fc_ets	
milk_fc_arima <- auto.arima(milk_ts)
milk_fc_arima
