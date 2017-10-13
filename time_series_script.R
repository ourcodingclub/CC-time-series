# Time series analysis
# Coding Club (ourcodingclub@gmail.com)
# 2017_10_13

# Packages ----
library(ggplot2)
library(forecast)
library(dplyr)

# Set working directory ----
setwd("~/Downloads/CC-time-series-master")

# Load data ----
monthly_milk <- read.csv("monthly_milk.csv")  # Milk production per cow per month
daily_milk <- read.csv("daily_milk.csv")  # Milk production per cow per milking

head(monthly_milk)
class(monthly_milk)

# Coerce monthly_milk to `Date` class ----
class(monthly_milk$month)
monthly_milk$month_date <- as.Date(monthly_milk$month, format = "%Y-%m-%d")
class(monthly_milk$month_date) 

# Coerce daily milk to POSIXct class ----
head(daily_milk)
class(daily_milk$date_time)
daily_milk$date_time_posix <- as.POSIXct(daily_milk$date_time, format = "%Y-%m-%d %H:%M:%S")
class(daily_milk$date_time_posix)

# Create badly formatted dates ----
monthly_milk$bad_date <- format(monthly_milk$month_date, format = "%d/%b/%Y-%u")
head(monthly_milk$bad_date)
class(monthly_milk$bad_date)

# Coerce bad date to good date ----
monthly_milk$good_date <- as.Date(monthly_milk$bad_date, format = "%d/%b/%Y-%u")
head(monthly_milk$good_date)
class(monthly_milk$good_date)

# Plot time series data ----

# Using scale_x_date
ggplot(monthly_milk, aes(x = month_date, y = milk_prod_per_cow_kg)) + 
	geom_line() + 
	scale_x_date(date_labels = "%Y", date_breaks = "1 year")

# Using scale_x_datetime
ggplot(daily_milk, aes(x = date_time_posix, y = milk_prod_per_cow_kg)) + 
	geom_line() + 
	scale_x_datetime(date_labels = "%p-%d", date_breaks = "36 hour")

# Viewing trend using loess smooth
ggplot(monthly_milk, aes(x = month_date, y = milk_prod_per_cow_kg)) +
	geom_line() +
	geom_smooth(method = "loess", se = FALSE, span = 0.6)

# Explore seasonal trends ----

# Extract month and year and store in new column
monthly_milk$year <- format(monthly_milk$month_date, format = "%Y")
monthly_milk$month_num <- format(monthly_milk$month_date, format = "%m")

# Plot months
ggplot(monthly_milk, aes(x = month_num, y = milk_prod_per_cow_kg, group = year)) + 
	geom_line(aes(colour = year))

# Using ts objects to decompose trends ----

# Transform to `ts` class
monthly_milk_ts <- ts(monthly_milk$milk_prod, start = 1962, end = 1975, freq = 12)  # Specify start and end year, measurement frequency (monthly = 12)

# Decompose using `stl()`
monthly_milk_stl <- stl(monthly_milk_ts, s.window = "period")

# Generate plots
plot(monthly_milk_stl)  # top=original data, second=estimated seasonal, third=estimated smooth trend, bottom=estimated irregular element i.e. unaccounted for variation
monthplot(monthly_milk_ts, choice = "seasonal")  # variation in milk production for each month
seasonplot(monthly_milk_ts)	

# Forecasting ----

# Split data into testing and training
monthly_milk_model <- window(x = monthly_milk_ts, start=c(1962), end=c(1970))
monthly_milk_test <- window(x = monthly_milk_ts, start=c(1970))

# Creating model objects of each type of ets model
milk_ets_auto <- ets(monthly_milk_model)
milk_ets_mmm <- ets(monthly_milk_model, model = "MMM")
milk_ets_zzz<- ets(monthly_milk_model, model = "ZZZ")
milk_ets_mmm_damped <- ets(monthly_milk_model, model = "MMM", damped = TRUE)

# Creating forecast objects
milk_ets_fc <- forecast(milk_ets_auto, h = 60)  # `h = 60` means that the forecast will be 60 time periods long, in our case a time period is one month
milk_ets_mmm_fc <- forecast(milk_ets_mmm, h = 60)
milk_ets_zzz_fc <- forecast(milk_ets_zzz, h = 60)
milk_ets_mmm_damped_fc <- forecast(milk_ets_mmm_damped, h = 60)

# Convert forecasts to data frames 
milk_ets_fc_df <- cbind("Month" = rownames(as.data.frame(milk_ets_fc)), as.data.frame(milk_ets_fc))  # Creating a data frame
names(milk_ets_fc_df) <- gsub(" ", "_", names(milk_ets_fc_df))  # Removing whitespace from column names
milk_ets_fc_df$Date <- as.Date(paste("01-", milk_ets_fc_df$Month, sep = ""), format = "%d-%b %Y")  # prepending day of month to date
milk_ets_fc_df$Model <- rep("ets")  # Adding column of model type

milk_ets_mmm_fc_df <- cbind("Month" = rownames(as.data.frame(milk_ets_mmm_fc)), as.data.frame(milk_ets_mmm_fc))
names(milk_ets_mmm_fc_df) <- gsub(" ", "_", names(milk_ets_mmm_fc_df))
milk_ets_mmm_fc_df$Date <- as.Date(paste("01-", milk_ets_mmm_fc_df$Month, sep = ""), format = "%d-%b %Y")
milk_ets_mmm_fc_df$Model <- rep("ets_mmm")

milk_ets_zzz_fc_df <- cbind("Month" = rownames(as.data.frame(milk_ets_zzz_fc)), as.data.frame(milk_ets_zzz_fc))
names(milk_ets_zzz_fc_df) <- gsub(" ", "_", names(milk_ets_zzz_fc_df))
milk_ets_zzz_fc_df$Date <- as.Date(paste("01-", milk_ets_zzz_fc_df$Month, sep = ""), format = "%d-%b %Y")
milk_ets_zzz_fc_df$Model <- rep("ets_zzz")

milk_ets_mmm_damped_fc_df <- cbind("Month" = rownames(as.data.frame(milk_ets_mmm_damped_fc)), as.data.frame(milk_ets_mmm_damped_fc))
names(milk_ets_mmm_damped_fc_df) <- gsub(" ", "_", names(milk_ets_mmm_damped_fc_df))
milk_ets_mmm_damped_fc_df$Date <- as.Date(paste("01-", milk_ets_mmm_damped_fc_df$Month, sep = ""), format = "%d-%b %Y")
milk_ets_mmm_damped_fc_df$Model <- rep("ets_mmm_damped")

# Combining into one data frame
forecast_all <- rbind(milk_ets_fc_df, milk_ets_mmm_fc_df, milk_ets_zzz_fc_df, milk_ets_mmm_damped_fc_df)

# Plotting with ggplot
ggplot() +
	geom_line(data = monthly_milk, aes(x = month_date, y = milk_prod_per_cow_kg)) +  # Plotting original data
	geom_line(data = forecast_all, aes(x = Date, y = Point_Forecast, colour = Model))  # Plotting model forecasts

# Comparing accuracy of forecasts
accuracy(milk_ets_fc, monthly_milk_test)
accuracy(milk_ets_mmm_fc, monthly_milk_test)
accuracy(milk_ets_zzz_fc, monthly_milk_test)
accuracy(milk_ets_mmm_damped_fc, monthly_milk_test)

# Extracting forecast estimates ----
milk_ets_fc_df %>%
	filter(Month == "Jan 1975") %>%
	select(Month, Point_Forecast)

milk_ets_zzz_fc_df %>%
	filter(Month == "Jan 1975") %>%
	select(Month, Point_Forecast)




