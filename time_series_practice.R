# Time series tutorial script

# Set working directory ----
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))	

# Packages ----
library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)
library(forecast)	
library(colortools)

# Functions ----
# multiplot
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
	require(grid)
	
	# Make a list from the ... arguments and plotlist
	plots <- c(list(...), plotlist)
	
	numPlots = length(plots)
	
	# If layout is NULL, then use 'cols' to determine layout
	if (is.null(layout)) {
		# Make the panel
		# ncol: Number of columns of plots
		# nrow: Number of rows needed, calculated from # of cols
		layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
										 ncol = cols, nrow = ceiling(numPlots/cols))
	}
	
	if (numPlots==1) {
		print(plots[[1]])
		
	} else {
		# Set up the page
		grid.newpage()
		pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
		
		# Make each plot, in the correct location
		for (i in 1:numPlots) {
			# Get the i,j matrix positions of the regions that contain this subplot
			matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
			
			print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
																			layout.pos.col = matchidx$col))
		}
	}
}

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

# Creating badly formatted date that we can fix
bad_date <- format(milk$month_date, format = "%d/%b/%Y-%u")
head(bad_date)
class(bad_date)

good_date <- as.Date(milk$bad_date, format = "%d/%b/%Y-%u")
head(good_date)

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
milk_decom <- decompose(milk_ts)
plot(milk_decom)
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

# Create subset of data to create forecast
milk_model <- window(milk_ts, start=c(1962), end=c(1970))
milk_test <- window(milk_ts, start=c(1970))

# Arima
milk_arima <- auto.arima(milk_model)
milk_arima
typeof(milk_arima)
class(milk_arima)
summary(milk_arima)
plot(milk_arima)

milk_arima_fc <- forecast(milk_arima, h = 60)
milk_arima_fc
class(milk_arima_fc)
plot(milk_arima_fc)

# Can also directly forecast() on time series object to auto choose a model and plot
plot(forecast(milk_model))  # Chose A,A,A

# Exponential smoothing
milk_ets_auto <- ets(milk_model)
milk_ets_auto
typeof(milk_ets_auto)
class(milk_ets_auto)
summary(milk_ets_auto)
plot(milk_ets_auto)

milk_ets_fc <- forecast(milk_ets_auto, h = 60)
milk_ets_fc
class(milk_ets_fc)
plot(milk_ets_fc)
plot(forecast(milk_ets_auto,level=c(50,80,95), fan = TRUE))

milk_ets_mmm <- ets(milk_model, model = "MMM")
milk_ets_mmm_fc <- forecast(milk_ets_mmm, h = 60)
plot(milk_ets_mmm_fc)

milk_ets_zzz <- ets(milk_model, model = "ZZZ")
milk_ets_zzz_fc <- forecast(milk_ets_zzz, h = 60)
plot(milk_ets_zzz_fc)

milk_ets_mmm_damped <- ets(milk_model, model = "MMM", damped = TRUE)
milk_ets_mmm_damped_fc <- forecast(milk_ets_mmm_damped, h = 60)
plot(milk_ets_mmm_damped_fc)
accuracy(milk_ets_mmm_damped_fc)

plot(simulate(milk_ets_mmm))
plot(fitted(milk_ets_mmm))
plot(coef(milk_ets_mmm))
plot(milk_ets_auto)

# Comparing forecasts numerically ----
# AIC value
AIC(milk_ets_auto)
AIC(milk_ets_mmm)
AIC(milk_ets_zzz)
AIC(milk_ets_mmm_damped)

# BIC value
BIC(milk_ets_auto)
BIC(milk_ets_mmm)
BIC(milk_ets_zzz)
BIC(milk_ets_mmm_damped)

# Accuracy against test data (milk_test)
accuracy(milk_ets_fc, milk_test)
accuracy(milk_ets_mmm_fc, milk_test)
accuracy(milk_ets_zzz_fc, milk_test)
accuracy(milk_ets_mmm_damped_fc, milk_test)

# Comparing forecasts with ggplot ----

milk_arima_fc_df <- cbind("Month" = rownames(as.data.frame(milk_arima_fc)), as.data.frame(milk_arima_fc))
names(milk_arima_fc_df) <- gsub(" ", "_", names(milk_arima_fc_df))
milk_arima_fc_df$Date <- as.Date(paste("01-", milk_arima_fc_df$Month, sep = ""), format = "%d-%b %Y")
milk_arima_fc_df$Model <- rep("arima")

milk_ets_fc_df <- cbind("Month" = rownames(as.data.frame(milk_ets_fc)), as.data.frame(milk_ets_fc))
names(milk_ets_fc_df) <- gsub(" ", "_", names(milk_ets_fc_df))
milk_ets_fc_df$Date <- as.Date(paste("01-", milk_ets_fc_df$Month, sep = ""), format = "%d-%b %Y")
milk_ets_fc_df$Model <- rep("ets")

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

forecast_all <- rbind(milk_arima_fc_df, milk_ets_fc_df, milk_ets_mmm_fc_df, milk_ets_zzz_fc_df, milk_ets_mmm_damped_fc_df)

# Define colour palette
palette <- wheel(color = "#D99741", num = 5)

# forecasts with original data
ggplot() +
	geom_line(data = milk, aes(x = month_date, y = milk_prod)) +
	geom_line(data = milk_arima_fc_df, aes(x = Date, y = Point_Forecast), colour = palette[1]) +
	geom_line(data = milk_ets_fc_df, aes(x = Date, y = Point_Forecast), colour = palette[2]) +
	geom_line(data = milk_ets_mmm_fc_df, aes(x = Date, y = Point_Forecast), colour = palette[3]) +
	geom_line(data = milk_ets_zzz_fc_df, aes(x = Date, y = Point_Forecast), colour = palette[4]) +
	geom_line(data = milk_ets_mmm_damped_fc_df, aes(x = Date, y = Point_Forecast), colour = palette[5]) +
	scale_x_date()

ggplot() + 
	geom_line(data = milk, aes(x = month_date, y = milk_prod)) + 
	geom_line(data = forecast_all, aes(x = Date, y = Point_Forecast, colour = Model))

# Forecasts only
ggplot() +
	geom_line(data = milk_arima_fc_df, aes(x = Date, y = Point_Forecast), colour = palette[1]) +
	geom_line(data = milk_ets_fc_df, aes(x = Date, y = Point_Forecast), colour = palette[2]) +
	geom_line(data = milk_ets_mmm_fc_df, aes(x = Date, y = Point_Forecast), colour = palette[3]) +
	geom_line(data = milk_ets_zzz_fc_df, aes(x = Date, y = Point_Forecast), colour = palette[4]) +
	geom_line(data = milk_ets_mmm_damped_fc_df, aes(x = Date, y = Point_Forecast), colour = palette[5]) +
	scale_x_date() + 
	theme_classic()

# Forecasts only, with 95% CI
ggplot() +
	geom_line(data = milk_arima_fc_df, aes(x = Date, y = Point_Forecast), colour = palette[1]) +
	geom_ribbon(data = milk_arima_fc_df, aes(x = Date, ymin = Lo_95, ymax = Hi_95), fill = palette[1], alpha = 0.5) +
	geom_line(data = milk_ets_fc_df, aes(x = Date, y = Point_Forecast), colour = palette[2]) +
	geom_ribbon(data = milk_ets_fc_df, aes(x = Date, ymin = Lo_95, ymax = Hi_95), fill = palette[2], alpha = 0.5) +
	geom_line(data = milk_ets_mmm_fc_df, aes(x = Date, y = Point_Forecast), colour = palette[3]) +
	geom_ribbon(data = milk_ets_mmm_fc_df, aes(x = Date, ymin = Lo_95, ymax = Hi_95), fill = palette[3], alpha = 0.5) +
	geom_line(data = milk_ets_zzz_fc_df, aes(x = Date, y = Point_Forecast), colour = palette[4]) +
	geom_ribbon(data = milk_ets_zzz_fc_df, aes(x = Date, ymin = Lo_95, ymax = Hi_95), fill = palette[4], alpha = 0.5) +
	geom_line(data = milk_ets_mmm_damped_fc_df, aes(x = Date, y = Point_Forecast), colour = palette[5]) +
	geom_ribbon(data = milk_ets_mmm_damped_fc_df, aes(x = Date, ymin = Lo_95, ymax = Hi_95), fill = palette[5], alpha = 0.5) +
	scale_x_date() + 
	theme_classic()

# Forecasts only, 95% CI, Grid

p1 <- ggplot() +
	geom_line(data = milk_arima_fc_df, aes(x = Date, y = Point_Forecast), colour = palette[1]) +
	geom_ribbon(data = milk_arima_fc_df, aes(x = Date, ymin = Lo_95, ymax = Hi_95), fill = palette[1], alpha = 0.5) +
	scale_x_date() + 
	scale_y_continuous(limits = c(300, 500)) +
	theme_classic()

p2 <- ggplot() +
	geom_line(data = milk_ets_fc_df, aes(x = Date, y = Point_Forecast), colour = palette[2]) +
	geom_ribbon(data = milk_ets_fc_df, aes(x = Date, ymin = Lo_95, ymax = Hi_95), fill = palette[2], alpha = 0.5) +
	scale_x_date() + 
	scale_y_continuous(limits = c(300, 500)) +
	theme_classic()
	
p3 <- ggplot() +
	geom_line(data = milk_ets_mmm_fc_df, aes(x = Date, y = Point_Forecast), colour = palette[3]) +
	geom_ribbon(data = milk_ets_mmm_fc_df, aes(x = Date, ymin = Lo_95, ymax = Hi_95), fill = palette[3], alpha = 0.5) +
	scale_x_date() + 	
	scale_y_continuous(limits = c(300, 500)) +
	theme_classic()
	
p4 <- ggplot() +
	geom_line(data = milk_ets_zzz_fc_df, aes(x = Date, y = Point_Forecast), colour = palette[4]) +
	geom_ribbon(data = milk_ets_zzz_fc_df, aes(x = Date, ymin = Lo_95, ymax = Hi_95), fill = palette[4], alpha = 0.5) +
	scale_x_date() + 
	scale_y_continuous(limits = c(300, 500)) +
	theme_classic()
	
p5 <- ggplot() +
	geom_line(data = milk_ets_mmm_damped_fc_df, aes(x = Date, y = Point_Forecast), colour = palette[5]) +
	geom_ribbon(data = milk_ets_mmm_damped_fc_df, aes(x = Date, ymin = Lo_95, ymax = Hi_95), fill = palette[5], alpha = 0.5) +
	scale_x_date() + 
	scale_y_continuous(limits = c(300, 500)) +
	theme_classic()

multiplot(p1, p2, p3, p4, p5)

# Extracting milk production from models

milk_arima_fc_df %>%
	filter(Month=="Jan 1975") %>%
	select(Point_Forecast) %>%
	as.numeric(.)
