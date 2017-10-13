# Generating fake time series data of daily cow milk production
# Coding Club (ourcodingclub@gmail.com)
# 2017_10_13

# Packages ----
library(ggplot2)

# Parameters ----
	# 24.605177 l per day
	# 2 milking sessions
	# 23.605177 / 2  = 11.80259 l per milking session

# Creating milk data ----
# January AM
jan_am <- seq(from = 11.8, to = 9.5, length.out = 31) + rnorm(31, sd = 0.5)

# January PM
jan_pm <- seq(from = 10.5, to = 10, length.out = 31) + rnorm(31, sd = 0.5)

# February AM
feb_am <- seq(from = 9.5, to = 14.5, length.out = 28) + rnorm(28, sd = 0.5)

# February PM
feb_pm <- seq(from = 10, to = 11.5, length.out = 28) + rnorm(28, sd = 0.5)

# Creating date and time data ----
# Time
time <- c(rep(c("05:00:00", "17:00:00"), 62), 
					rep(c("05:00:00", "17:00:00"), 56))

# Dates
date <- c(rep(seq(from = as.Date("1975-01-01"), length.out = 31, by = "days"), each = 2),
							rep(seq(from = as.Date("1975-02-01"), length.out = 28, by = "days"), each = 2))

# Concatenate
date_time <- paste(date, time)
milk_prod_per_cow_kg <- as.vector(rbind(c(jan_am, feb_am), 
																				c(jan_pm, feb_pm)))
daily_milk <- data.frame(date_time, milk_prod_per_cow_kg)

# Create plots ----

# Convert date time to POSIX
daily_milk$date_time_posix <- as.POSIXct(daily_milk$date_time, format = "%Y-%m-%d %H:%M:%S")

# Add am/pm column
daily_milk$am_pm <- format(daily_milk$date_time_posix, format = "%H")

# Combined time series
ggplot(daily_milk, aes(x = date_time_posix, y = milk_prod_per_cow_kg)) + 
	geom_line() + 
	scale_x_datetime(date_labels = "%p-%d", date_breaks = "36 hour")

# Time series decomposed by am pm
ggplot(daily_milk, aes(x = date_time_posix, y = milk_prod_per_cow_kg, group = am_pm)) + geom_line(aes(colour = am_pm)) + scale_x_datetime()

# Write dataset to csv
write.csv(daily_milk[,c("date_time", "milk_prod_per_cow_kg")], "daily_milk.csv", row.names = FALSE)
