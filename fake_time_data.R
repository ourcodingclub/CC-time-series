daily_milk <- read.csv("daily_milk.csv")

daily_milk

# 24.605177 l per day
# 23.605177 / 2  = 11.80259 l per milking session

# January AM
jan_am <- seq(from = 11.8, to = 9.5, length.out = 31) + rnorm(31, sd = 0.5)

# February AM
feb_am <- seq(from = 9.5, to = 14.5, length.out = 28) + rnorm(28, sd = 0.5)

# January PM
jan_pm <- seq(from = 10.5, to = 10, length.out = 31) + rnorm(31, sd = 0.5)

# February PM
feb_pm <- seq(from = 10, to = 11.5, length.out = 28) + rnorm(28, sd = 0.5)

am <- c(jan_am, feb_am)

pm <- c(jan_pm, feb_pm)

daily_milk$milk_prod_per_cow_kg <- c(rbind(am, pm))

daily_milk

daily_milk$date_time_posix <- as.POSIXct(daily_milk$date_time, format = "%Y-%m-%d %H:%M:%S")

daily_milk$am_pm <- format(daily_milk$date_time_posix, format = "%H")

ggplot(daily_milk, aes(x = date_time_posix, y = milk_prod_per_cow_kg)) + 
	geom_line() + 
	scale_x_datetime(date_labels = "%p-%d", date_breaks = "36 hour")

ggplot(daily_milk, aes(x = date_time_posix, y = milk_prod_per_cow_kg, group = am_pm)) + geom_line(aes(colour = am_pm)) + scale_x_datetime()

write.csv(daily_milk[,c("date_time", "milk_prod_per_cow_kg")], "daily_milk.csv", row.names = FALSE)
