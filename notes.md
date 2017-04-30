# Time series analysis in R

## `Date` class
* `as.Date(dataframe$vector)` to coerce a factor with a strings e.g. `2017-04-27` into dates
* A sensible date-time format for recording info is `2017-04-19 12:30:20` = `%Y-%m-%d %H:%M:S`

## `ts` class
* The time series class
* `ts(data_frame)` to create a time series object

## Plotting time series with ggplot2
* ggplot() + geom_line() + scale_x_date(format

## scales packages
* Provides functionality for POSIXct and POSIXt classes

## Decomposition of time series
* Deconstructs a time series into several components each representing a single pattern present in the data
	* e.g. seasonal, smooth, cyclic, irregular
		* Cyclic patterns have no fixed period 
		* Seasonal patterns have a regular period and are associated with some calendar event
		* Cyclic > seasonal length of trend
	* Can be used to understand the time series better
	* Can be used to improve forecasting


* zoo `rollmean()` function

* Other packages
	* ggseas

## Forecasting
* What is the difference between ARIMA and Exponential smoothing?
