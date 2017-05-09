data <- rnorm(3650, m=10, sd=2)

data

series <- ts(data, frequency=365, start=c(1919, 1))

attributes(series)
class(series)
length(series)

plot(series)

model_data = window(series, start=c(1919,1), end=c(1926,365))

plot(model_data)

test_data = window(series, start=c(1927,1), end=c(1928, 365))

library(fpp)

arimaMod <- auto.arima(model_data, stepwise=FALSE, approximation=FALSE)
arimaMod.Fr <-forecast(arimaMod,h=730)  # h = length of forecast, in this case 2 years = 365*2 = 730 

plot(arimaMod.Fr)
lines(test_data, col="red")
legend("topleft",lty=1,bty = "n",col=c("red","blue"),c("test_data","ARIMAPred"))

plot(test_data, main="Daily data", ylab="", xlab="Months", col = "red")

lines(arimaMod.Fr$mean, col="blue", lwd=3)

accuracy(arimaMod.Fr,test_data)

tsdisplay(residuals(arimaMod))

fit.ets <- ets(model_data)

fr.ets <- forecast(fit.ets,h=730)

plot(forecast(fit.ets,h=730))

plot(test_data)

lines(fr.ets$mean, col="red", lwd=5)
lines(arimaMod.Fr$mean, col="green")
legend("topleft",lty=1,cex=1,y.intersp=0.6,bty = "n",col=c("black","red","green"),c("data","ETS","ARIMA"))

accuracy(fr.ets,test_data)
accuracy(arimaMod.Fr,test_data)
