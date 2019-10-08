library(mongolite)
library(forecast)
library(tseries)
## MongoDB connection
con <- mongolite::mongo(collection = "BTC_5MIN", db = "BINANCE", url = " ", verbose = TRUE, options = ssl_options())
df <- con$find(query = '{"$and": [{"time_period_end": {"$lte":"2019-01-01"}}, {"time_period_end": {"$gt":"2018-01-01 00:00:00"}}]}')

orderDF <- arrange(df, time_period_start)
y <- ts(orderDF['price_close'], start=c(2018, 1), end = c(2019,1), frequency = 12*24*60/5)
plot.ts(y)
par(oma=c(0,0,5,0))
par(mfrow = c(1,2))
acf(y , main="ACF", ylab="")
pacf(y, main="PACF", ylab="")
mtext("ACF, PACF of BINANCE BTC/USDT",outer=TRUE,cex=2)

dev.off()
adf.test(y, k=0)

plot.ts(diff(log(y)), main="log-diff of BINANCE BTC/USDT")

par(oma=c(0,0,5,0))
par(mfrow = c(1,2))
acf((diff(log(y))) , main="ACF", ylab="")
pacf(diff(log(y)), main="PACF", ylab="")
mtext("ACF, PACF of log-diff BINANCE BTC/USDT",outer=TRUE,cex=2)
adf.test(diff(log(y)), k=0)
