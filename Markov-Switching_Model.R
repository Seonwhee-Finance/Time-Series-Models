library(mongolite)

con <- mongolite::mongo(collection = "BTC_30MIN", db = "BINANCE", url = " ", verbose = TRUE, options = ssl_options())
df <- con$find(query = '{"$and": [{"time_period_end": {"$lte":"2018-06-01"}}, {"time_period_end": {"$gt":"2018-01-01 00:00:00"}}]}')

orderDF <- arrange(df, time_period_start)
subDF <- subset(orderDF, select = c("time_period_end", "price_close"))

library(zoo)
price_close <- read.zoo(subDF, format = "%Y-%m-%d %H:%M:%S", header=TRUE, FUN=as.POSIXct)
plot(diff(log(price_close)), main="log-diff of BINANCE BTC/USDT")
dev.off()
library(MSwM)

#Model with only intercept
mod<-lm(diff(log(price_close)) ~ 1)

#Fit regime-switching model
msm_intercept <- msmFit(mod, k=2, sw=c(T,T), p=0)


#Fit regime-switching model with AR(1) model
msm_ar1 <- msmFit(mod, k=2, sw=c(T,T,T), p=1)
par(mar=c(3,3,3,3))
plotProb(msm_intercept, which=1)
dev.off()
plotProb(msm_intercept, which=2)

dev.off()

rm(df)
rm(con)
rm(mod)
rm(orderDF)
rm(subDF)
rm(msm_intercept)
rm(price_close)
