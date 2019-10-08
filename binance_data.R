library(mongolite)
library(plyr)
library(forecast)
## MongoDB connection
con <- mongolite::mongo(collection = "BTC_30MIN", db = "BINANCE", url = " ", verbose = TRUE, options = ssl_options())
df <- con$find(query = '{"$and": [{"time_period_end": {"$lte":"2019-01-01"}}, {"time_period_end": {"$gt":"2018-01-01 00:00:00"}}]}')

orderDF <- arrange(df, time_period_start)
y <- ts(orderDF['price_close'], start=c(2018, 1), frequency = 12*24*60/30)
decompose_y = decompose(y, "multiplicative")


plot(as.ts(decompose_y$seasonal))
plot(as.ts(decompose_y$trend))
plot(as.ts(decompose_y$random))
plot(decompose_y)

tail(orderDF)

autoplot(y) + ggtitle("BINANCE 2018") + ylab("price close every 5mins") + xlab("time")
ggAcf(y)

y %>% decompose(type="multiplicative") %>%
  autoplot() + xlab("time") +
  ggtitle("multiplicative decomposition")

dev.off()
rm(df)
rm(y)
rm(orderDF)
rm(con)
rm(decompose_y)
