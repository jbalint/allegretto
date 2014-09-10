data <- read.csv("scco_data1.dump.csv", head=TRUE, sep=",")
data$time = data$time / 1000
data$time2 <- as.POSIXlt(data$time)

data <- subset(data, time >= "2014-03-01")

nrow(data)
ncol(data)

# remove afterhours data
data <- subset(data, time2$hour >= 8 & time2$hour <= 15)

library(data.table)
dt <- data.table(data)
dt[,list(mean=mean(close),sd=sd(close)),by=time]
library(quantmod)

#data <- subset(data, select = -c(symbol))

data2 <- as.xts(data[c("close", "open", "volume", "high", "low")], order.by=data$time)
myohlc <- as.quantmod.OHLC(data2,col.names=c("Close", "Open", "Volume", "High", "Low"))

# Test buy data
buyx <- subset(data, time2$hour == 12 & (time2$mday %% 5) == 0)

buyx$ind <- buyx$high * 1.01
buy1 <- as.xts(buyx[c("ind")], order.by=buyx$time)

buyx$ind <- buyx$high * .99
buy2 <- as.xts(buyx[c("ind")], order.by=buyx$time)

#addTA(buy1, on=1, type="p", pch=24, col=139, bg=139)
#addTA(buy1, on=1, type="p", pch=20, col=139, bg=139)
#addTA(buy2, on=1, type="h", col=139)

#png(filename="x1.png", width=1680, height=300, units="px", pointsize=12)
chartSeries(myohlc, theme="black", TA=c(addVo(),
                                       addTA(buy1, on=1, type="p", pch=20, col=139, bg=139),
                                       addTA(buy2, on=1, type="h", col=139)),
            name="SCCO")
#dev.off()
