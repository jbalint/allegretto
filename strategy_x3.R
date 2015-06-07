# http://www.forbes.com/sites/katestalter/2012/04/27/how-a-top-trader-uses-moving-average-crossovers/

# Trade history should be loaded already (c.f. mysql_import.R)
symbol <- "AAPL"
mktdata <- get(symbol)
#mktdata <- mktdata["2014-09-01/2014-09-15"]

mktdata <- merge.xts(mktdata, ADX(mktdata))
mktdata$SMAFast <- SMA(mktdata$Close, n = 10)
mktdata$SMASlow <- SMA(mktdata$Close, n = 20)
mktdata <- merge.xts(mktdata, ATR(mktdata))
mktdata$AtrStop <- lag(mktdata$Close, k=1) - (2 * lag(mktdata$atr, k=1))
mktdata$AtrStopout <- mktdata$Close < mktdata$AtrStop
mktdata <- merge.xts(mktdata, sigCrossover("MACrossover", columns=c("SMAFast", "SMASlow")))
mktdata <- merge.xts(mktdata, sigCrossover("MACrossback", columns=c("SMASlow", "SMAFast")))
mktdata$mac2 <- mktdata$MACrossover * mktdata$Close
#head(mktdata[,c("AAPL.Close", "atr", "AtrStop", "AtrStopout")], n=100)
# these packages are really stupid in this regard, Cl() uses grep, etc
# AND there's no nicer way to do this?
mktdata <- mktdata[, !(colnames(mktdata) %in% c("trueHigh","trueLow"))]
#mktdata <- subset(mktdata, select = -c("trueHigh", "trueLow"))

#
currency("USD")
stock(symbol, currency="USD")
Sys.setenv(TZ="America/Chicago")
rm.strat("default")
initPortf(symbols=c(symbol))
strategy("default", store = TRUE)
initOrders(portfolio="default")
initAcct(portfolios="default", initEq=10000)

# position sizing, passed as osFUN (order sizing function)
osRisk <- function(data, timestamp, ...) {
    mktdata <- data
    ts <- timestamp
    maxSpend <- 5000
    maxRisk <- 100 # 2% of $5k, TODO: example value (for now)
    maxLoss <- as.numeric(mktdata[ts]$atr) * 3
    orderqty <- floor(maxRisk / maxLoss)
    price <- as.numeric(mktdata[ts]$Close)
    cost <- orderqty * price
    maxqty <- floor(maxSpend / price)
    return(ifelse(cost > maxSpend, maxqty, orderqty))
}

add.rule("default", "ruleSignal",
         list(sigcol = "MACrossover", sigval = TRUE, orderqty = 1, osFUN = osRisk, ordertype = "market", orderside = "long"),
         type = "enter",
         label = "enterLong")

## add.rule("default", "ruleSignal",
##          list(sigcol = "AtrStopout", sigval = 1, orderqty = "all", ordertype = "market", orderside = "long"), type = "exit")

add.rule("default", "ruleSignal",
         list(orderqty = "all", ordertype = "stoplimit", threshold = -0.01, tmult = TRUE),
         type = "chain", parent = "enterLong")

add.rule("default", "ruleSignal",
         list(sigcol = "MACrossback", sigval = TRUE, orderqty = "all", ordertype = "market", orderside = "long"), type = "exit")


## add.rule("default", name = 'ruleSignal',
## 	arguments=list(sigcol='long' , sigval=TRUE,
## 		replace=FALSE,
## 		orderside='long',
## 		ordertype='stoplimit', tmult=TRUE, threshold=(.4/100),
## 		#TxnFees=.txnfees,
## 		orderqty='all',
## 		orderset='ocolong'
## 	),
## 	type='chain', parent='enterLONG',
## 	label='stopLossLONG',
## 	enabled=FALSE
## )


applyStrategy(strategy="default", portfolios="default", mktdata=mktdata)

###
updatePortf("default")
updateAcct()
updateEndEq(Account="default")

t(tradeStats("default"))

tradeQuantiles("default", symbol)

perTradeStats("default", symbol)

#par(bg="gray")
#par(bg="#2F4F4F")


TA <- "add_TA(mktdata$AtrStop,on=1)"

TA <- ""

TA <- "add_TA(mktdata$SMAFast,col=4,on=1);add_TA(mktdata$SMASlow,col=6,on=1)"

#chart.Posn("default", symbol, TA=TA)

#myChartPosn("default", symbol, mktdata["/2014-11"], TA=TA)

png(filename = "test_out.png",
    width = 6000, height = 1000, units = "px", pointsize = 12,
    bg = "white",  res = 150,
    type = c("cairo", "cairo-png", "Xlib", "quartz"))

myChartPosn("default", symbol, mktdata, TA=TA)

dev.off()


# this one causes problems with `chartSeries' (because it has the word "low" in it)
mktdata <- mktdata[, !(colnames(mktdata) %in% c("SMASlow"))]

chart_Series(mktdata["2014-09"])

add_TA(mktdata$AtrStop, on = 1)
#add_TA(mktdata$mac2, on = 1, col = 8)

add_TA(mktdata$SMAFast,col=4,on=1)
