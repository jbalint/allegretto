# http://www.forbes.com/sites/katestalter/2012/04/27/how-a-top-trader-uses-moving-average-crossovers/

# Trade history should be loaded already (c.f. mysql_import.R)
symbol <- "AAPL"
mktdata <- get(symbol)
mktdata <- merge.xts(mktdata, ADX(mktdata))
mktdata$SMAFast <- SMA(Cl(mktdata), n = 10)
mktdata$SMASlow <- SMA(Cl(mktdata), n = 20)
mktdata <- merge.xts(mktdata, ATR(mktdata))
mktdata$AtrStoploss <- Cl(mktdata) < (lag(Cl(mktdata), k=1) - (2 * lag(mktdata$tr, k=1)))
mktdata <- merge.xts(mktdata, sigCrossover("MACrossover", columns=c("SMAFast", "SMASlow")))
mktdata <- merge.xts(mktdata, sigCrossover("MACrossback", columns=c("SMASlow", "SMAFast")))


#
currency("USD")
stock(symbol, currency="USD")
Sys.setenv(TZ="America/Chicago")
rm.strat("default")
initPortf(symbols=c(symbol))
strategy("default", store = TRUE)
initOrders(portfolio="default")
initAcct(portfolios="default", initEq=10000)

osRisk <- function(data, timestamp, ...) {
    mktdata <- data
    ts <- timestamp
    maxSpend <- 15000
    maxRisk <- 100 # 2% of $5k, TODO: example value (for now)
    maxLoss <- as.numeric(mktdata[ts]$atr) * 3
    orderqty <- floor(maxRisk / maxLoss)
    price <- as.numeric(Cl(mktdata[ts]))
    cost <- orderqty * price
    maxqty <- floor(maxSpend / price)
    return(ifelse(cost > maxSpend, maxqty, orderqty))
}

# TODO: need to code up some position sizing logic

add.rule("default", "ruleSignal",
         list(sigcol = "MACrossover", sigval = TRUE, orderqty = 1, osFUN = osRisk, ordertype = "market", orderside = "long"),
         type = "enter",
         label = "enterLong")

## add.rule("default", "ruleSignal",
##          list(sigcol = "AtrStoploss", sigval = 1, orderqty = "all", ordertype = "market", orderside = "long"), type = "exit")
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

par(bg="gray")
par(bg="#2F4F4F")

chart.Posn("default", symbol, TA="add_TA(mktdata$SMAFast,col=4,on=1);add_TA(mktdata$SMASlow,col=6,on=1)")
