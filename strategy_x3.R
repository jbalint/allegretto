# http://www.forbes.com/sites/katestalter/2012/04/27/how-a-top-trader-uses-moving-average-crossovers/

# Stoploss = price - (mult * ATR)
stoplossAtrMultiplier <- 3

TxnFees <- -10

# Trade history should be loaded already (c.f. mysql_import.R)
symbol <- "MTUM"
mktdata <- get(symbol)


mktdata <- merge.xts(mktdata, ADX(mktdata))

mktdata$SMAFast <- SMA(mktdata$Close, n = 10)
mktdata$SMASlow <- SMA(mktdata$Close, n = 20)
mktdata <- merge.xts(mktdata, ATR(mktdata))
mktdata$AtrStop <- lag(mktdata$Close, k=1) - (2 * lag(mktdata$atr, k=1))
mktdata$AtrStopout <- mktdata$Close < mktdata$AtrStop
mktdata <- merge.xts(mktdata, sigCrossover("MACrossover", columns=c("SMAFast", "SMASlow")))
mktdata <- merge.xts(mktdata, sigCrossover("MACrossback", columns=c("SMASlow", "SMAFast")))

mktdata$rsi5 <- RSI(mktdata$Close, n=5, maType="WMA", wts=mktdata[,"Volume"])

mktdata$maxOhlc <- pmax(mktdata$Open, mktdata$High, mktdata$Low, mktdata$Close)
mktdata$CloseRise <- mktdata$Close / lag(mktdata$maxOhlc)

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

# manual order entry & tracking
longstop <- 0
longpos <- 0
shortstop <- 0
shortpos <- 0
for (i in 1:nrow(mktdata)) {
    if ((i %% 400) == 0) {
        print(i)
    }
    if (is.na(mktdata[i,]$atr)) {
        next
    }
    # Trailing stop calculation
    if (longpos > 0) {
        newstop <- data.frame(mktdata[i,]$Close - (2.5 * mktdata[i,]$atr))[1,]
        if (newstop > longstop) {
            longstop <- newstop
        }
    }
    # Sell signals, (stopLoss || crossBack || notIncreasing)
    if (longpos > 0 && (mktdata[i,]$Close < longstop || !is.na(mktdata[i,]$MACrossback) || mktdata[i,]$CloseRise < 0.998)) {
        addTxn("default", symbol, index(mktdata[i,]), -1 * longpos, mktdata[i,]$Close, TxnFees=TxnFees)
        longpos <- 0
    }
    # short stopLoss
    if (shortpos > 0 && mktdata[i,]$Close > shortstop) {
        addTxn("default", symbol, index(mktdata[i,]), shortpos, mktdata[i,]$Close, TxnFees=TxnFees)
        shortpos <- 0
    }
    if (!is.na(mktdata[i,]$MACrossover) && mktdata[i,]$rsi5 > 50) {
        if (shortpos > 0) {
            addTxn("default", symbol, index(mktdata[i,]), shortpos, mktdata[i,]$Close, TxnFees=TxnFees)
            shortpos <- 0
        }
        longstop <- data.frame(mktdata[i,]$Close - (stoplossAtrMultiplier * mktdata[i,]$atr))[1,]
        buyamt <- floor(80 / data.frame(mktdata[i,]$Close - longstop)[1,])
        longpos <- longpos + buyamt
        addTxn("default", symbol, index(mktdata[i,]), buyamt, mktdata[i,]$Close, TxnFees=TxnFees)
    }
    ## if (!is.na(mktdata[i,]$MACrossback)) {
    ##     if (longpos > 0) {
    ##         addTxn("default", symbol, index(mktdata[i,]), -1 * longpos, mktdata[i,]$Close, TxnFees=TxnFees)
    ##         longpos <- 0
    ##     }
    ##     shortstop <- data.frame(mktdata[i,]$Close + (stoplossAtrMultiplier * mktdata[i,]$atr))[1,]
    ##     buyamt <- floor(80 / data.frame(shortstop - mktdata[i,]$Close)[1,])
    ##     shortpos <- shortpos + buyamt
    ##     addTxn("default", symbol, index(mktdata[i,]), -1 * buyamt, mktdata[i,]$Close, TxnFees=TxnFees)
    ## }
}


###
updatePortf("default")
updateAcct()
updateEndEq(Account="default")
t(tradeStats("default"))

#tradeQuantiles("default", symbol)

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

myChartPosn("default", symbol, mktdata["2014-09"])

dev.off()


# this one causes problems with `chartSeries' (because it has the word "low" in it)
#
mktdata <- mktdata[, !(colnames(mktdata) %in% c("SMASlow", "CloseRise"))]


png(filename = "ex_v1/ta.ex1.png",
    width = 1000, height = 400, units = "px", pointsize = 12,
    bg = "white",
    type = c("cairo", "cairo-png", "Xlib", "quartz"))

chart_Series(mktdata["2014-12"])
#chart_Series(mktdata)

add_TA(mktdata$AtrStop, on = 1)

add_TA(mktdata$SMAFast,col=4,on=1)

dev.off()


# Longest draw-down calculation
# Initialize the new column
ts <- perTradeStats("default")
ts$ddStreakNum <- NA
for (i in 1:nrow(ts)) {
    prev <- ifelse(i == 1, 0, ts[i-1,]$ddStreakNum)
    ts[i,]$ddStreakNum <- ifelse(ts[i,]$Net.Trading.PL < 0, 1 + prev, 0)
}
longestDd <- max(ts$ddStreakNum)
print(paste("Longest drawdown is", longestDd))

# Longest draw-down calculation (using rle())
ts$isWin <- ts$Net.Trading.PL > 0
streaks <- rle(ts$isWin)
losingStreaks <- streaks$lengths[streaks$values == FALSE]
longestDd <- max(losingStreaks)
print(paste("Longest drawdown is", longestDd))
# Way of the Turtle, pg 188
avgMaxDd <- mean(tail(losingStreaks[sort.list(losingStreaks)], n = 5))
print(paste("Average maximum drawdown is", avgMaxDd))

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
