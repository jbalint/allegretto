# continuating of backtesting methodology
# using "Walking Zombie": http://forums.babypips.com/free-forex-trading-systems/65437-walking-zombie-system.html

# System input
TxnFees <- -10
stoplossAtrMultiplier <- 3
# R
maxRiskPerTrade <- 80
filenames <- c()
filenames["stats"] <- "stats.txt"
filenames["winloss.stats"] <- "winloss.stats.txt"
filenames["return.mae.mfe"] <- "ex_v1/returns.mae.mfe.ex2.png"
filenames["entry.exit"] <- "ex_v1/entry.exit.ex2.png"
filenames["equity.drawdown"] <- "ex_v1/equity.drawdown.ex2.png"

global.pars <- par()

################################################
# Load data and calculate technical indicators #
################################################
symbol <- "MTUM"
myGetSymbol(symbol)
mktdata <- get(symbol)
mktdata <- merge.xts(mktdata, ADX(mktdata))
mktdata$SMAFast <- SMA(mktdata$Close, n = 10)
mktdata$SMASlow <- SMA(mktdata$Close, n = 20)
mktdata <- merge.xts(mktdata, ATR(mktdata))
mktdata$AtrStop <- lag(mktdata$Close, k=1) - (2 * lag(mktdata$atr, k=1))
mktdata$AtrStopout <- mktdata$Close < mktdata$AtrStop
mktdata <- merge.xts(mktdata, sigCrossover("MACrossover", columns=c("SMAFast", "SMASlow")))
mktdata <- merge.xts(mktdata, sigCrossover("MACrossback", columns=c("SMASlow", "SMAFast")))
#mktdata$rsi5 <- RSI(mktdata$Close, n=5, maType="WMA", wts=mktdata[,"Volume"])
#mktdata$maxOhlc <- pmax(mktdata$Open, mktdata$High, mktdata$Low, mktdata$Close)
#mktdata$CloseRise <- mktdata$Close / lag(mktdata$maxOhlc)

mktdata <- merge.xts(mktdata, stoch(mktdata$Close, nFastK=10))

##############################
# Initialize Blotter Objects #
##############################
currency("USD")
stock(symbol, currency="USD")
Sys.setenv(TZ="America/Chicago") # TODO: find their issue with non-UTC TZs
rm.strat("default")
initPortf(symbols=c(symbol))
strategy("default", store = TRUE)
initOrders(portfolio="default")
initAcct(portfolios="default", initEq=10000)

##################################
# Apply Strategy & Create Orders #
##################################
longstop <- 0
longpos <- 0
shortstop <- 0
shortpos <- 0
for (i in 1:nrow(mktdata)) {
    if (is.na(mktdata[i,]$atr)) {
        # Too early to calculate necessary metrics
        next
    }
    if (!is.na(mktdata[i,]$MACrossback)) {
        # exit long and enter short
        if (longpos > 0) {
            addTxn("default", symbol, index(mktdata[i,]), -1 * longpos, mktdata[i,]$Close, TxnFees=TxnFees)
            longpos <- 0
        }
        shortstop <- data.frame(mktdata[i,]$Close + (stoplossAtrMultiplier * mktdata[i,]$atr))[1,]
        shortpos <- floor(maxRiskPerTrade / data.frame(shortstop - mktdata[i,]$Close)[1,])
        addTxn("default", symbol, index(mktdata[i,]), -1 * shortpos, mktdata[i,]$Close, TxnFees=TxnFees)
    }
    if (!is.na(mktdata[i,]$MACrossover)) {
        # exit short and enter long
        if (shortpos > 0) {
            addTxn("default", symbol, index(mktdata[i,]), shortpos, mktdata[i,]$Close, TxnFees=TxnFees)
            shortpos <- 0
        }
        longstop <- data.frame(mktdata[i,]$Close - (stoplossAtrMultiplier * mktdata[i,]$atr))[1,]
        longpos <- floor(maxRiskPerTrade / data.frame(mktdata[i,]$Close - longstop)[1,])
        addTxn("default", symbol, index(mktdata[i,]), longpos, mktdata[i,]$Close, TxnFees=TxnFees)
    }
}
# crunch orders into stats
updatePortf("default")
updateAcct()
updateEndEq(Account="default")
ts <- tradeStats("default")
pts <- perTradeStats("default")
