require(ggplot2)
require(quantmod)
require(blotter)
require(quantstrat)
require(TTR)
source("multiplot.R")
source("my_init.R")

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

sink("perTradeStats.txt")
toOrgTable(pts)
sink()

# Calculate the longest DD and avg max DD
pts$isWin <- pts$Net.Trading.PL > 0
streaks <- rle(pts$isWin)
losingStreaks <- streaks$lengths[streaks$values == FALSE]
longestDd <- max(losingStreaks)
# Way of the Turtle, pg 188
avgMaxDd <- mean(tail(losingStreaks[sort.list(losingStreaks)], n = 5))
# TODO: longest DD duration (days)

sink(filenames["stats"])
cat("This is the stats file generated from the strategy test.\n")
# Line 2
cat(paste("+ Symbol: *~", symbol, "~*\n", sep=""))
cat(paste("+ Time period: ", strftime(index(mktdata[1,]), format="%Y-%m-%d"), " / ", strftime(index(mktdata[nrow(mktdata),]), format="%Y-%m-%d"), "\n", sep=""))
# Line 4
cat("| *Expectancy* (Profit Factor) | ", round(ts$Profit.Factor, 2), "|\n")
cat("| *Total Net Profit* | $", round(ts$Net.Trading.PL, 0), "|\n")
cat("| *Max DD* | $", round(ts$Max.Drawdown, 0), "|\n")
cat("| *Longest DD* |", format(longestDd), "|\n")
cat("| *Avg Max DD* |", format(avgMaxDd, nsmall=1), "|\n")
cat("| Trades (pairs) |", nrow(pts), "|\n")
cat("| Avg Net Profit | $", round(ts$Avg.Trade.PL, 2), "|\n")
sink()

sink(filenames["winloss.stats"])
cat("||*Winning Trade*|*Losing Trade*|\n")
cat("|Gross Profit| $", round(ts$Gross.Profits, 0), " | $", round(ts$Gross.Losses, 0), "|\n")
cat("|Largest| $", round(ts$Largest.Winner, 0), " | $", round(ts$Largest.Loser, 0), "|\n")
cat("|Average| $", round(ts$Avg.Win.Trade, 0), " | $", round(ts$Avg.Losing.Trade, 0), "|\n")
cat("|Number| ", nrow(pts[pts$isWin, c()]), "|", nrow(pts[!pts$isWin, c()]), "|\n")
cat("|Percent| ", round(ts$Percent.Positive, 0), "% | ", round(ts$Percent.Negative, 0), "% |\n")
sink()

########################################
# Return Distribution & MAE/MFE Graphs #
########################################

# Return distribution - calculation
pts$WinLoss <- ifelse(pts$Net.Trading.PL > 0, "Winning Trades", "Losing Trades")
pts$RMulti <- pts$Net.Trading.PL / maxRiskPerTrade
returnsXwidth <- ceiling(max(abs(pts$RMulti)) / 2) * 2
# TODO: this works well but puts the wrong X labels on the "Losing
# Trades" size. The next improvement to this would involve generating
# multiple graphs and comining them together
pts$RMulti <- ifelse(pts$RMulti < 0, pts$RMulti + returnsXwidth, pts$RMulti)

# MAE/MFA - calculation
pts$isWin <- pts$Net.Trading.PL > 0
# "limit" the profit for the wins so we have a resonable plot. we
# don't care as much about profitable trades here as non-profitable
# trades
# - first, get max loss
maxLossAbs <- abs(min(pts$Net.Trading.PL))
# - second, calculate a special PL for MAE graph that doesn't go past
# the max loss, for wins
pts$PL.MAE <- ifelse(pts$Net.Trading.PL > maxLossAbs, maxLossAbs, pts$Net.Trading.PL)

# Return distribution & MAE/MFE combined graphs
png(filename = filenames["return.mae.mfe"],
    width = 1000, height = 300, units = "px", pointsize = 12,
    bg = "white",
    type = c("cairo", "cairo-png", "Xlib", "quartz"))
returnGraph <- (ggplot(pts, aes(x=RMulti, fill=WinLoss))
    # TODO: figure out good `bindwidth' values
    +geom_histogram(binwidth=.25, color="black")
    +scale_fill_manual(values=c("#FF9999", "#90FF90"))
    +ylab("Number of Trades")
    +xlab("R-Multiple of Win/Loss")
    +xlim(0, returnsXwidth)
    +ggtitle("R-Multiple Distribution")
    +facet_grid(. ~ WinLoss)#, scales="free")
    +theme(legend.position="none")
    +theme(strip.text = element_text(face = "bold"))
    +theme(axis.title = element_text(face = "bold"))
    +theme(plot.title = element_text(face = "bold"))  # http://docs.ggplot2.org/0.9.2.1/element_text.html
    #+theme(strip.background = element_rect(fill="#90FF90"))
    #+theme(panel.border = element_rect(size=1)) # this seems completely broken
)
# size of MAE/MFE graphs
maeMfeXwidth <- ceiling(max(pmax(pts$PL.MAE, pts$Net.Trading.PL)) / 200) * 200
mae <- (ggplot(pts, aes(x=PL.MAE, y=MAE, color=isWin, shape=isWin))
    +geom_point()
    +scale_colour_manual(values=c("#FF9999", "#00CC00"))
    +geom_smooth(method=glm, se=FALSE)
    +xlab("Trade Profit")
    +ylab("MAE")
    +xlim(-maeMfeXwidth, maeMfeXwidth)
    +theme(legend.position="none")
    +theme(axis.title = element_text(face = "bold"))
)
mfe <- (ggplot(pts, aes(x=Net.Trading.PL, y=MFE, color=isWin, shape=isWin))
    +geom_point()
    +scale_colour_manual(values=c("#FF9999", "#00CC00"))
    +geom_smooth(method=glm, se=FALSE)
    +xlab("Trade Profit")
    +ylab("MFE")
    +xlim(-maeMfeXwidth, maeMfeXwidth)
    +theme(legend.position="none")
    +theme(axis.title = element_text(face = "bold"))
)
multiplot(returnGraph, mae, mfe, cols=3)
dev.off()

###########################################################
# Candles with Technical Indicators and Order Entry/Exits #
###########################################################

mktdata$n <- row(mktdata)[,1]
# TODO: this is SYSTEM-SPECIFIC. I need to add a "standard" entry/exit
# field so correlate the trades, OR use the per-trade stats (pts) to
# do this
entries <- data.frame(mktdata[!is.na(mktdata$MACrossover)])
entries$EntryN <- entries$n
entries$EntryPrice <- entries$Close
entries$newN <- row(entries)[,1]
exits <- data.frame(mktdata[!is.na(mktdata$MACrossback)])
exits$ExitN <- exits$n
exits$ExitPrice <- exits$Close
# TODO: make sure we don't have an exit before an entry. this should be handled properly with the order book
if (exits[1,]$ExitN < entries[1,]$EntryN) {
    exits <- exits[-1,]
}
exits$newN <- row(exits)[,1]
pairs <- merge(entries[,c("EntryN", "EntryPrice", "newN")], exits[,c("ExitN", "ExitPrice", "newN")], by="newN")

png(filename = filenames["entry.exit"],
    width = 8000, height = 500, units = "px", pointsize = 12,
    bg = "white",
    type = c("cairo", "cairo-png", "Xlib", "quartz"))
# we need print here, as per http://grokbase.com/t/r/r-help/091gdnyvxa/r-xyplot-in-lattice-package
print(chart_Series(my.OHLC(mktdata), pars=list(cex=1, mar=c(3,0,0,0))))
print(add_TA(mktdata[,"SMASlow"], on=1, col="#00FFFF"))
print(add_TA(mktdata[,"SMAFast"], on=1, col="#FF7F50"))
print(add_TA(mktdata[!is.na(mktdata$MACrossover), "Close"], pch=2, type='p', col='green', on=1))
print(add_TA(mktdata[!is.na(mktdata$MACrossback), "Close"], pch=6, type='p', col='red', on=1))
print(add_BBands())
# NOTE: this doesn't work well with add_Vo() because it redraws the
# graph. When I do it after add_Vo(), then it draws it on the volume
# graph. Need to fix this.
for (i in 1:nrow(pairs)) {
    col <- ifelse(pairs[i,]$EntryPrice < pairs[i,]$ExitPrice, "green", "red")
    #lines(c(pairs[i,]$EntryN, pairs[i,]$ExitN), c(pairs[i,]$EntryPrice, pairs[i,]$ExitPrice), col=col, type='h')
    lines(c(pairs[i,]$EntryN, pairs[i,]$ExitN), c(pairs[i,]$EntryPrice, pairs[i,]$ExitPrice), col=col, lwd=1)
}
dev.off()

## png(filename = filenames["entry.exit"],
##     width = 8000, height = 500, units = "px", pointsize = 12,
##     bg = "white",
##     type = c("cairo", "cairo-png", "Xlib", "quartz"))
## #par(oma=c(0,0,0,0))
## chart_Series(my.OHLC(mktdata), pars=list(cex=1, mar=c(3,0,0,0)))
## add_TA(mktdata[,"SMASlow"], on=1, col="#00FFFF")
## add_TA(mktdata[,"SMAFast"], on=1, col="#FF7F50")
## add_TA(mktdata[!is.na(mktdata$MACrossover), "Close"], pch=2, type='p', col='green', on=1)
## add_TA(mktdata[!is.na(mktdata$MACrossback), "Close"], pch=6, type='p', col='red', on=1)
## add_Vo()
## add_BBands()
## dev.off()

#####################################################
# Equity Curve & Drawdown (normal quantstrat graph) #
#####################################################

TA <- "add_TA(mktdata$SMAFast,col=4,on=1);add_TA(mktdata$SMASlow,col=6,on=1)"
par(global.pars)
png(filename = filenames["equity.drawdown"],
    width = 1200, height = 600, units = "px", pointsize = 12,
    type = c("cairo", "cairo-png", "Xlib", "quartz"))
myChartPosn("default", symbol, my.OHLC(mktdata), TA=TA)
dev.off()

# Extras

# some further calculation
# Merge in the atr from the mktdata, use "Start" as the name for the merge
pts.atr <- data.frame(Start=index(mktdata), coredata(mktdata$atr))

pts2 <- merge(pts, pts.atr, all.x = TRUE)
# some E-Ratio-like measure of the system in backtest, not JUST entry
# edge, but total result using MFE/MAE
eResult <- (sum(pts2$MFE / pts2$atr) / nrow(pts2)) / (sum(abs(pts2$MAE) / pts2$atr) / nrow(pts2))
