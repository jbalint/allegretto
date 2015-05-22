# Attempt to implement a strategy similar to that defined here:
# http://alvarezquanttrading.com/2014/08/11/simple-ideas-for-a-mean-reversion-strategy-with-good-results/

require(blotter)
require(quantstrat)


.slow <- 50
.fast <- 10
# Trade history should be loaded already (c.f. mysql_import.R)
symbol <- "VYM"
mktdata <- get(symbol)
#
currency("USD")
stock(symbol, currency="USD")
Sys.setenv(TZ="UTC")
rm.strat("default")
initPortf(symbols=c(symbol))
strategy("default", store = TRUE)
initOrders(portfolio="default")
initAcct(portfolios="default", initEq=10000)
# Two moving averages used, need the [,1] because Cl() will return several columns once an SMA based on Close is added
#add.indicator("default", "SMA", list(x = quote(Cl(mktdata)[,1]), n = .fast), label = "SMAFast")
#add.indicator("default", "SMA", list(x = quote(Cl(mktdata)[,1]), n = .slow), label = "SMASlow")
# these are transfered to `mktdata' on applyStrategy(..., mktdata=mktdata)
mktdata$SMAFast <- SMA(Cl(mktdata), n = .fast)
mktdata$SMASlow <- SMA(Cl(mktdata), n = .slow)
mktdata$PrevCl <- lag(Cl(mktdata), k = 1)
mktdata$IsInRange <- Cl(mktdata) < mktdata$SMAFast # && Cl(mktdata) > mktdata$SMASlow
mktdata$IsInRange2 <- mktdata$IsInRange ^ lag(mktdata$IsInRange, k = -1)

## findCol <- function(data, col) {
##     data[,grep(col,colnames(data))]
## }
#add.signal("default", "sigCrossover", list(columns = c("Close", "SMA10"), relationship = "gt"), label = "Cl.gt.SMA100")
#add.signal("default", "sigCrossover", list(columns = c("Close", "SMA10"), relationship = "lt"), label = "Cl.lt.SMA5")
# First try
#add.signal("default", "sigFormula", list(formula="mktdata$ORCL.Close < mktdata$ORCL.Close.SMA.5.SMAFast & mktdata$ORCL.Close > mktdata$ORCL.Close.SMA.100.SMASlow"), label = "between_SMA")
# Second try, a little nicer
#add.signal("default", "sigFormula", list(formula="Cl(mktdata)[,1] < findCol(mktdata, 'SMAFast') & Cl(mktdata)[,1] > findCol(mktdata, 'SMASlow')"), label = "between_SMA")
# final formula is clean
## add.signal("default", "sigFormula",
##            list(formula="Cl(mktdata) < mktdata$SMAFast & Cl(mktdata) > mktdata$SMASlow"),
##            label = "between_SMA")
add.signal("default", "sigFormula",
           list(formula="Cl(mktdata) < mktdata$PrevCl"),
           label = "close_higher")
## add.signal("default", "sigComparison",
##            list(columns="IsInRange", relationship="eq"),
##            label = "between_SMA")

add.rule("default", "ruleSignal",
         list(sigcol = "IsInRange", sigval = TRUE, orderqty = 900, ordertype = "market", orderside = "long"), type = "enter")
add.rule("default", "ruleSignal",
         list(sigcol = "close_higher", sigval = TRUE, orderqty = "all", ordertype = "market", orderside = "long"), type = "exit")
applyStrategy(strategy="default", portfolios="default", mktdata=mktdata)

###
updatePortf("default")
updateAcct()
updateEndEq(Account="default")
t(tradeStats("default"))

chart.Posn("default", symbol, TA="add_TA(mktdata$SMAFast,col=4,on=1);add_TA(mktdata$SMASlow,col=6,on=1)")
