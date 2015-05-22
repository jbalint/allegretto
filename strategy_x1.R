# some basic strategy code
require(blotter)
require(quantstrat)

# ORCL should be loaded already (c.f. mysql_import.R)

currency("USD")
stock("ORCL", currency="USD")
Sys.setenv(TZ="UTC")
rm.strat("default")
initPortf(symbols=c("ORCL"))
strategy("default", store = TRUE)
initOrders(portfolio="default")
initAcct(portfolios="default", initEq=10000)

# actual strategy definition
add.indicator("default", "SMA", list(x = quote(Cl(mktdata)), n = 10), label = "SMA10")
add.indicator("default", "BBands", list(x = quote(mktdata), "SMA"), label = "BBands")

add.signal("default", "sigCrossover", list(columns = c("Close", "SMA10"), relationship = "gt"), label = "Cl.gt.SMA")
add.signal("default", "sigCrossover", list(columns = c("Close", "SMA10"), relationship = "lt"), label = "Cl.lt.SMA")
add.rule("default", "ruleSignal", list(sigcol = "Cl.gt.SMA", sigval = TRUE, orderqty = 900, ordertype = "market", orderside = "long"), type = "enter")
add.rule("default", "ruleSignal", list(sigcol = "Cl.lt.SMA", sigval = TRUE, orderqty = "all", ordertype = "market", orderside = "long"), type = "exit")

applyStrategy(strategy="default", portfolios="default")

updatePortf("default")
updateAcct()
updateEndEq(Account="default")
#t(perTradeStats("default"))
t(tradeStats("default"))

# attempted, doesn't work
add_Close <- function(on=1) {
    plot_object <- current.chob()
    lenv <- new.env()
    lenv$xdata <- ORCL$ORCL.Close
    plot_object$set_frame(sign(on)*(abs(on)+1L))
    plot_object$add(quote(),env=c(lenv, plot_object$Env),expr=TRUE)
    plot_object
}
chart.Posn("default", "ORCL", TA="add_Close()");

chart.Posn("default", "ORCL", TA="add_BBands();add_SMA(n=50,col=4,on=1,lwd=2)")


# raw graphing from quantmod:
barChart(ORCL)
addTA(SMA(ORCL$ORCL.Close), on = 1, col = 7)
addTA(SMA(ORCL$ORCL.Close)*1.025, on = 1, col = 8)
addTA(SMA(ORCL$ORCL.Close)*0.975, on = 1, col = 8)
