# Blotter examples
require(blotter)

# Init stuff
currency("USD")
stock("AAPL", currency="USD")
stock("ORCL", currency="USD")
fund("SLV", currency="USD")
Sys.setenv(TZ="UTC")
#getSymbols("AAPL", from="2015-01-01", to="2015-05-01", index.class="POSIXct", adjust=T)
#getSymbols("SLV", from="2015-01-01", to="2015-05-01", index.class="POSIXct", adjust=T)

# Blotter creates an environment called ".blotter". You can see it by
# calling ls(all=T) or ls(.blotter)

##############
# Portfolios #
##############

# Portfolio objects are stored as "portfolio.xyz" in the .blotter
# environment. You can see it: ls(.blotter$portfolio.xyz)
rm(portfolio.xyz, envir=.blotter) # easier to re-eval buffer in same session
initPortf(name="xyz", c("AAPL"))

# Portfolio objects contain transactions. You can add them like so:
addTxn("xyz", "AAPL", "2015-05-19 12:00:00", 10, 120.07)
addTxn("xyz", "AAPL", "2015-05-19 14:00:00", -10, 130.07, TxnFees=-40)

# addPortfInstr() # what does this actually do?
#addPortfInstr("xyz", "SLV")
addTxn("xyz", "SLV", "2015-05-19 12:00:00", 100, 15.07)
addTxn("xyz", "SLV", "2015-05-19 14:00:00", -100, 16.07, TxnFees=-40)

# Each item will show an individual P/L for the tx
updatePortf("xyz")

getTxns("xyz", "AAPL")
getTxns("xyz", "ORCL")
getTxns("xyz", "SLV")

# makes it a little easier to read if your terminal can handle it:
# c.f. http://stackoverflow.com/questions/1172485/how-to-increase-the-number-of-columns-using-r-in-linux
#options(width=as.integer(260))
# transpose looks even better
t(tradeStats("xyz"))

############
# Accounts #
############

# Account objects are stored as "account.default" in .blotter
# environment.
rm(account.default, envir=.blotter)
initAcct(portfolios="xyz", initEq=10000)

updateAcct()
updateEndEq(Account="default")

# http://www.investopedia.com/terms/s/sharperatio.asp

############
# Strategy #
############
require(quantstrat) # now starting quantstrat stuff

initOrders("xyz")

strat <- strategy("")
summary(strat)

indicator.func <- "SMA" # function name (from TTR package)
# the second param here is the list of arguments to SMA
# "mktdata" is a special object (c.f. "quantstrat.pdf" (Guy Yollin) pg 34)
strat2 <- add.indicator(strat, "SMA", list(x = quote(Cl(mktdata)), n = 10), label = "SMA10")

signal.func <- "sigCrossover" # function name (c.f. add.signal() help)
strat3 <- add.signal(strat2, signal.func, list(columns = c("Close", "SMA10"), relationship = "gt"), label = "Cl.gt.SMA")
strat4 <- add.signal(strat3, signal.func, list(columns = c("Close", "SMA10"), relationship = "lt"), label = "Cl.lt.SMA")

rule.func <- "ruleSignal" # ???
strat5 <- add.rule(strat4, rule.func, list(sigcol = "Cl.gt.SMA", sigval = TRUE, orderqty = 900, ordertype = "market", orderside = "long"), type = "enter")
strat6 <- add.rule(strat5, rule.func, list(sigcol = "Cl.lt.SMA", sigval = TRUE, orderqty = "all", ordertype = "market", orderside = "long"), type = "exit")

applyStrategy(strat6, "xyz")

chart.Posn("xyz", "AAPL", TA="add_SMA(n=10,col=4,on=1,lwd=2)")

t(perTradeStats("xyz"))
