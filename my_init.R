# My R init stuff. Load the quantstrat modules locally so I can view/edit the source code more easier within emacs
require(devtools)

# not sure what this really does (R is a pain in the ass)
Sys.setenv(R_KEEP_PKG_SOURCE="yes")

load_all("/home/jbalint/sw/quantmod")
load_all("/home/jbalint/Dropbox/Apps/R-trading-packages/FinancialInstrument")
load_all("/home/jbalint/Dropbox/Apps/R-trading-packages/blotter")
load_all("/home/jbalint/Dropbox/Apps/R-trading-packages/quantstrat")

# my version of mysql import from my db
source("mysql_import.R")
myGetSymbol('VYM')
myGetSymbol('AAPL')
myGetSymbol('MSFT')
myGetSymbol('CRM')
myGetSymbol('SCCO')

# Copied so I can pass `mktdata' directly
myChartPosn <- function(Portfolio, Symbol, mktdata, Dates = NULL, ...,TA=NULL)
{
    pname<-Portfolio
    Portfolio<-getPortfolio(pname)

    Prices = mktdata
    freq = periodicity(Prices)
    switch(freq$scale,
            seconds = { mult=1 },
            minute = { mult=60 },
            hourly = { mult=3600 },
            daily = { mult=86400 },
            {mult=86400}
    )
    if(!isTRUE(freq$frequency*mult == round(freq$frequency,0)*mult)) {
        # if the equality
        n=round((freq$frequency/mult),0)*mult
    } else { n=mult }

    tzero = xts(0,order.by=index(Prices[1,]))
    if(is.null(Dates)) Dates<-paste(first(index(Prices)),last(index(Prices)),sep='::')

    #scope the data by Dates
    Portfolio$symbols[[Symbol]]$txn<-Portfolio$symbols[[Symbol]]$txn[Dates]
    Portfolio$symbols[[Symbol]]$posPL<-Portfolio$symbols[[Symbol]]$posPL[Dates]

    Trades = Portfolio$symbols[[Symbol]]$txn$Txn.Qty

    Buys = Portfolio$symbols[[Symbol]]$txn$Txn.Price[which(Trades>0)]
    Sells = Portfolio$symbols[[Symbol]]$txn$Txn.Price[which(Trades<0)]

    Position = Portfolio$symbols[[Symbol]]$txn$Pos.Qty

    if(nrow(Position)<1) stop ('no transactions/positions to chart')

    if(as.POSIXct(first(index(Prices)))<as.POSIXct(first(index(Position)))) Position<-rbind(xts(0,order.by=first(index(Prices)-1)),Position)
    Positionfill = na.locf(merge(Position,index(Prices)))

    CumPL = cumsum(Portfolio$symbols[[Symbol]]$posPL$Net.Trading.PL)
    if(length(CumPL)>1)
        CumPL = na.omit(na.locf(merge(CumPL,index(Prices))))
    else
        CumPL = NULL

    if(!is.null(CumPL)) {
        CumMax <- cummax(CumPL)
        Drawdown <- -(CumMax - CumPL)
        Drawdown<-rbind(xts(-max(CumPL),order.by=first(index(Drawdown)-1)),Drawdown)
    } else {
        Drawdown <- NULL
    }
    #     # These aren't quite right, as abs(Pos.Qty) should be less than prior abs(Pos.Qty)
    # SellCover = Portfolio$symbols[[Symbol]]$txn$Txn.Price * (Portfolio$symbols[[Symbol]]$txn$Txn.Qty<0) * (Portfolio$symbols[[Symbol]]$txn$Pos.Qty==0)
    # BuyCover = Portfolio$symbols[[Symbol]]$txn$Txn.Price * (Portfolio$symbols[[Symbol]]$txn$Txn.Qty>0) * (Portfolio$symbols[[Symbol]]$txn$Pos.Qty==0)
    #
    #     #Symbol 24 (up) and 25 (dn) can take bkgd colors
    # addTA(BuyCover,pch=24,type="p",col="green", bg="orange", on=1)
    # addTA(SellCover,pch=25,type="p",col="red", bg="orange", on=1)

    # scope the Price data by Dates
    if(!is.null(Dates)) Prices=Prices[Dates]

    chart_Series(Prices, name=Symbol, TA=TA, ...)
    if(!is.null(nrow(Buys)) && nrow(Buys) >=1 ) (add_TA(Buys,pch=2,type='p',col='green', on=1));
    if(!is.null(nrow(Sells)) && nrow(Sells) >= 1) (add_TA(Sells,pch=6,type='p',col='red', on=1));
    if(nrow(Position)>=1) {
        (add_TA(Positionfill,type='h',col='blue', lwd=2))
        (add_TA(Position,type='p',col='orange', lwd=2, on=2))
    }
    if(!is.null(CumPL))  (add_TA(CumPL, col='darkgreen', lwd=2))
    if(!is.null(Drawdown)) (add_TA(Drawdown, col='darkred', lwd=2, yaxis=c(0,-max(CumMax))))
    plot(current.chob())
}
