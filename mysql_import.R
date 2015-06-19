# Import trade history from MySQL. I use a different data model than quantmod supports so I use these routines instead
require(RMySQL)
require(DBI)
require(xts)

# Load trade history data from the internal storage
loadHistory <- function (symbol) {
    # TODO: parameterize the DB
    con <- DBI::dbConnect(RMySQL::MySQL(), user="root", password="", dbname="thinkorswim", host="127.0.0.1", port=3319)
    query <- paste("select time, open, high, low, close, volume, vwap from trade_history where symbol = '", symbol, "'", sep="")
    rs <- DBI::dbSendQuery(con, query)
    # TODO: check nrow(fr) and if it's zero, bail with an error
    fr <- DBI::fetch(rs, n=-1)
    # TODO: currently all data (afaik) is in CDT timezone
    fr2 <- xts(as.matrix(fr[,-1]),order.by=as.POSIXct(fr$time, tz="America/Chicago", origin="1970-01-01"))
    # don't use the "SYM.Xxx" prefixed names like quantmod does.
    colnames(fr2) <- c('Open','High','Low','Close','Volume','Adjusted')
    DBI::dbDisconnect(con)
    return(fr2)
}

# And assign it in the global environment like `getSymbol()'
myGetSymbol <- function (symbol) {
    data <- loadHistory(symbol)
    assign(symbol, data, envir = globalenv())
}
