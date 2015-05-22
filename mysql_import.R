# Import trade history from MySQL. I use a different data model than quantmod supports so I use these routines instead
require(RMySQL)
require(DBI)
require(xts)

# Load trade history data from the internal storage
loadHistory <- function (symbol, id) {
    con <- DBI::dbConnect(RMySQL::MySQL(), user="root", password="", dbname="thinkorswim", host="127.0.0.1", port=3319)
    query <- paste("select time, open, high, low, close, volume, v_w_a_p from trade_history where capture_id = '", id, "'", sep="")
    rs <- DBI::dbSendQuery(con, query)
    # TODO: check nrow(fr) and if it's zero, bail with an error
    fr <- DBI::fetch(rs, n=-1)
    # TODO: currently all data (afaik) is in CDT timezone
    fr2 <- xts(as.matrix(fr[,-1]),order.by=as.POSIXct(fr$time, tz="America/Chicago", origin="1970-01-01"))
    colnames(fr2) <- paste(symbol, c('Open','High','Low','Close','Volume','Adjusted'), sep='.')
    DBI::dbDisconnect(con)
    return(fr2)
}

# And assign it in the global environment like `getSymbol()'
myGetSymbol <- function (symbol, id) {
    data <- loadHistory(symbol, id)
    assign(symbol, data, envir = globalenv())
}
