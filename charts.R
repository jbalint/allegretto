# Charts....

# Re-use `mktdata' from strategy_x3.R

mktdata$n <- row(mktdata)[,1]

#chart_Series(my.OHLC(mktdata["2015-05/"]))

png(filename = "ta.ex3.png",
    width = 8000, height = 500, units = "px", pointsize = 12,
    bg = "white",
    type = c("cairo", "cairo-png", "Xlib", "quartz"))

#chart_Series(my.OHLC(mktdata), subset="2015-04-01/2015-04-02")


chart_Series(my.OHLC(mktdata), pars=list(mar=c(3,0,0,0)))#, subset="2015-05/2015-06")

add_TA(mktdata[,"SMASlow"], on=1, col="#00FFFF")
add_TA(mktdata[,"SMAFast"], on=1, col="#FF7F50")
add_TA(mktdata[!is.na(mktdata$MACrossover), "Close"], pch=2, type='p', col='blue', on=1)
add_TA(mktdata[!is.na(mktdata$MACrossback), "Close"], pch=6, type='p', col='red', on=1)
#add_Vo()

# Merge buy/sell (enter/exit) pairs

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

# draw lines for each trade.

# NOTE: this doesn't work well with add_Vo() because it redraws the
# graph. When I do it after add_Vo(), then it draws it on the volume
# graph. Need to fix this.
for (i in 1:nrow(pairs)) {
    col <- ifelse(pairs[i,]$EntryPrice < pairs[i,]$ExitPrice, "green", "red")
    #lines(c(pairs[i,]$EntryN, pairs[i,]$ExitN), c(pairs[i,]$EntryPrice, pairs[i,]$ExitPrice), col=col, type='h')
    lines(c(pairs[i,]$EntryN, pairs[i,]$ExitN), c(pairs[i,]$EntryPrice, pairs[i,]$ExitPrice), col=col, lwd=1)
}

# 1351-1367 # TYPE=H IS VERY USEFUL. DON'T FORGET IT
#lines(c(1351,1367), c(71.42,71.31), lwd=2, col="red")#, type="h")

#lines(c(607,625), c(67.6,68.31), lwd=2, col="green")#, type="h")
#lines(c(607,625), c(67.6,68.31), col="green", type="h")

dev.off()

add_WMA()

add_Vo()

add_SMA()

add_MACD()

add_BBands()
