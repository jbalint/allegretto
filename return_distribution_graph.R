# Return distribution ala Way of the Turtle (C. Faith) pg 57/58

symbol <- "AAPL"
account <- 5000

require(ggplot2)

# Trade order stats looks like this
#                     Order.Qty Order.Price Order.Type Order.Side Order.Threshold Order.Status Order.StatusTime      Prefer Order.Set Txn.Fees Rule              Time.In.Force Start                 End                   Init.Pos Max.Pos Num.Txns
# 2014-09-10 14:00:00 "all"     "67.219"    "market"   "long"     NA              "closed"     "2014-09-10 14:00:00" ""     NA        "0"      "ruleSignal.rule" ""            "2014-09-08 16:00:00" "2014-09-10 19:00:00" "900"    "15300" "18"    
# 2014-09-15 13:00:00 "all"     "67.144"    "market"   "long"     NA              "closed"     "2014-09-15 13:00:00" ""     NA        "0"      "ruleSignal.rule" ""            "2014-09-12 14:00:00" "2014-09-15 18:00:00" "900"    " 9900" "12"    
#                     Max.Notional.Cost Net.Trading.PL MAE         MFE        Pct.Net.Trading.PL Pct.MAE       Pct.MFE      tick.Net.Trading.PL tick.MAE    tick.MFE  
# 2014-09-10 14:00:00 "1028560"         "  1742.13"    " -4200.12" " 1742.13" " 0.00169376"      "-0.00473610" "0.00208398" " 11.3865"          " -31.8543" " 14.0360"
# 2014-09-15 13:00:00 " 664494"         "   786.42"    "  -699.48" "  786.42" " 0.00118349"      "-0.00231532" "0.00118349" "  7.9436"          " -15.5440" "  7.9436"

################## Final result to generate a graph
# get the trade order stats (includes P/L per trade pair)



#pts <- xts(pts$Net.Trading.PL, pts$End)

# calculate the percent return
#pctReturn <- period.max(tos$Net.Trading.PL, endpoints(tos, on = "weeks")) / 10000
#pctReturn <- pctReturn * 100
#pctReturn <- data.frame(pctReturn) # xts can't handle different typed columns?!
colnames(pctReturn) <- c('PL')
# split out pos/negative for coloring on the table
pctReturn$positive <- ifelse(pctReturn$PL > 0, "Winning Trades", "Losing Trades")

# graphing functions

(ggplot(pctReturn, aes(x=PL, fill=positive))
    +geom_histogram(binwidth=2, color="black")
    # this sets my colors to green/red
    +scale_fill_manual(values=c("#FF9999", "#90FF90"))
    +ylab("Trades")
    +xlab("")
    #+xlim(-400, 400)
    +ggtitle("R-Multiple Distribution")
    +facet_grid(. ~ positive, scales="free")
    +theme(legend.position="none")
    +theme(strip.text = element_text(face = "bold"))
    +theme(plot.title = element_text(face = "bold"))  # http://docs.ggplot2.org/0.9.2.1/element_text.html
)

(ggplot(pctReturn, aes(x=PL))
    +geom_histogram(binwidth=.5, color="black", fill="#90FF90")
    +ylab("Trades")
    +ggtitle("R-Multiple Distribution")
  +facet_grid(. ~ pctReturn$positive)
)

last_plot() + (ggplot(pctReturn[!pctReturn$positive], aes(x=PL))
    +geom_histogram(binwidth=10, color="black", fill="#FF9999")
    +ylab("Trades")
    +ggtitle("R-Multiple Distribution")
)

################## exploratory stuff below

# TODO: replace "AAPL" direct use below

# this is an xts object
class(.strategy$order_book.default$default$AAPL)

t(head(.strategy$order_book.default$default$AAPL, n=3))

t(head(.strategy$order_book.default$default$AAPL, n=3))

#t(head(tradeOrderStats("default", "AAPL"), n=3))

tos <- tradeOrderStats("default", symbol)

t(head(tos, n = 4))

pctReturn <- period.max(tos$Net.Trading.PL, endpoints(tos, on = "weeks")) / 5000
pctReturn <- pctReturn * 100
colnames(pctReturn) <- c('PL')
pctReturn$positive <- as.factor(pctReturn > 0)

barplot(pctReturn)

hist(pctReturn, 50, xlim = c(-2, 12), col = c("#90EE90", "black"))
lines(density(pctReturn))

plot(density(pctReturn))

qplot(pctReturn, geom="histogram")
lines(density(pctReturn))

ggplot(pctReturn, aes(x=PL, fill=as.factor(positive))) +
    geom_histogram(binwidth=.25, color="gray") + scale_fill_manual(values=c("#FF9999", "#90FF90"))

ggplot(pctReturn, aes(x=PL)) +
    geom_histogram(aes(y=..density..), binwidth=.5, colour="black", fill="white") +
        geom_density()

