# Return distribution ala Way of the Turtle (C. Faith) pg 57/58

symbol <- "AAPL"
account <- 5000

require(ggplot2)

################## Final result to generate a graph
# get the trade order stats (includes P/L per trade pair)
tos <- tradeOrderStats("default", symbol)
# calculate the percent return
pctReturn <- period.max(tos$Net.Trading.PL, endpoints(tos, on = "weeks")) / account
pctReturn <- pctReturn * 100
colnames(pctReturn) <- c('PL')
# split out pos/negative for coloring on the table
pctReturn$positive <- pctReturn > 0
# graphing functions
ggplot(pctReturn, aes(x=PL, fill=as.factor(positive))) +
    geom_histogram(binwidth=.25, color="black") +
        # this sets my colors to green/red
        scale_fill_manual(values=c("#FF9999", "#90FF90"))

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

