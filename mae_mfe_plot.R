# MAE / MFE scatter plot ala:
# http://www.tradingheroes.com/mfe-and-mae-deconstructed-and-how-they-can-help-your-trading/
# NOTE: need to eval the multiplot() function at the bottom before drawing the plot

source("multiplot.R")

# ggplot2 reference:
# http://www.cookbook-r.com/Graphs/Scatterplots_(ggplot2)/

library(ggplot2)

# need to run the analysis first to get perTradeStats()
ts <- perTradeStats("default")
ts$isWin <- ts$Net.Trading.PL > 0

# "limit" the profit for the wins so we have a resonable plot. we
# don't care as much about profitable trades here as non-profitable
# trades
# - first, get max loss
maxLossAbs <- abs(min(ts$Net.Trading.PL))
# - second, calculate a special PL for MAE graph that doesn't go past
# the max loss, for wins
ts$PL.MAE <- ifelse(ts$Net.Trading.PL > maxLossAbs, maxLossAbs, ts$Net.Trading.PL)

t(head(ts, n=3))

p1 <- ggplot(ts, aes(x=PL.MAE, y=MAE, color=isWin, shape=isWin)) +
      geom_point() +
      # this sets my colors to green/red
      scale_colour_manual(values=c("#FF9999", "#00CC00")) +
      geom_smooth(method=glm, se=FALSE)
p2 <- ggplot(ts, aes(x=Net.Trading.PL, y=MFE, color=isWin, shape=isWin)) +
      geom_point() +
      # this sets my colors to green/red
      scale_colour_manual(values=c("#FF9999", "#00CC00")) +
      geom_smooth(method=glm, se=FALSE)

multiplot(p1, p2)

# *three* graphs (including return distribution)

## p3 <- ggplot(pctReturn, aes(x=PL, fill=as.factor(positive))) +
##     geom_histogram(binwidth=.25, color="black") +
##         # this sets my colors to green/red
##         scale_fill_manual(values=c("#FF9999", "#90FF90"))

## png(filename = "ex_v1/mae_mfe_plot.ex1.png",
##     width = 1500, height = 300, units = "px", pointsize = 12,
##     bg = "white",
##     type = c("cairo", "cairo-png", "Xlib", "quartz"))
## print(multiplot(p3, p1, p2, cols=3))
## dev.off()


# some further calculation
# Merge in the atr from the mktdata, use "Start" as the name for the merge
ts.atr <- data.frame(Start=index(mktdata), coredata(mktdata$atr))
ts2 <- merge(ts, ts.atr, all.x = TRUE)
# some E-Ratio-like measure of the system in backtest, not JUST entry
# edge, but total result using MFE/MAE
eResult <- (sum(ts2$MFE / ts2$atr) / nrow(ts2)) / (sum(abs(ts2$MAE) / ts2$atr) / nrow(ts2))
