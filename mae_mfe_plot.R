# MAE / MFE scatter plot ala:
# http://www.tradingheroes.com/mfe-and-mae-deconstructed-and-how-they-can-help-your-trading/
# NOTE: need to eval the multiplot() function at the bottom before drawing the plot

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

# some further calculation
# Merge in the atr from the mktdata, use "Start" as the name for the merge
ts.atr <- data.frame(Start=index(mktdata), coredata(mktdata$atr))
ts2 <- merge(ts, ts.atr, all.x = TRUE)
# some E-Ratio-like measure of the system in backtest, not JUST entry
# edge, but total result using MFE/MAE
eResult <- (sum(ts2$MFE / ts2$atr) / nrow(ts2)) / (sum(abs(ts2$MAE) / ts2$atr) / nrow(ts2))

#################
# From: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
