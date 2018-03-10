# Example strategy using:
# - crossing simple moving averages as a buy signal
# - ATR trailing stop
# - crossing back as a sell signal

input maxRisk = 3000;

#############################################
# technical indicators used in the strategy #
#############################################
def sma10 = Average(close, 10);
def sma20 = SimpleMovingAvg(close, 20, 0, no).SMA;
def atr = ATR();

#######################################
# Variables - several subtleties here #
#######################################

# is it a buy signal?
def buySignal = sma10 crosses above sma20;

# how much did we buy for?
def buyAmt = if(buySignal, close, buyAmt[1]);

# 2*ATR less than what we bought for - used in trailing stop
def x = buyAmt - (2 * atr);
#def x = max(buyAmt - .1, close - atr);
# this is initially a tight stop, but seems to work well over the long run.
# when we're wrong, we're wrong quickly, but longer trades let the profits ride
#def x = buyAmt - .1;

# Recursive calculation of stopLoss using a conditional. We
# only want the trailing stop to RISE
def stopLoss;
if x > stopLoss[1] or buySignal {
    stopLoss = x;
} else {
    stopLoss = if (stopLoss[1] == 0, close, stopLoss[1]);
}
# This does not work (can't reference stopLoss[1] in the first arg like this)
#def stopLoss = if(x > stopLoss[1], x, stopLoss[1]);

# Should we close the trade and take profits? (trend is reversing)
def takeProfits = sma10 crosses below sma20;
# Are we stopped out?
def stoppedOut = close < stopLoss;
# Sell for one of the two above reasons?
def sellSignal = takeProfits or stoppedOut;

# Are we in a trade? Same type of recursive definition as using in stopLoss
def inTrade;
if buySignal {
    inTrade = yes;
} else if sellSignal[1] {
    # use offsets here so we see the stop-loss when then sell is placed
    inTrade = no;
} else {
    inTrade = inTrade[1];
}

###################
# Position sizing #
###################
def R1 = maxRisk; # max dollars we want to risk
# shares held (or num to buy)
def shares = if(buySignal, floor(R1 / stopLoss), shares[1]);

#########
# Plots #
#########
# See the moving averages
plot SMAx10 = sma10;
plot SMAx20 = sma20;

# our stopLoss if we're in a trade, (NaN doesn't plot a value)
plot stopLossAmt = if (inTrade, stopLoss, Double.NaN);

####################
# Order Indicators #
####################
def isLoss = close < buyAmt;
addOrder(OrderType.SELL_TO_CLOSE, sellSignal and isLoss, open[-1], shares, Color.LIGHT_RED, Color.LIGHT_RED, "-");
addOrder(OrderType.SELL_TO_CLOSE, sellSignal and ! isLoss, open[-1], shares, Color.LIME, Color.LIME, "-");
addOrder(OrderType.BUY_TO_OPEN, buySignal, open[-1], shares, Color.CYAN, Color.CYAN, "");

