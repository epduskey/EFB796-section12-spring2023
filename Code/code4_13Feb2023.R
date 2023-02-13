# Lecture 4 - Theory of Harvest

# Created: 10 Feb 2023
# Last modified: 13 Feb 2023 by EPD

# Set working directory
setwd("/Users/epdus/OneDrive/Breathless/Teaching/EFB796")

# Contents (ctrl-f):
# I. Exponential decline
# II. American martens
# III. Constant vulnerability
# IV. Changing vulnerability
# V. Percent decline


########## I. Exponential decline ##########

# Create up abundance vector
N = rep(0,10)

# Choose starting value (i.e. N0)
N[1] = 100

# Create times vector
Year = seq(length(N))

# Calculate abundance in each successive time period
for(i in 1:9) N[i+1] = N[i]*exp(-0.2)

# Plot decline
plot(Year, N, cex = 2, ylim = c(0,100))
lines(Year, N, lwd = 3)

# In barplot format
xbarplot = barplot(height = cbind(Day1=c(80,20), Day2=c(64,16), Day3=c(51,13)), 
        col=c("blue","red"))
xbarplot
text(xbarplot[1], 90, "20")
text(xbarplot, c(90,72,57.5), c("20","16","13"))


########## II. American martens ##########

# Read in the data
marten = read.csv("Data/martens.csv")

# Look at your data
marten
head(marten)
str(marten)
summary(marten)

# Visualize your data
plot(Harvest ~ Season, marten, cex = 2, pch = 18, type = "b")
plot(TrapNights ~ Season, marten, cex = 2, pch = 18, type = "b")

# Multiple graphs
par(mfrow = c(1,2))
plot(Harvest ~ Season, marten, cex = 2, pch = 18, type = "b")
plot(TrapNights ~ Season, marten, cex = 2, pch = 18, type = "b")
par(mfrow = c(1,1))


########## III. Constant vulnerability ##########

# Assume constant vulnerability to the traps
q = 0.01

# Calculate harvest/effort index (harvest per thousand trap-nights)
marten$HPUE = 1000 * marten$Harvest / marten$TrapNights

# Plot the index
plot(HPUE ~ Season, marten, cex = 3, pch = 20, type = "b")

# Estimate mean population size
marten$Nbar = marten$HPUE / q

# Plot the resulting population time trend
plot(Nbar ~ Season, marten, cex = 3, pch = 22, bg = "darkred", type = "b")
title("American martens")


########## IV. Changing vulnerability ##########

# Look at HPUE as a function of effort
plot(HPUE ~ TrapNights, marten, cex = 2, pch = 13)

# Plot a super smooth trend line along the data
lines(supsmu(marten$TrapNights, marten$HPUE), lwd = 2, col = "cyan")

# Run a linear model to extract the slope of the decrease 
hpue.lm = lm(HPUE ~ TrapNights, data = marten)

# Look at a summary of the output
summary(hpue.lm)

# Use the time-varying vector of vulnerability values to estimate population
marten$Nbar_t = marten$HPUE / marten$q

# Compare the two estimates of population size
par(mfrow = c(1,2))
plot(Nbar ~ Season, marten, ylim = c(0,2000), main = "Constant q")
plot(Nbar_t ~ Season, marten, ylim = c(0,2000), main = "Changing q")
par(mfrow = c(1,1))


########## V. Percent decline ##########

# Run a linear regression model on population change with time
n.lm = lm(Nbar ~ Season, data = marten)

# Look at a summary of the output
summary(n.lm)

# Look at the predicted population size for each season
n.preds = predict(n.lm)

# Look at the percent decline during the entire time period
100 *(1 - n.preds[length(n.preds)] / n.preds[1])

# Repeat the steps above for time-varying vulnerability
nt.lm = lm(Nbar_t ~ Season, data = marten)
summary(nt.lm)
nt.preds = predict(nt.lm)
100 * (1 - nt.preds[length(nt.preds)] / nt.preds[1])
