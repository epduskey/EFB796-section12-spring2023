# Lecture 3 - Population Growth

# Created: 5 Feb 2023
# Last modified: 6 Feb 2023 by EPD

# Set working directory
setwd("/Users/epdus/OneDrive/Breathless/Teaching/EFB796")

# Contents (ctrl-f):
# I. Geometric growth
# II. Exponential growth
# III. Logistic growth
# IV. Tuna data


########## I. Geometric growth ##########

# Starting value
N = 1

# See that N is a vector
is.vector(N)
is.matrix(N)
is.data.frame(N)
is.list(N)
is.array(N)

# Years
Year=seq(20)

# Geometric growth loop
for(i in 1:19) {
  N[i+1] = N[i]*2
}

# Geometric growth function
# N0: starting value
# R: geometric growth rate
# t: time step(s)
# returns vector containing population value N at time(s) t
geogrowth = function(N0, R, t) {
  N0 * R^t
}

# Apply custom function for one time step
geogrowth(5, 1.2, 15)

# Apply custom function for multiple time steps
geogrowth(5, 1.2, c(5,10,15,20))

# Apply custom function for multiple growth rate values
geogrowth(5, seq(0.8,1.8,0.2), 12)

# Get instantaneous rate of change
diff(log(N))

# See equality of instantaneous and geometric rates of change
log(2)

# Use barplot to see discrete growth
N = geogrowth(5, 1.02, seq(50))
barplot(N, xlab = "Year", ylab = "N")
axis(side = 1)
box()
title("Geometric Growth")


########## II. Exponential growth ##########

# Starting value
N0 = 20000

# Years
Year = 1:20

# Set up empty plot
plot(c(1,20), c(0,50000), xlab = "Year (t)", ylab = "N(t)", type = "n")

# Growth
N = N0 * exp(0.05*Year)
lines(Year, N, lwd = 4, col = 2)

# Equilibrium
N = N0 * exp(0.0*Year)
lines(Year, N, lwd = 4, col = 1)

# Decline
N = N0 * exp(-0.05*Year)
lines(Year, N, lwd = 4, col = 3)

# Exponential growth function
# N0: starting value
# r: instantaneous growth rate
# t: time step(s)
# returns vector containing population value N at time(s) t
expgrowth = function(N0, r, t) {
  N0 * exp(r*t)
}

# Plot exponential growth
times = seq(0,100,0.1)
N_exp = expgrowth(20000, 0.4, times)
plot(times, N_exp)
plot(times, N_exp, type = "l", lwd = 4)
plot(times, N_exp, type = "l", lwd = 4, log = "y")


########## III. Logistic growth ##########

# Starting value
N0 = 20000

# Years
Year = 1:20

# Set up empty plot
plot(c(1,20), c(0,60000), xlab = "Time", ylab = "N(t)", type = 'n')

# Exponential growth
r = 0.05
N = N0 * exp(r*Year)
lines(Year, N, lwd = 4, col = 1)

# Logistic growth
K = 40000
Nlogistic = N0*exp(r*Year) / (1 - N0/K + N0*exp(r*Year)/K)
lines(Year, Nlogistic, lwd = 4, col = 6)

# Plot long logistic time series
N0 = 10
K = 1000
r = 0.1
times = seq(0,100)
N_ts = N0*exp(r*times) / (1 - N0/K + N0*exp(r*times)/K)
plot(times, N_ts, type = "l", lwd = 4, col = 6)
abline(a = K, b = 0, lwd = 4)
plot(diff(N_ts))


########## IV. Tuna data ##########

# Load the yellowfin data
tuna = read.csv("Data/yellowfin.csv")

# Add CPUE column
tuna$cpue = tuna$catch / tuna$effort

# Look at data structure
str(tuna)

# Explore catch values
hist(tuna$catch)

# Explore log of catch
hist(log(tuna$catch))

# Are these data log normal?
qqnorm(log(tuna$catch))
qqline(log(tuna$catch))

# How about CPUE?
hist(tuna$cpue)
hist(log(tuna$cpue))
qqnorm(log(tuna$cpue))
qqline(log(tuna$cpue))

# Plot catch relative to CPUE
plot(cpue ~ catch, data = tuna)

# Run a linear model to see if CPUE is negatively related to catch
cpue.lm = lm(cpue ~ catch, data = tuna)
summary(cpue.lm)
abline(cpue.lm$coefficients, lwd = 3, col = "red")

# Plot data again
plot(cpue ~ catch, data = tuna, pch = 17, cex = 1.5)

# Calculate predictions with confidence intervals
catch.preds = seq(30000,250000,100)
lm.preds = predict(cpue.lm, newdata = list(catch = catch.preds), se.fit = T)
str(lm.preds)
upper.ci = lm.preds$fit + 1.96*lm.preds$se.fit
lower.ci = lm.preds$fit - 1.96*lm.preds$se.fit

# Add regression line and confidence interval shaded area
lines(catch.preds, lm.preds$fit, lwd = 3)
polygon(x = c(catch.preds,rev(catch.preds)),
        y = c(upper.ci,rev(lower.ci)),
        border = NA, col = rgb(0, 0, 0, 0.3))

# Look at correlation of catch and CPUE
ccf(x = tuna$catch, y = tuna$cpue, type="correlation", plot = T)