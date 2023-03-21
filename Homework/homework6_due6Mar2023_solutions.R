# Homework 6 - due 6 Mar 2023

# Set working directory (if not using R Project)
setwd("/Users/epdus/OneDrive/Breathless/Teaching/EFB796")

# Install and load packages
#install.packages("FSA")
library(FSAdata)
#install.packages("investr")
library(investr)

# Contents (ctrl-f):
# I. Load data
# II. Density-independence
# III. Jackknife


########## I. Load data ##########

# Load data from the FSA package
data(WalleyeEL)

# Take a look at the data
head(WalleyeEL)

# Which variable should be the stock and which should be the recruits?  Explain your answer:
#
#

# Refresh your plot settings
dev.off()

# Plot the recruits vs. the stock here; try to make it as pretty or as weird as you like!
plot(age0 ~ age5, data = WalleyeEL, 
     xlim = c(0,3000), ylim = c(0,36000),
     pch = 24, cex = 2, lwd = 3, bg = "magenta", 
     cex.axis = 1.2, cex.lab = 1.2, font = 5, font.lab = 5)

# Do you expect a stock-recruitment function to fit well to these data?
#
#


########## II. Density-independence ##########

# Fit a density-independent model to this stock-recruit relationship
# Hint: you could use the lm function but...(why wouldn't you?)
#
fit.lm = nls(age0 ~ a*age5, 
             data = WalleyeEL,
             start = c(a=3))

# Look at a summary output of your model
summary(fit.lm)

# Now fit using the Deriso model with NLS.  Here's a desmos plot to help you choose starting values
# https://www.desmos.com/calculator/5i4yagdqs8
fit.ds = nls(age0 ~ a*age5*(1-b*c*age5)^(1/c), 
             data = WalleyeEL,
             start = c(a=10,b=1e-4,c=-1))

# Look at the output of your NLS model
summary(fit.ds)

# Make predictions for both models and include the confidence intervals
x.new = seq(0,3000)
lm.preds = predFit(fit.lm, newdata = list(age5=x.new), interval = "confidence")
ds.preds = predFit(fit.ds, newdata = list(age5=x.new), interval = "confidence")

# Add a prediction line with confidence intervals to your graph for the density-independent model
lines(x.new, lm.preds[,1], lwd = 3, col = "darkgreen")
polygon(x = c(x.new,rev(x.new)),
        y = c(lm.preds[,2],rev(lm.preds[,3])),
        border = NA, col = adjustcolor("darkgreen",alpha.f=0.2))

# Repeat for the density-dependent Deriso model
lines(x.new, ds.preds[,1], lwd = 3, col = "darkred")
polygon(x = c(x.new,rev(x.new)),
        y = c(ds.preds[,2],rev(ds.preds[,3])),
        border = NA, col = adjustcolor("darkred",alpha.f=0.2))

# Compare the fits of each model; which one is better, and why do you think that is?
#
#
anova(fit.lm, fit.ds)

# You can also calculate the standard error of regression (lower numbers are better)
# Here, replace my model names with your own, assuming they're different
# Which model is better, according to this statistics?
#
#
( lm.se = sum(resid(fit.lm)^2) / sqrt(length(resid(fit.lm))) )
( ds.se = sum(resid(fit.ds)^2) / sqrt(length(resid(fit.ds))) )


########## III. Recruitment at mean stock size ##########

# Estimate recruitment at mean stock size, with confidence intervals
# Hint: first, calculate the mean stock size
mean_ss = mean(WalleyeEL$age5)
predFit(fit.lm, newdata = list(age5=mean_ss), interval = "confidence")
predFit(fit.ds, newdata = list(age5=mean_ss), interval = "confidence")

# How different are the results among the two models?
#
#


########## III. Jackknife ##########

# First, why do you think it's called jackknife estimation?  Could you think of a better name?
# (Yes, this is largely a silly question)
#
#

# First, re-plot your data so there are no lines/CI's 
# It's fine to copy/paste from the plot above
plot(age0 ~ age5, data = WalleyeEL, 
     xlim = c(0,3000), ylim = c(0,36000),
     pch = 24, cex = 2, lwd = 3, bg = "magenta", 
     cex.axis = 1.2, cex.lab = 1.2, font = 5, font.lab = 5)

# Add the prediction line and CI of whichever model is better
lines(x.new, ds.preds[,1], lwd = 3, col = "darkred")
polygon(x = c(x.new,rev(x.new)),
        y = c(ds.preds[,2],rev(ds.preds[,3])),
        border = NA, col = adjustcolor("darkred",alpha.f=0.2))

# Now do jackknife estimation of the Deriso model
# Add the lines on top of the current CI
# Do jackknife estimation for the Beverton-Holt model
# Hint: Use the parameter estimates from the Deriso NLS model above as starting values
for(i in 1:nrow(WalleyeEL)) {
  
  # Remove one data point
  walleye.jk = WalleyeEL[-i,]
  
  # Run the model on the new data set
  walleye.nls = nls(age0 ~ a*age5*(1-b*c*age5)^(1/c), 
                    data = walleye.jk,
                    start = coef(fit.ds))
  
  # Prediction Escape values
  x.new = list(age5 = seq(0,3000))
  
  # Add a transparent line to the plot
  lines(x.new$age5, predict(walleye.nls,newdata=x.new),
        lwd = 1.2, col = adjustcolor("black",0.2))
}

# Uh-oh.  Bet you're getting some errors.
# (If you're not getting errors, ignore this part)
# Copy/paste the error (after the colon) into the search engine of your choice,
# and see if you can understand what's happening.  Try and explain what you
# think is going on here, and how you might fix it.  If you're feeling adventurous,
# do try and fix it!  Otherwise, we will discuss it in class
#
#