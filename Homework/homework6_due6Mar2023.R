# Homework 6 - due 6 Mar 2023

# Set working directory (if not using R Project)
setwd("/Users/epdus/OneDrive/Breathless/Teaching/EFB796")

# Install and load packages
#install.packages("FSAdata")
library(FSAdata)
#install.packages("investr")
library(investr)

# Contents (ctrl-f):
# I. Load data


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


# Do you expect a stock-recruitment function to fit well to these data?
#
#


########## II. Density-independence ##########

# Fit a density-independent model to this stock-recruit relationship
# Hint: you could use the lm function but...(why wouldn't you?)
#


# Look at a summary output of your model


# Now fit using the Deriso model with NLS.  Here's a desmos plot to help you choose starting values
# https://www.desmos.com/calculator/5i4yagdqs8


# Look at the output of your NLS model


# Make predictions for both models and include the confidence intervals


# Add a prediction line with confidence intervals to your graph for the density-independent model


# Repeat for the density-dependent Deriso model


# Compare the fits of each model; which one is better, and why do you think that is?
#
#


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


# Add the prediction line and CI of whichever model is better


# Now do jackknife estimation of the Deriso model
# Add the lines on top of the current CI
# Do jackknife estimation for the Beverton-Holt model
# Hint: Use the parameter estimates from the Deriso NLS model above as starting values


# Uh-oh.  Bet you're getting some errors.
# (If you're not getting errors, ignore this part)
# Copy/paste the error (after the colon) into the search engine of your choice,
# and see if you can understand what's happening.  Try and explain what you
# think is going on here, and how you might fix it.  If you're feeling adventurous,
# do try and fix it!  Otherwise, we will discuss it in class
#
#