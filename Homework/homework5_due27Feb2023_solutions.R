# Homework 5 - due 27 Feb 2023

# Set working directory (if not using R Project)
setwd("/Users/epdus/OneDrive/Breathless/Teaching/EFB796")

# Install and load packages
#install.packages("rje")
library(rje)
#install.packages("unmarked")
library(unmarked)

# Contents (ctrl-f):
# I. Choose baseline parameters
# II. Simulate cover of preferred grass species
# III. Simulate fox dens
# IV. Simulate forest cover data
# V. Simulate detection data as a function of forest cover
# VI. Estimate number of dens

# Imagine a scenario in which you are using aerial photographs to count
# visible fox dens in an oak savanna habitat.  Your hypothesis
# is that there is a higher density of fox dens in habitats with
# a greater coverage of a certain species of grass.  However, this
# is complicated by the fact that the tree cover can make some dens 
# difficult to see.  To quantify these effects, you want to run
# a model which accounts both for the difficulty of detecting
# fox dens in relatively dense tree cover, and for the effect of
# the presence of grass on the abundance of fox dens.  To do so,
# you can run a site occupancy model using the famed "unmarked"
# package.  The code below walks you through the data generating
# and the estimation processes.  Have fun!


########## I. Choose baseline parameters ##########

# Number of observations
n = 100

# Number of observers (undergraduates roped into looking at your aerial photographs)
nobs = 3


########## II. Simulate cover of preferred grass species ##########

# Use a uniform distribution
grass_cover = runif(n = n, min = 0, max = 100)

# Set up a data frame to store data
foxdens = data.frame(grass = grass_cover)


########## III. Simulate fox dens ##########

# Number of fox dens is an increasing function of the
# cover of their preferred grass species

# Choose slope
m_grass = 0.05

# Choose intercept
b_grass = -2

# Calculate probabilities on the standard scale
dens = m_grass * grass_cover + b_grass

# Convert this to the exponential scale
dens_mean = exp(dens)

# Take a look at your mean values
hist(dens_mean)

# Simulate fox den number with poisson distribution
foxdens$dens = rpois(n = n, lambda = dens_mean)

# Take a look at your fox den numbers
hist(foxdens$dens)


########## IV. Simulate forest cover data ##########

# Use a beta distribution to simulate forest cover: 
# https://en.wikipedia.org/wiki/Beta_distribution
#
# This distribution produces values between 0 and 1
# according to two shape parameters alpha and beta.
# I've chosen values below, but please try some
# different ones based on the figure on the Wiki page!

# Choose alpha
alpha = 2

# Choose beta
beta = 5

# Choose number of values
n = 100

# Simulate forest cover
forest_cover = rbeta(n = n, shape1 = alpha, shape2 = beta)

# Take a look at the values
hist(forest_cover)

# Store forest cover in the data frame
foxdens$forest = forest_cover


########## V. Simulate detection data as a function of forest cover ##########

# Forest cover makes the dens hard to see!  You've got
# nobs people helping you look at your photographs
# and counting dens.  This repeated sampling will help
# you get an idea of how forest cover influences your
# ability to see (detect) the dens

# Choose slope
m_detect = 10

# Choose intercept
b_detect = 10

# Calculate probability of detecting dens on the logit scale
detect = m_detect * forest_cover + b_detect

# Convert detection to the expit scale
dprob = expit(detect)

# Take a look at the detection probability values
hist(dprob)

# All the probabilities are 1!  My slope and intercept values
# are crap.  Use desmos to help you pick some better values:
# https://www.desmos.com/calculator/yiqcdzb8hc
#
# Remember that the x-axis represents forest cover, and so
# values of y at x in [0,1] are the only ones that matter
# for y_expit

# Choose your new values
m_detect = 1.5
b_detect = 0.5

# Calculate probability of detecting dens on the logit scale
detect = m_detect * forest_cover + b_detect

# Convert detection to the expit scale
dprob = expit(detect)

# Now let your helpers count!
observers = matrix(rbinom(n = n*nobs, size = foxdens$dens, prob = dprob), ncol = nobs)

# Name your matrix columns
colnames(observers) = paste("obs", seq(nobs), sep = "")

# Append the observer data to your data frame
foxdens = cbind(foxdens, observers)

# Make sure it looks alright
head(foxdens)


########## VI. Estimate number of dens ##########

# Store your observer counts and covariate (i.e. grass) data 
# in a special type of object for the unmarked package
dens_data = unmarkedFramePCount(y = observers,
                                siteCovs = foxdens[,c("forest","grass")])

# Take a look at the object
str(dens_data)
summary(dens_data)

# Time to build your model!  There are two formulas; the first
# one is for detection as a function of some covariates, the
# second one is for abundance as a function of some covariates.
# In our case, detection is affected by pesky trees, and
# the number of dens is affected by the cover of a preferred
# grass species.  The value of K is a control variable for
# the fitting process, don't worry too much about it now.
dens.occ = pcount(formula = ~ forest 
                            ~ grass,
                  data = dens_data,
                  K = 50)

# Look at the summary output
summary(dens.occ)

# Compare the coefficients of detection to your chosen values
# Does this model seem appropriate for the data?
#
#
coef(dens.occ)
c(b_grass, m_grass, b_detect, m_detect)

# Use your model to predict abundance
# Try typing unmarked::predict into the console
# to get a better idea as to how the predict function
# works for this package.  How would you extract
# the predicted detection probabilities?
#
#
N.preds = predict(dens.occ, type = "state")

# Plot to compare to your simulated values
plot(N.preds$Predicted, foxdens$dens)
abline(0,1)

# Now you know how to do basic site occupancy models!