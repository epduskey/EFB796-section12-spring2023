# Homework 4 - due 20 Feb 2023

# Set working directory (if not using R Project)


# Contents (ctrl-f):
# I. Load data
# II. Plot data
# III. Calculate HPUE
# IV. Estimate population size


########## I. Load data ##########

# Load in the white-tailed deer data
deer = read.csv("Data/wtdeer.csv")

# Use whatever functions you find appropriate to examine your data


# Is there anything that jumps out at you about this data set?
#
#


########## II. Plot data ##########

# Plot male deer harvest over time
plot(Harvest_Males ~ Year, deer)

# Add a super smooth line (remember supsmu!)


# Plot female deer harvest over time, but play with the plotting options!
plot(Harvest_Females ~ Year, deer)

# Plot both males and females together on one plot space


# Plot harvest as a function of effort



########## III. Calculate HPUE ##########

# Add male harvest per unit effort HPUE to the data frame
deer$HPUE_Males = 

# Add female HPUE to the data frame
deer$HPUE_Females = 

# Plot both male and female HPUE over time


# Plot harvest as a function of effort for both sexes



########## IV. Estimate population size ##########

# Assume constant q (vulnerability/catchability) and choose a reasonable value below
# (Try a few different values and see how your estimates change)
q = 0.00002

# Estimate population size (recall that population size can be estimated by HPUE/q)


# Plot these estimates with time, modifying the functions below to make more interesting plots!
plot(Nmales ~ Year, deer)
plot(Nfemales ~ Year, deer)
# Take a look at the population estimates in the Fryxell paper (Fig. 1) and compare.
# Why do you think your estimates look different from theirs?
#
#

# We hope HPUE will not change with effort; check if it does for males and females
plot(HPUE_Males ~ Effort_Males, deer)
plot(HPUE_Females ~ Effort_Females, deer)

# What would you recommend be done with these data, absent additional information?
# 
#