# Homework 4 - due 20 Feb 2023

# Set working directory (if not using R Project)
setwd("/Users/epdus/OneDrive/Breathless/Teaching/EFB796")

# Contents (ctrl-f):
# I. Load data
# II. Plot data
# III. Calculate HPUE
# IV. Estimate population size


########## I. Load data ##########

# Load in the white-tailed deer data
deer = read.csv("Data/wtdeer.csv")

# Use whatever functions you find appropriate to examine your data
head(deer)
str(deer)
summary(deer)
hist(deer$Harvest_Males)
hist(deer$Harvest_Females)

# Is there anything that jumps out at you about this data set?
#
#


########## II. Plot data ##########

# Plot male deer harvest over time
plot(Harvest_Males ~ Year, deer)

# Add a super smooth line (remember supsmu!)
lines(supsmu(x = deer$Year, y = deer$Harvest_Males))

# Plot female deer harvest over time
plot(Harvest_Females ~ Year, deer)

# Plot both together on one plot space
par(mfrow = c(2,1))
plot(Harvest_Males ~ Year, deer)
plot(Harvest_Females ~ Year, deer)
par(mfrow = c(1,1))

# Plot harvest as a function of effort
plot(Harvest_Males ~ Effort_Males, deer)
plot(Harvest_Females ~ Effort_Females, deer)


########## III. Calculate HPUE ##########

# Add male harvest per unit effort HPUE to the data frame
deer$HPUE_Males = deer$Harvest_Males / deer$Effort_Males

# Add female HPUE to the data frame
deer$HPUE_Females = deer$Harvest_Females / deer$Effort_Females

# Plot both male and female HPUE over time
par(mfrow = c(1,2))
plot(HPUE_Males ~ Year, deer)
plot(HPUE_Females ~ Year, deer)
par(mfrow = c(1,1))

# Plot harvest as a function of effort for both sexes on one graph
plot(Harvest_Females ~ Effort_Females, deer, 
     xlab = "Effort", ylab = "Harvest",
     pch = 16, col = "darkgreen")
points(deer$Effort_Males, deer$Harvest_Males, pch = 16, col = "darkred")


########## IV. Estimate population size ##########

# Assume constant q (vulnerability/catchability) and choose reasonable value below
# (Try a few different values and see how your estimates change)
q = 0.0004

# Estimate population size 
deer$Nmales = deer$HPUE_Males / q
deer$Nfemales = deer$HPUE_Females / q

# Plot these estimates with time
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