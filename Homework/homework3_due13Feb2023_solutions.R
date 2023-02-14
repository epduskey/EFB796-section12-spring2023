# Homework 3 - Population Growth

# Created: 6 Feb 2023
# Last modified: 6 Feb 2023 by EPD

# Set working directory
setwd("/Users/epdus/OneDrive/Breathless/Teaching/EFB796")

# Contents (ctrl-f):
# I. Basic surplus production
# II. Equilibrium harvest
# III. Maximum sustainable yield


########## I. Basic surplus production ##########

# Choose an initial biomass value (prior to any harvest)
B0 = 1000

# Choose an instantaneous growth rate
r = 0.1

# Choose a carrying capacity
K = 1000

# Choose a harvest rate (we call this F = fishing mortality in fisheries)
Fmort = 0.05

# Choose the number of time units (years/months/days) through which to project
times = seq(0, 200)

# Create a vector to house both biomass and harvest
Bt = B0
Ht = vector(mode = "numeric")

# Calculate the biomass at time step 2
Bt[2] = Bt[1] + r*Bt[1]*(1 - (Bt[1]/K)) - Fmort*Bt[1]

# Calculate harvest at time step 2
Ht[1] = Fmort*Bt[1]

# Write a for loop to calculate biomass and harvest from time steps 3 onwards
for(i in 3:length(times)) {
  Bt[i] = Bt[i-1] + r*Bt[i-1]*(1 - (Bt[i-1]/K)) - Fmort*Bt[i-1]
  Ht[i-1] = Fmort*Bt[i-1]
}

# Plot biomass (if it doesn't reach equilibrium i.e. flatten out then make "times" longer)
plot(Bt, xlab = "Time", ylab = "Biomass")

# Plot harvest (if it doesn't reach equilibrium i.e. flatten out then make "times" longer)
plot(Ht, xlab = "Time", ylab = "Harvest")


########## II. Equilibrium harvest ##########

# Extract the last value of your harvest time series
Ht[length(times)-1]

# Create a sequence of Fmort values to try
Fseq = seq(0,0.2,length.out=100)

# Write a function to calculate equilibrium harvest 
# i.e. calculate harvest for a time series and extract the last value.
# Hint: Fmort is what is varying, so that must be one of your arguments.
eqHarvest = function(B0, r, K, tsteps, Fmort) {
  
  # Set initial biomass
  Bt = B0
  
  # Set initial harvest
  Ht = vector(mode = "numeric")
  
  # Set up time steps vector
  times = seq(tsteps)
  
  # Use a for loop to calculate biomass and harvest
  for(i in 1:length(times)) {
    Bt[i+1] = Bt[i] + r*Bt[i]*(1 - (Bt[i]/K)) - Fmort*Bt[i]
    Ht[i] = Fmort*Bt[i]
  }
  
  # Return the equilibrium harvest
  return(Ht[length(Ht)])
}

# Use your function to calculate equilibrium harvest at just the first value of "Fseq" (should be zero!)
eqHarvest(B0, r, K, 100, Fseq[1])

# Apply your function to each value in "Fseq" to get equilibrium harvest
# Loop version
H_loop = vector()
for(i in 1:length(Fseq)) H_loop[i] = eqHarvest(B0,r,K,100,Fseq[i])
# sapply version
H_Fseq = sapply(Fseq, eqHarvest, B0 = B0, r = r, K = K, tsteps = 100)
# You can check if they're identical!
identical(H_loop, H_Fseq)


########## III. Maximum sustainable yield ##########

# Plot equilibrium harvest as a function of harvest rate/fishing mortality
plot(H_Fseq ~ Fseq, xlab = "F", ylab = "Harvest")

# What is the maximum?
msy = max(H_Fseq)
msy # You've just calculated maximum sustainable yield (MSY)!

# What is the harvest rate/fishing mortality at MSY?
Fmsy = Fseq[which.max(H_Fseq)]
Fmsy # Now you've got Fmsy!
