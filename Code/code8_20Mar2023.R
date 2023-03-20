# Lecture 8 - Cohort Dynamics

# Created: 20 Mar 2023
# Last modified: 20 Mar 2023 by EPD

# Install and load packages
library(ggplot2)
library(investr)
library(rje)

# Set working directory
setwd("/Users/epdus/OneDrive/Breathless/Teaching/EFB796")

# Contents (ctrl-f):
# I. Bison data
# II. Branching process
# III. Leslie matrix model
# IV. VPA


########## I. Bison data ##########

# Read in the data (Figure 6.3b in Gates et al 2010)
bison = read.table("Data/bison.txt", header = T)
bison

# Plot the data
p = ggplot(bison, aes(x=Year,y=N)) +
      geom_point(color = "dodgerblue", size = 5) +
      theme_bw()
p

# Classic nonlinear estimation
bison.nls = nls(N ~ N0*exp(r*(Year-1901)), data = bison, start = list(r = 0.2, N0 = 1))
summary(bison.nls)

# Get predictions
preds = predFit(bison.nls, se.fit = T, interval = "confidence")

# Create prediction data frame
bison.preds = data.frame(Year = bison$Year)
bison.preds$N = preds$fit[,"fit"]
bison.preds$N_lo = preds$fit[,"lwr"]
bison.preds$N_hi = preds$fit[,"upr"]

# Add line to ggplot
p = p + geom_line(data = bison.preds, aes(x=Year,y=N), color = "darkblue", linewidth = 1) +
        geom_ribbon(data = bison.preds, aes(ymin=N_lo,ymax=N_hi), fill = adjustcolor("darkblue",alpha.f=0.2), color = NA)
p


########## II. Branching process ##########

# mode function
Mode = function(x) {
  ux = unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Branching function
# par: vector containing survival probability and probability of producing each possible number of offspring
# offspring: possible numbers of offspring corresponding to all but first element of par
# nreps: number of repetitions for simulations to get median values
# N: vector containing observed number of individuals in population at each time step
# err: returns estimates rather than error if FALSE
# returns SSE between N and estimated number of individuals at each time step
branch = function(par, offspring, nreps, N, err = T) {
  
  # Survival probability is first element of par
  s = expit(par[1])
  
  # All other elements are probabilities of produces 0 - length(par)-1 offspring
  probs = expit(par[-1])
  
  # Initialize vector for number at each generation
  Z = vector(mode = "numeric", length = length(N))
  
  # Name the elements
  names(Z) = paste("Z[", seq(0,length(N)-1), "]", sep = "")
  
  # First element is initial starting value
  Z[1] = N[1]
  
  # Branching loop
  for(i in 2:length(Z)) {
    
    # Check if Z[i-1] individuals survived, on average
    N_s = Mode(rbinom(n = nreps, size = Z[i-1], prob = s))
    
    # Get median number of offspring from each survivor
    N_bbs = sum(apply(rmultinom(n = nreps, size = N_s, prob = probs),1,Mode) * offspring)
    
    # Number at current time step is survivors + offspring
    Z[i] = N_s + N_bbs
    
  }
  
  # Calculate sum of squared error
  Z_err = sum((N - Z)^2)
  
  # Check if user wants estimates; return error if not
  if(err) {
    return(Z_err)
  } else{
    return(Z)
  }
}

# Choose all possible offspring numbers
offspring = c(0,1,2,3)

# Try branch function for chosen values
branch(par = logit(c(0.6, 0.3, 0.5, 0.1, 0.1)), offspring = offspring, nreps = 1000, N = bison$N)

# Use optim to estimate survival and offspring distribution
bison.optim = optim(par = logit(c(0.4, 0.4, 0.4, 0.1, 0.1)), 
                    fn = branch, 
                    hessian = T, 
                    offspring = offspring, 
                    nreps = 10000,
                    N = bison$N,
                    method = "BFGS",
                    control = list(maxit = 500))
bison.optim

# Run function with estimated values
optim.preds = branch(par = bison.optim$par, offspring = offspring, nreps = 10000, N = bison$N, err = F)

# Add branching predictions
bison.preds$branch = optim.preds

# Add line for branching process
p = p + geom_line(data = bison.preds, aes(x=Year,y=branch), color = "darkred")
p


########## III. Leslie matrix model ##########

# An organism that lives for a maximum of three years
f1 = 0
f2 = 1
f3 = 2

# Survival
S0 = 0.6
S1 = 0.8
S2 = 0.8

# Leslie matrix
M = matrix(c(S0*f1,  S0*f2,  S0*f3,
             S1,     0,      0,
             0,      S2,     0),
           nrow = 3, ncol = 3, byrow = T)

# Number of years to project
nyears = 30

# Cohort matrix
N = matrix(0, nrow = nyears, ncol = 3)

# Starting values
N[1,] = c(100,30,50)

# Projection loop
times = 1:nyears
for(i in times[-nyears]){
  N[i+1,]  =  M %*% N[i,]
}

# Use matplot to show each age
matplot(times, N, type = "l", lwd = 4)
legend("topleft", c("a0","a1","a2"), col = seq(3), lty = seq(3), lwd = 4)

# Create data frame from results matrix
leslie.df = as.data.frame(c(N)); colnames(leslie.df) = "N"
leslie.df$year = seq(nyears) - 1
leslie.df$age = factor(rep(c(0,1,2), each = nyears))

# Use ggplot to show each age
m = ggplot(leslie.df, aes(x=year,y=N,color=age)) +
      geom_line(linewidth = 2) +
      theme_linedraw()
m

# Use spectral analysis to find eigenvalues and eigenvectors
eigen(M)

# Look at rough estimate of rate of change
mean(N[21:30,] / N[20:29,])


########## IV. VPA ##########

# Data from walleye pollock (Quinn and Deriso Table 8.13)
ages = seq(2,10)
catch = c(1073816, 1097360, 682467, 228463, 74749, 21178, 13306, 25241)
M_mort = c(0.45, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3)

# Vector to hold estimates
N_vpa = vector(mode = "numeric", length = length(ages))
names(N_vpa) = ages
H_vpa = N_vpa

# Final harvest mortality
FA = 0.2

# First step with John Gulland's method
ZA = FA + last(M_mort)							
N_vpa[length(N_vpa)] = last(catch) / ((FA/ZA)*(1-exp(-ZA)))
H_vpa[length(H_vpa)] = FA

# Function to try different harvest mortalities
ftry = function(par,M,catch,N) {
  (catch/N - (par/(par+M))*(exp(par+M)-1))^2
}

# Loop through John Gulland's method
for(i in (length(N_vpa)-1):1) {
  
  # Estimate harvest mortality
  H_vpa[i] = optim(par = 0.2, 
                  fn = ftry, 
                  M = M_mort[i], catch = catch[i], N = N_vpa[i+1], 
                  method = "Brent", lower = 0, upper = 1)$par
  
  # Estimate number in current year
  Z_temp = H_vpa[i] + M_mort[i]
  N_vpa[i] = catch[i] / ((H_vpa[i]/(Z_temp))*(1-exp(-Z_temp)))
}

# Estimates data frame
vpa.df = data.frame(year = 1975:1983, N = N_vpa/100000, H = H_vpa)

# Plot estimates on dual y-axis
ggplot(vpa.df, aes(x=year)) +
  geom_line(aes(y=N), linewidth = 1, color = "blue") +
  geom_line(aes(y=H*1e2), linewidth = 1, color = "red") +
  scale_y_continuous(name = "N", sec.axis = sec_axis(~.*1e-2,name="H")) +
  theme_minimal()