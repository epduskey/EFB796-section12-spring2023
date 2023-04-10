# Lecture 11 - Movement Models

# Created: 9 Apr 2023
# Last modified: 9 Apr 2023 by EPD

# Load packages
library(ggplot2)
library(rje)

# Contents (ctrl-f):
# I. Leslie matrix parameters
# II. Leslie matrices
# III. Simulate time series
# IV. Plot results
# V. Eigenvalue analysis


########## I. Leslie matrix parameters ##########

# Starting values in each of three ages
N0 = rep(c(100, 80, 72), times = 2)

# Fecundity for each age
f1 = 0
f2 = 0
f3 = 3

# Survival for each age
S0 = 0.47
S1 = 0.8
S2 = 0.9

# Movement probability
theta_1 = matrix(c(0.7, 0.3,
                   0.8, 0.2), nrow = 2, ncol = 2, byrow = TRUE)
theta_2 = matrix(c(0.5, 0.5,
                   0.7, 0.3), nrow = 2, ncol = 2, byrow = TRUE)


########## II. Leslie matrices ##########

# Leslie sub-matrices
M11 = matrix(c(S0*f1,           S0*f2,           S0*f3,
               S1*theta_1[1,1], 0,               0,
               0,               S2*theta_2[1,1], 0),
             nrow = 3, ncol = 3, byrow = T)
M12 = matrix(c(0,               0,               0,
               S1*theta_1[1,2], 0,               0,
               0,               S2*theta_2[1,2], 0),
             nrow = 3, ncol = 3, byrow = T)
M21 = matrix(c(0,               0,               0,
               S1*theta_1[2,1], 0,               0,
               0,               S2*theta_2[2,1], 0),
             nrow = 3, ncol = 3, byrow = T)
M22 = matrix(c(S0*f1,           S0*f2,           S0*f3,
               S1*theta_1[2,2], 0,               0,
               0,               S2*theta_2[2,2], 0),
             nrow = 3, ncol = 3, byrow = T)

# Combine matrices into complete Leslie matrix
leslie = rbind(cbind(M11,M21), cbind(M12,M22))


########## III. Simulate time series ##########

# Time steps
ntimes = 50

# Output matrix
N = matrix(nrow = ntimes, ncol = 3*2)
rownames(N) = paste("Time", seq(ntimes))
colnames(N) = paste(rep(paste("a", seq(3), sep = ""),2), ", Region ", rep(seq(2),each=3), sep = "")

# Set initial values
N[1,] = N0

# Loop through time steps
for(i in 2:ntimes) {
  N[i,] = leslie %*% N[i-1,]
}


########## IV. Plot results ##########

# Plot all ages and regions
matplot(seq(ntimes), N, type = "l", lwd = 3, col = rep(seq(3),2), lty = rep(c(1,2),3))
legend("topleft", colnames(N), lwd = 3, col = rep(seq(3),2), lty = rep(c(1,2),3))

# Create data frame
N.df = data.frame(N = c(N), Time = seq(ntimes), Age.Region = rep(colnames(N),each=ntimes))

# Use ggplot
ggplot(N.df, aes(x=Time,y=N,color=Age.Region)) + geom_line()


########## V. Eigenvalue analysis ##########

# Get eigenvalues of Leslie matrix
eigen(leslie)
