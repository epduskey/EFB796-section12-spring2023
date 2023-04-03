# Lecture 10 - Size-based Models

# Created: 30 Mar 2023
# Last modified: 30 Mar 2023 by EPD

# Contents (ctrl-f):
# I. Simulate Markov process data
# II. Simulate size-based Leslie matrix data


########## I. Simulate Markov process data ##########

# Initial distribution
pi_0 = c(1,0,0)

# Probability transition matrix
P = matrix(c(0.6, 0.4, 0.0,
             0.0, 0.8, 0.2,
             0.0, 0.0, 1.0), nrow = 3, ncol = 3, byrow = T)

# Number of individuals
ninds = 10

# Number of steps
nsteps = 100

# Create storage matrix
markov = matrix(nrow = nsteps, ncol = ninds)
rownames(markov) = paste("Step", seq(nrow(markov)))
colnames(markov) = paste("Ind", seq(ncol(markov)))
markov[1:10,1:10]

# Use initial distribution to sort individuals
initial_step = rmultinom(n = ninds, size = 1, prob = pi_0)
initial_step[,1]
which(initial_step[,1] == 1)
markov[1,] = apply(initial_step, 2, function(x) which(x == 1))
markov[1,]

# Simulate additional steps
for(i in 1:ninds) {
  for(j in 2:nsteps) {
    prob = P[markov[j-1,i],]
    markov[j,i] = which(rmultinom(n = 1, size = 1, prob = prob) == 1)
  }
}

# Plot with matplot
matplot(markov, type = "l", axes = F, xlab = "Step", ylab = "State")
axis(side = 1, at = c(1,20,40,60,80,100))
axis(side = 2, at = c(1,2,3))
box()


########## II. Simulate size-based Leslie matrix data ##########

# Fecundity values
fecundity = c(0, 0, 0, 0, 0, 0, 0, 1, 3, 6, 10, 12, 13, 16, 20)

# Survival values
survival = c(0.1, 0.2, 0.2, 0.3, 0.3, 0.5, 0.6, 0.6, 0.7, 0.7, 0.7, 0.8, 0.8, 0.8, 0.8)

# First year survival
s0 = 0.01

# Size transition matrix
Psize = matrix(c(0.2, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
                 0.5, 0.1, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
                 0.3, 0.8, 0.1, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
                 0.0, 0.1, 0.9, 0.1, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
                 0.0, 0.0, 0.0, 0.9, 0.1, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
                 0.0, 0.0, 0.0, 0.0, 0.7, 0.2, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
                 0.0, 0.0, 0.0, 0.0, 0.2, 0.8, 0.3, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.4, 0.2, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.2, 0.7, 0.3, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.1, 0.1, 0.5, 0.4, 0.0, 0.0, 0.0, 0.0, 0.0,
                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.2, 0.4, 0.4, 0.0, 0.0, 0.0, 0.0,
                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.1, 0.5, 0.5, 0.0, 0.0, 0.0,
                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.1, 0.1, 0.4, 0.7, 0.0, 0.0,
                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.1, 0.2, 0.7, 0.0,
                 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.1, 0.3, 1.0),
                 nrow = 15, ncol = 15, byrow = T)

# Construct projection matrix
project = Psize * matrix(survival, nrow = 15, ncol = 15, byrow = T)
project[1,] = project[1,] + fecundity * s0

# Initial distribution
initial_sizes = c(0.02, 0.12, 0.07, 0.09, 0.08, 0.08, 0.10, 0.09, 0.07, 0.06, 0.10, 0.03, 0.05, 0.03, 0.01)

# Number of individuals to start with
nstart = 10000

# Number of time steps
ntimes = 30

# Initialize population size structure matrix
N = matrix(nrow = ntimes, ncol = 15)
rownames(N) = paste("Time", seq(nrow(N)))
colnames(N) = paste("Size Class", seq(ncol(N)))

# Starting values for each size class
N[1,] = as.vector(rmultinom(n = 1, size = nstart, prob = initial_sizes))

# Project forward for each time step
for(i in 2:nrow(N)) {
  N[i,] = project %*% N[i-1,]
}

# Use matplot
matplot(N, type = "l", xlab = "Time")
lines(seq(nrow(N)), rowSums(N), lwd = 4)