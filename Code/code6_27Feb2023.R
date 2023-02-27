# Lecture 6 - Recruitment

# Created: 26 Feb 2023
# Last modified: 26 Feb 2023 by EPD

# Install and load packages
library(rje)
library(investr)

# Set working directory
setwd("/Users/epdus/OneDrive/Breathless/Teaching/EFB796")

# Contents (ctrl-f):
# I. Simulate density-independent stock-recruit data
# II. Simulate density-dependent stock-recruit data
# III. Overcompensation
# IV. Depensation
# V. Jackknife estimation


########## I. Simulate density-independent stock-recruit data ##########

# Eggs per female
epf = 4000

# Probability of survival to recruitment
ps = 0.01

# Number of females
Nf = seq(0,1000,length.out=100)

# Simulate recruitment with process error
Nrecs = Nf * epf * ps * rnorm(n = length(Nf), mean = 1, sd = 0.2)

# Plot the results
plot(Nf, Nrecs)


########## II. Simulate density-dependent stock-recruit data ##########

# Survival decreases to some asymptote as number of females increases
# https://www.desmos.com/calculator/gnicfmsrbv
ps_dd = ps * (1 - Nf/(50+Nf))

# Calculate number of recruits with variable survival probability
Nrecs_dd = Nf * epf * ps_dd

# Plot the results
plot(Nf, Nrecs_dd, type = "l", lwd = 2,
     xlab = "Number of Females", ylab = "Number of Recruits")


########## III. Overcompensation ##########

# Decrease in probability of survival with number of females
m_dd = -0.002

# Probability of survival to recruitment
ps_dd = m_dd * Nf + ps

# Plot the results; what's wrong with this plot?
plot(Nf, ps_dd, xlab = "Number of Females", ylab = "Probability of Survival")

# Convert the linear relationship to the correct scale
ps_dd = expit(m_dd * Nf + logit(ps))

# Plot the results
plot(Nf, ps_dd, xlab = "Number of Females", ylab = "Probability of Survival")

# Let's turn this into a function
# Nf_max: maximum number of females
# epf: number of eggs per female
# ps: baseline probability of survival
# m_dd: rate of decline in egg survival probability with increasing spawners
# plot.line: if TRUE, plots survival ~ females line on existing graph
# Nf: allows user to provide their own vector for number of females
# col.line: user must provide a color if plot.line is TRUE
# returns probability of survival
survival = function(Nf_max, epf, ps, m_dd, plot.line = F, Nf = NULL, col.line = NULL) {
  
  # Create Nf vector if none is provided
  if(is.null(Nf)) {
    Nf = seq(0, Nf_max)
  }
  
  # Linear relationship of survival as a function of #females on standard scale
  ret_st = m_dd * Nf + logit(ps)
  
  # Convert to logit scale
  ret = expit(ret_st)
  
  # Add survival line to an existing plot
  if(plot.line) lines(Nf, ret, lwd = 2, col = col.line)
  
  # Return result
  return(return(ret))
}

# Plot using several different slopes
m_vec = seq(-0.003,-0.001,0.0001)

# Choose a line color for each slope
col_vec = hcl.colors(n = length(m_vec), palette = "Purp", rev = T)

# Initialize plot
par(mar = c(5,5,3,7))
plot(0, type = "n", ylim = c(0.000,0.010), xlim = range(Nf), 
     xlab = "Number of Females", ylab = "Probability of Egg Survival",
     cex.lab = 1.2, cex.axis = 1.2, font.axis = 4, font.lab = 4)

# Apply survival function to different slope and color values
m_survival = mapply(survival, 
                    Nf_max = max(Nf), 
                    epf = epf, 
                    ps = ps, 
                    m_dd = m_vec, 
                    plot.line = T, 
                    col.line = col_vec)

# Add color scale
image.plot(zlim = c(-0.003,-0.001), legend.only = T, legend.lab = "Slope", legend.line = -2, col = hcl.colors(1001,"Purp",rev=T))
dev.off()

# Calculate the number of recruits
Nrecs_dd = Nf * epf * survival(Nf = Nf, epf = epf, ps = ps, m_dd = -0.002)
plot(Nf, Nrecs_dd, type = "l", lwd = 6,
     xlab = "Number of Females", ylab = "Number of Recruits")


########## IV. Depensation ##########

# Number of eggs per female increases as number of females increases
# Nf: number of females
# m: slope of linear relationship between number of females and eggs per female
# b: intercept of linear relationship between number of females and eggs per female
# epf_max: maximum number of eggs per female
# returns number of eggs per female
EPF = function(Nf, m, b, epf_max) {
  
  # Calculate proportion of maximum eggs per female on standard scale
  epf_st = m * Nf + b
  
  # Convert relationship to the logit scale
  epf_logit = expit(epf_st)
  
  # Calculate and return number of eggs per female
  return(epf_logit * epf_max)
}

# Choose slope and intercept of increase in eggs per female as a function of the number of females
m_dp = 0.03
b_dp = -5

# Calculate the number of eggs per female
epf_dp = EPF(Nf, m_dp, b_dp, epf)

# Calculate recruits
Nrecs_dp = Nf * epf_dp * ps_dd

# Plot the results
plot(Nf, Nrecs_dp, type = "l", lty = 4, lwd = 4,
     xlab = "Number of Females", ylab = "Number of Recruits",
     font.lab = 2, font = 2, cex.axis = 1.2, cex.lab = 1.2)


########## V. NLS estimation ##########

# Load in fin whale data
fin = read.csv("Data/finwhales.csv")
head(fin)

# Plot the data in a few ways
plot(Recruits ~ Year, data = fin, pch = 17, cex = 1.5)
plot(Females ~ Year, data = fin, pch = 17, cex = 1.5)
plot(Recruits ~ Females, data = fin, pch = 17, cex = 1.5)

# Linear model
fin.lm = nls(Recruits ~ a*Females,
             data = fin,
             start = c(a=0.1))

# Ricker model
fin.rk = nls(Recruits ~ a*Females*exp(-b*Females), 
             data = fin, 
             start = c(a=0.1,b=1e-3))

# Compare models
anova(fin.lm, fin.rk)

# Make predictions
x.new = seq(0,200)
lm.preds = predFit(fin.lm, newdata = list(Females=x.new), interval = "confidence")
rk.preds = predFit(fin.rk, newdata = list(Females=x.new), interval = "confidence")

# Initalize results plot
plot(Recruits ~ Females, data = fin,
     xlim = c(0,200), ylim = c(0,100),
     pch = 17, cex = 1.5,
     cex.axis = 1.2, cex.lab = 1.2)

# Add linear model
lines(x.new, lm.preds[,1], lwd = 3, col = "red")
polygon(x = c(x.new,rev(x.new)),
        y = c(lm.preds[,2],rev(lm.preds[,3])),
        border = NA, col = adjustcolor("red",alpha.f=0.3))

# Add Ricker model
lines(x.new, rk.preds[,1], lwd = 3, col = "blue")
polygon(x = c(x.new,rev(x.new)),
        y = c(rk.preds[,2],rev(rk.preds[,3])),
        border = NA, col = adjustcolor("blue",alpha.f=0.3))


########## VI. Jackknife estimation ##########

# Load in pink salmon data
pink = read.table("Data/pink.txt", sep = "\t", header = T)
head(pink)
pink

# Initialize plot
plot(Return ~ Escape, data = pink, 
     xlim = c(0,10000), ylim = c(0,30000),
     cex.axis = 1.2, cex.lab = 1.2, font = 4, font.lab = 4,
     pch = 23, lwd = 3, bg = "pink")

# Do jackknife estimation for the Beverton-Holt model
for(i in 1:nrow(pink)) {
  
  # Remove one data point
  pink.jk = pink[-i,]
  
  # Run the model on the new data set
  pinkjk.nls = nls(Return ~ a*Escape/(1+b*Escape), 
                   data = pink.jk,
                   start = c(a=0.3,b=1e-5))
  
  # Prediction Escape values
  x.new = list(Escape = seq(0,10000))
  
  # Add a transparent line to the plot
  lines(x.new$Escape, predict(pinkjk.nls,newdata=x.new),
        lwd = 1.2, col = adjustcolor("black",0.2))
}