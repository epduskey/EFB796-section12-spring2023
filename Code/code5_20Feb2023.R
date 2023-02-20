# Lecture 5 - Linear and Nonlinear Models

# Created: 17 Feb 2023
# Last modified: 17 Feb 2023 by EPD

# Install and load packages
#install.packages("investr")
library(investr)

# Set working directory
setwd("/Users/epdus/OneDrive/Breathless/Teaching/EFB796")

# Contents (ctrl-f):
# I. Simulate linear data
# II. Run linear model
# III. Plot linear model
# IV. Simulate nonlinear data
# V. Run nonlinear model
# VI. Plot nonlinear model
# VII. Load deer data
# VIII. Run generalized linear model (GLM)
# VII. Load BBS data
# VIII. Run generalized linear model (GLM)


########## I. Simulate linear data ##########

# Choose slope
m = 2

# Choose intercept
b = 0

# Choose x values
x = seq(0, 10, 0.1)

# Deterministic data
y = m*x + b
plot(x, y)

# Generate data
set.seed(897)
y_sampling = m*x + b + rnorm(n = length(x), mean = 0, sd = 2)
y_process = rnorm(n = length(x), mean = m*x + b, sd = 2)

# Plot data
par(mfrow = c(1,2))
plot(x, y_sampling, pch = 16, cex = 0.8, xlab = "", ylab = "", main = "Sampling Error")
plot(x, y_process, pch = 16, cex = 0.8, xlab = "", ylab = "", main = "Process Error")
mtext("x", side = 1, outer = TRUE, line = -2)
mtext("y", side = 2, outer = TRUE, line = -1)
par(mfrow = c(1,1))


########## II. Run linear model ##########

# Run a simple linear model
sampling.lm = lm(y_sampling ~ x)

# Look at the results
summary(sampling.lm)
plot(sampling.lm)

# Run the same model on the process error data
process.lm = lm(y_process ~ x)
summary(process.lm)
plot(process.lm)


########## III. Plot linear model ##########

# Look at coefficient estimates from linear models
coef(sampling.lm)
coef(process.lm)

# Make predictions
ys.preds = predict(sampling.lm)
yp.preds = predict(process.lm)

# Plot predictions
plot(x, ys.preds, type = "l", lwd = 3, lty = 1, col = "red")
lines(x, yp.preds, lwd = 3, lty = 2, col = "cyan")
legend("topleft", bty = "n", c("sampling","process"), lty = c(1,2), col = c("red","cyan"))

# Make new predictions
x.new = seq(0, 10)
ys.preds.new = coef(sampling.lm)[1] + coef(sampling.lm)[2] * x.new
yp.preds.new = coef(process.lm)[1] + coef(process.lm)[2] * x.new
plot(x.new, ys.preds.new, type = "l", lwd = 3, lty = 1, col = "red")
lines(x.new, yp.preds.new, lwd = 3, lty = 2, col = "cyan")
legend("topleft", bty = "n", c("sampling","process"), lty = c(1,2), col = c("red","cyan"))

# Make new predictions using predict
ys.preds.fn = predict(sampling.lm, newdata = list(x = x.new))
yp.preds.fn = predict(process.lm, newdata = list(x = x.new))

# Check that the manual and predict methods produce the same values
identical(ys.preds.new, unname(ys.preds.fn))
identical(yp.preds.new, unname(yp.preds.fn))
sum(ys.preds.new - ys.preds.fn)
sum(yp.preds.new - yp.preds.fn)

# Add confidence intervals
ys.preds.se = predict(sampling.lm, newdata = list(x = x.new), se.fit = TRUE)
yp.preds.se = predict(process.lm, newdata = list(x = x.new), se.fit = TRUE)
str(ys.preds.se)

# Plot data with estimates and CI
plot(x, y_sampling, 
     xlab = "x", ylab = "y", 
     pch = 15, cex.lab = 1.5, cex.axis = 1.5, cex = 1.5, 
     col = rgb(0,0,0,0.5))
lines(x.new, ys.preds.se$fit, lwd = 2, col = "red")
lines(x.new, ys.preds.se$fit + 2 * ys.preds.se$se.fit, lwd = 2, lty = 2, col = "red")
lines(x.new, ys.preds.se$fit - 2 * ys.preds.se$se.fit, lwd = 2, lty = 2, col = "red")


########## IV. Simulate nonlinear data ##########

# Logistic model function
# N0: population size at time 0
# r: instantaneous growth rate
# K: carrying capacity
# t: time
# err: process error
# returns population size at time t
logistic = function(N0, r, K, t, err) {

  # Calculate population at time t
  N = (N0*exp(r*t*err)) / (1-(N0/K)+(N0/K)*exp(r*t*err))
  
  # Return result
  return(N)
}

# Choose population values
N0 = 1000
r = 0.1
K = 20000
err = 1

# Apply function to a sequence of times with no process error
times = seq(0,100)
N = sapply(times, logistic, N0 = N0, r = r, K = K, err = err)
plot(times, N)

# Add process error
set.seed(2347)
err = rnorm(length(times), mean = 1, sd = 0.2)

# Loop through process error values for each time
N_err = vector()
for(i in 1:length(times)) {
  N_err[i] = logistic(N0, r, K, times[i], err[i])
}
plot(times, N_err)

# Alternatively, use mapply, for which multiple arguments can vary simultaneously
N_mapply = mapply(logistic, N0 = N0, r = r, K = K, t = times, err = err)
plot(times, N_mapply) 
  

########## V. Run nonlinear model ##########

# Create data frame to house population and time values
N.df = data.frame(N = N_mapply, t = times)
head(N.df)

# Use nonlinear least squares
N.nls = nls(N ~ (N0*exp(r*t)) / (1-(N0/K)+(N0/K)*exp(r*t)), 
            data = N.df,
            start = list(N0=800,r=0.2,K=15000),
            trace = TRUE)
summary(N.nls)
str(N.nls)

# Use the predFit function to get fit and CI
N.preds = predFit(N.nls, interval = "confidence")
str(N.preds)


########## VI. Plot nonlinear model ##########

# Get parameter estimates
N0.est = coef(N.nls)["N0"]
r.est = coef(N.nls)["r"]
K.est = coef(N.nls)["K"]

# Use the logistic function to calculate mean estimate
N.est = logistic(N0.est, r.est, K.est, times, err = 1)

# Plot data
plot(N ~ t, N.df, 
     pch = 18, cex = 2, col = rgb(1,0,1,0.4),
     font = 6, cex.axis = 1.2,
     font.lab = 6, cex.lab = 1.2)

# Add estimated line
lines(times, N.est, lwd = 3, col = "midnightblue")

# Convert your color to rgb values
mb.rgb = col2rgb("midnightblue")

# Plot shaded confidence intervals
polygon(x = c(times,rev(times)), 
        y = c(N.preds[,"upr"],rev(N.preds[,"lwr"])),
        border = NA,
        col = rgb(mb.rgb["red",],mb.rgb["green",],mb.rgb["blue",],100,maxColorValue=255))


########## VII. Load deer data ##########

# Read in deer data
deer = read.csv("Data/wtdeer.csv")

# Check out the data
str(deer)

# Get data summary
summary(deer)

# Visualize data
hist(deer$Harvest_Males)
hist(deer$Harvest_Females)
hist(deer$Effort_Males)
hist(deer$Effort_Females)

# Check normality of harvest data
qqnorm(deer$Harvest_Males)
qqline(deer$Harvest_Males)
qqnorm(deer$Harvest_Females)
qqline(deer$Harvest_Females)


########## VIII. Run generalized linear model (GLM) ##########

# Run a poisson model
deer.glm = glm(Harvest_Males ~ Effort_Males, deer, family = poisson())

# Look at model output
summary(deer.glm)

# Plot model output
plot(deer.glm)

# Compare predictions to output
plot(deer$Harvest_Males, deer.glm$fitted.values)
abline(0,1)

# Make predictions
effort.preds = seq(min(deer$Effort_Males), max(deer$Effort_Males))
harvest.preds = predict(deer.glm, newdata = list(Effort_Males = effort.preds), type = "response", se.fit = TRUE)

# Plot output
plot(Harvest_Males ~ Effort_Males, deer, 
     xlab = "Effort", ylab = "Harvest", main = "Males",
     pch = 22, bg = "darkred", cex = 1.2,
     font = 6, font.lab = 6, font.main = 6, 
     cex.lab = 1.4, cex.main = 2, cex.axis = 1.2)

# Add prediction line and dotted CI's
lines(effort.preds, harvest.preds$fit, lwd = 3, col = "darkblue")
lines(effort.preds, harvest.preds$fit + 2*harvest.preds$se.fit, lwd = 3, lty = 2, col = "darkblue")
lines(effort.preds, harvest.preds$fit - 2*harvest.preds$se.fit, lwd = 3, lty = 2, col = "darkblue")