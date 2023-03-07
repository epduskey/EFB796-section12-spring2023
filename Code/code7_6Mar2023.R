# Lecture 7 - Bioenergetics and Individual Growth

# Created: 4 Mar 2023
# Last modified: 7 Mar 2023 by EPD

# Install and load packages
library(ggplot2)
library(jagsUI)
library(modelr)

# Set working directory
setwd("/Users/epdus/OneDrive/Breathless/Teaching/EFB796")

# Contents (ctrl-f):
# I. Load and plot data
# II. Bootstrap resampling
# III. Gompertz using JAGS


########## I. Load and plot data ##########

# Load leaf area data
leaf = read.csv("Data/leafarea.csv")

# Set growth stage as a factor
leaf$Stage = factor(leaf$Stage, levels = c("early","late"))

# Look at data structure
str(leaf)

# Use ggplot to plot by stage
ggplot(leaf, aes(x=Day, y=LeafArea)) + 
  geom_point(shape = 18, color = "darkblue") +
  geom_smooth(span = 0.3) + 
  facet_grid(~ Stage, scales = "free") +
  theme(strip.text.x = element_text(size=15),
        axis.text = element_text(size=12),
        axis.title = element_text(size=14,face="bold"),
        panel.grid  = element_blank(),
        panel.grid.major=element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour="black",fill=NA,linewidth=2)) +
  xlab("Day") + 
  ylab("Leaf area")


########## II. Bootstrap resampling ##########

# Subset leaf data
leaf.early = subset(leaf, Stage == "early")

# Run NLS von Bertalanffy model on early stage leaf area
leaf.nls = nls(LeafArea ~ Ainf * (1 - exp(-k*(Day - t0))),
               data = leaf.early,
               start = c(Ainf=10,k=0.1,t0=-1))

# Check results
summary(leaf.nls)

# Use ggplot to plot early stage only
p = ggplot(leaf.early, aes(x=Day, y=LeafArea)) + 
  geom_point(shape = 18, color = "darkblue") +
  theme(strip.text.x = element_text(size=15),
        axis.text = element_text(size=12),
        axis.title = element_text(size=14,face="bold"),
        panel.grid  = element_blank(),
        panel.grid.major=element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour="black",fill=NA,linewidth=2)) +
  xlab("Day") + 
  ylab("Leaf area")

# See plot
p

# Number of bootstrap samples
nboot = 1000
npreds = 100

# Prediction data frame
x.new = seq_range(leaf.early$Day, n = npreds)

# Do bootstrap resampling
for(i in 1:nboot) {
  
  # Resample data indices with replacement
  inds = sample(seq(nrow(leaf.early)), size = nrow(leaf.early), replace = T)
  
  # Access resampled data rows
  tempdat = leaf[inds,]
  
  # Run NLS on resampled data
  temp.nls = nls(LeafArea ~ Ainf * (1 - exp(-k*(Day - t0))),
                 data = tempdat,
                 start = c(Ainf=10,k=0.1,t0=-1))
  
  # Make predictions
  y.new = predict(temp.nls, newdata = list(Day = x.new))
  df = data.frame(Day = x.new, LeafArea = y.new)
  
  # Add new line to ggplot
  p = p + geom_line(data = df, aes(x = Day, y = LeafArea), color = "darkgreen", linetype = "dotted", linewidth = 0.1)
}

# See new plot
p


########## III. Gompertz using JAGS ##########

# Extract late emergent leaf growth data
leaf.late = subset(leaf, Stage == "late")

# Use ggplot to plot late stage only
p = ggplot(leaf.late, aes(x=Day, y=LeafArea)) + 
  geom_point(shape = 18, color = "darkblue") +
  theme(strip.text.x = element_text(size=15),
        axis.text = element_text(size=12),
        axis.title = element_text(size=14,face="bold"),
        panel.grid  = element_blank(),
        panel.grid.major=element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour="black",fill=NA,linewidth=2)) +
  xlab("Day") + 
  ylab("Leaf area")

# See plot
p

# Write JAGS model
cat("
model {
    
  # Likelihood
  for(i in 1:nobs){
  
    # Calculate expected mean for each observation
    mu[i] = A_inf * exp(-exp(-k*(Day[i] - ti)))
        
    # Choose likelihood probability distribution
    X[i] ~ dnorm(mu[i],tau)
        
  }
      
  # Priors
  A_inf ~ dunif(40,80)
  k ~ dgamma(0.001,0.001)
  ti ~ dgamma(0.001,0.001)
  tau ~ dgamma(0.001,0.001)
  
  # Generate predicted values
  for(i in 1:npreds) {
    y.preds[i] = A_inf * exp(-exp(-k*(x.preds[i] - ti)))
  }
  
}", file = "gompertz.jags")

# Create predictive data
x.preds = seq_range(leaf.late$Day, n = 50)

# Store data in a list
data.jags = list(nobs = nrow(leaf.late),
                 Day = leaf.late$Day,
                 X = leaf.late$LeafArea,
                 x.preds = x.preds,
                 npreds = length(x.preds))

# Write function to contain starting values
inits.jags = function() {
  list(A_inf = 60,
       k = 0.2,
       ti = 20,
       tau = 10)
}

# Set parameters to monitor
params.jags = c("A_inf",
                "k",
                "ti",
                "tau",
                "y.preds")

# Run JAGS model
gomp.jags = jags(data = data.jags,
                 inits = inits.jags,
                 parameters.to.save = params.jags,
                 model.file = "gompertz.jags",
                 n.chains = 3,
                 n.adapt = 1000,
                 n.iter = 6000,
                 n.burnin = 1000,
                 n.thin = 2)

# Examine output
gomp.jags

# Create prediction data frame
jags.df = data.frame(Day = x.preds,
                     LeafArea = gomp.jags$mean$y.preds,
                     lowerCI = gomp.jags$q2.5$y.preds,
                     upperCI = gomp.jags$q97.5$y.preds)

# Add line and ribbon to the plot
p = p + geom_line(data = jags.df) +
  geom_ribbon(data = jags.df, aes(ymin = lowerCI, ymax = upperCI), alpha = 0.2)
p
