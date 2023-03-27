# Lecture 9 - Bayesian Estimation

# Created: 25 Mar 2023
# Last modified: 25 Mar 2023 by EPD

# Load packages
library(rstan)
library(rstanarm)
library(loo)
library(bayesplot)
library(ggplot2)
library(boot)
library(modelr)

# Contents (ctrl-f):
# I. Simulate linear data
# II. Linear regression with Stan
# III. GLM with Stan
# IV. Random effects


########## I. Simulate linear data ##########

# Number of points
npts = 100

# Choose intercept
b = 2

# Choose slope
m = -3

# Choose x-values
x = seq(-5, 5, length.out = npts)

# Simulate y-values
y = rnorm(npts, mean = m*x+b, sd = 2)

# Store results in data frame
xy = data.frame(x = x, y = y)

# Plot data
dev.off()
ggplot(xy, aes(x=x,y=y)) +
  geom_point(size = 2)


########## II. Linear regression with Stan ##########

# Write model with mean only
stan.h0 = "
data
{
  // Number of observations
  int n;
  
  // x-values
  vector[n] x;
  
  // y-values
  vector[n] y;
}
parameters
{
  // Mean
  real mu;
  
  // SD
  real<lower=0> sigma;
}
model
{
  // Priors
  mu ~ normal(0,1000);
  sigma ~ gamma(1,1);
  
  // Likelihood
  y ~ normal(mu, sigma);
}
"

# Write model with slope and intercept
stan.h1 = "
data
{
  // Number of observations
  int n;
  
  // x-values
  vector[n] x;
  
  // y-values
  vector[n] y;
}
parameters
{
  // Intercept
  real b;
  
  // Slope
  real m;
  
  // SD
  real<lower=0> sigma;
}
transformed parameters {
  // Expected mean
  vector[n] mu;
  
  // Calculate expectation
  mu = m*x + b;
}
model
{
  // Priors
  b ~ normal(0,1000);
  m ~ normal(0,1000);
  sigma ~ gamma(1,1);
  
  // Likelihood
  y ~ normal(mu, sigma);
}
"

# Compile models
lm.h0 = stan_model(model_code = stan.h0, model_name = "stanh0")
lm.h1 = stan_model(model_code = stan.h1, model_name = "stanh1")

# Create list of data
dat = list(n = npts, x = x, y = y)

# Create function for initial values
inits.h0 = function() {list(mu=0,sigma=1)}
inits.h1 = function() {list(b=0,m=0,sigma=1)}

# Run the null hypothesis model
h0.stan = sampling(lm.h0,
                   data = dat,
                   chains = 3,
                   cores = 3,
                   warmup = 500,
                   iter = 1000,
                   refresh = 100,
                   init = inits.h0)

# Run alternative hypothesis model
h1.stan = sampling(lm.h1,
                   data = dat,
                   chains = 3,
                   cores = 3,
                   warmup = 500,
                   iter  = 1000,
                   refresh = 100,
                   init = inits.h1)

# Print model output
print(h0.stan)
print(h1.stan)

# Check model diagnostics
check_hmc_diagnostics(h0.stan)
check_hmc_diagnostics(h1.stan)

# Extract samples
h0.samples = rstan::extract(h0.stan, pars = c("mu","sigma"), inc_warmup = T, permuted = F)
h1.samples = rstan::extract(h1.stan, pars = c("m","b","sigma"), inc_warmup = T, permuted = F)

# Plot chains
mcmc_trace(h0.samples, n_warmup = 500) + theme_classic()
mcmc_trace(h1.samples, n_warmup = 500) + theme_classic()

# Density plots
mcmc_areas(h0.stan, pars = c("mu","sigma")) + theme_bw()
mcmc_areas(h1.stan, pars = c("m","b","sigma")) + theme_bw()

# Scatter plot
mcmc_pairs(h1.stan, pars = c("b","m"))

# NUTS energy
mcmc_nuts_energy(nuts_params(h1.stan))


########## III. GLM with Stan ##########

# Read in bee abundance and richness data
bee = read.csv(url("https://zenodo.org/record/5842604/files/Gerner_Sargent-2021-Bee-and-Floral-Supplementary.csv?download=1"))
head(bee)
summary(bee)

# Scale floral richness and abundance data
bee$fa_scale = as.vector(scale(bee$Floral.Abundance))
bee$fr_scale = as.vector(scale(bee$Floral.Richness))

# Look at pairs plot to see potential relationships
pairs(bee[,-1])

# See if abundance and richness of flora is correlated
corr(as.matrix(bee[,c("Floral.Abundance","Floral.Richness")]))

# Write model of bee abundance as a function of floral abundance and richness
model.bees = "
data
{
  // Number of observations
  int n;
  
  // Bee abundance
  array[n] int bees;
  
  // Floral abundance
  vector[n] fa;
  
  // Floral richness
  vector[n] fr;
  
  // Number of predicted values
  int npreds;
  
  // Floral abundance for predictions
  vector[npreds] fa_preds;
  
  // Floral richness for predictions
  vector[npreds] fr_preds;
}
parameters
{
  // Floral abundance effect
  real beta_fa;
  
  // Floral richness effect
  real beta_fr;
}
transformed parameters
{
  // Expected mean
  vector[n] lambda;
  
  // Calculate mean with log-link
  lambda = exp(beta_fa*fa + beta_fr*fr);
}
model
{
  // Priors
  beta_fa ~ normal(0,1000);
  beta_fr ~ normal(0,1000);
  
  // Likelihood
  bees ~ poisson(lambda);
}
generated quantities
{
  // Predicted values
  vector[npreds] preds;
  
  // Calculate predictions
  preds = exp(beta_fa*fa_preds + beta_fr*fr_preds);
}
"

# Compile model
bees.mod = stan_model(model_code = model.bees, model_name = "stanbees")

# Prediction data
preds.df = data.frame(fa = seq_range(bee$fa_scale,10), fr = rep(seq_range(bee$fr_scale,10),each=10))

# Create list of data
dat = list(n = nrow(bee), 
           bees = bee$Bee.Abundance, 
           fa = bee$fa_scale, 
           fr = bee$fr_scale,
           npreds = nrow(preds.df),
           fa_preds = preds.df$fa,
           fr_preds = preds.df$fr)

# Create function for initial values
inits.bees = function() {list(beta_fa=0,beta_fr=0)}

# Run the null hypothesis model
bees.stan = sampling(bees.mod,
                   data = dat,
                   chains = 3,
                   cores = 3,
                   warmup = 500,
                   iter = 1000,
                   refresh = 100,
                   init = inits.bees)

# Print model output
print(bees.stan)

# Check model diagnostics
check_hmc_diagnostics(bees.stan)

# Extract samples
bees.samples = rstan::extract(bees.stan, pars = c("beta_fa","beta_fr"), inc_warmup = T, permuted = F)

# Density plots
mcmc_areas(bees.stan, pars = c("beta_fa","beta_fr")) + theme_bw()

# Extract and store mean predictions
preds.df$preds = as.vector(get_posterior_mean(bees.stan)[-c(1:29,130),-(1:3)])

# Plot predictions of abundance
ggplot(preds.df, aes(x=fa,y=fr,fill=preds)) +
  geom_raster() +
  ggtitle("Bee abundance", "fa: floral abundance; fr: floral richness")


########## IV. GLM with random effects ##########

# Write model of bee abundance as a function of floral abundance and richness
model.re = "
data
{
  // Number of observations
  int n;
  
  // Bee abundance
  array[n] int bees;
  
  // Floral abundance
  vector[n] fa;
  
  // Floral richness
  vector[n] fr;
  
  // Site
  array[n] int site;
  
  // Number of predicted values
  int npreds;
  
  // Floral abundance for predictions
  vector[npreds] fa_preds;
  
  // Floral richness for predictions
  vector[npreds] fr_preds;
}
parameters
{
  // Floral abundance effect
  real beta_fa;
  
  // Floral richness effect
  real beta_fr;
  
  // Site effect
  vector[n] sef;
  
  // Site effect mean and SD
  real mu_site;
  real sigma_site;
}
transformed parameters
{
  // Expected mean
  vector[n] lambda;
  
  // Calculate mean with log-link
  lambda = exp(beta_fa*fa + beta_fr*fr + sef);
}
model
{
  // Priors
  beta_fa ~ normal(0,1000);
  beta_fr ~ normal(0,1000);
  sef ~ normal(mu_site,sigma_site);
  
  // Likelihood
  bees ~ poisson(lambda);
}
generated quantities
{
  // Predicted values
  vector[npreds] preds;
  
  // Calculate predictions
  preds = exp(beta_fa*fa_preds + beta_fr*fr_preds);
}
"

# Compile model
re.mod = stan_model(model_code = model.re, model_name = "rebees")

# Prediction data
preds.df.re = data.frame(fa = seq_range(bee$fa_scale,10), fr = rep(seq_range(bee$fr_scale,10),each=10))

# Create list of data
dat = list(n = nrow(bee), 
           bees = bee$Bee.Abundance, 
           fa = bee$fa_scale, 
           fr = bee$fr_scale,
           site = seq(nrow(bee)),
           npreds = nrow(preds.df),
           fa_preds = preds.df$fa,
           fr_preds = preds.df$fr)

# Create function for initial values
inits.re = function() {list(beta_fa=0,beta_fr=0,sef=rep(0,nrow(bee)),mu_site=0,sigma_site=1)}

# Run the null hypothesis model
re.stan = sampling(re.mod,
                   data = dat,
                   chains = 3,
                   cores = 3,
                   warmup = 500,
                   iter = 1000,
                   refresh = 100,
                   init = inits.re)

# Print model output
print(re.stan)

# Check model diagnostics
check_hmc_diagnostics(re.stan)

# Extract samples
re.samples = rstan::extract(re.stan, pars = c("beta_fa","beta_fr","mu_site","sigma_site"), inc_warmup = T, permuted = F)

# Density plots
mcmc_areas(re.stan, pars = c("beta_fa","beta_fr")) + theme_bw()
mcmc_areas(re.stan, pars = c("mu_site","sigma_site")) + theme_bw()