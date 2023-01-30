# Lecture 2 - Introduction to R

# Created: 29 January, 2023
# Last modified: 30 January, 2023 by EPD

# Load packages
library(readxl)

# Contents (ctrl-f):
# I. Set working directory
# II. Read in data
# III. Packages
# IV. Examine data
# V. Manipulate data
# VI. Analyze data
# VII. Function-ize it


########## I. Set working directory ##########

# Show current working directory
getwd()

# Set your own working directory (all these things work)
setwd("C:\\Users\\epdus\\OneDrive\\Breathless\\Teaching\\EFB796")
setwd("C:/Users/epdus/OneDrive/Breathless/Teaching/EFB796")
setwd("/Users/epdus/OneDrive/Breathless/Teaching/EFB796")

# List files in current directory
list.files()


########## II. Read in data ##########

# Read in a text file
df.txt = read.table("Data/code1_df.txt", header = T)

# Read in a CSV file
df.csv = read.csv("Data/code1_df.csv")

# Read in an Excel file
df.xl = read_excel("Data/code1_df.xlsx")

# Read in fish stomach data
ray = read.csv("Data/blonderay_diet.csv")

# Load in data from the web
url = "https://zenodo.org/record/2533383/files/export.csv?download=1"
trap = read.csv(url)
head(trap)
str(trap)


########## III. Packages ##########

# Install and load a package finder
install.packages("packagefinder")
library(packagefinder)

# Find packages for time series data
findPackage(keywords = "time series")

# Find packages for time series and regressions
findPackage(keywords = c("time series","regression"), mode = "and")

# Install package "dyn"
install.packages("dyn")
library(dyn)

# Get help with a function
help("dyn-package")

# Run example provided
x <- ts(seq(10)^2)
dyn$lm(x ~ lag(x,-1))
dyn$glm(x ~ lag(x,-1))
dyn$loess(x ~ lag(x,-1))


########## IV. Examine data ##########

# See the first few values of your three data frames
head(df.txt)
head(df.csv, n = 3)
head(df.xl)

# See the column names, dimensions, and data types
str(df.txt)
str(df.csv)
str(df.xl)

# Add new column
df.txt$z = round(df.txt$y)

# Change data type
df.txt$z_factor = factor(df.txt$z)

# Take another look
head(df.txt)
str(df.txt)

# Look at the ray data
head(ray)
str(ray)

# Get a sense of the values
unique(ray$Year)
unique(ray$Sea)
range(ray$Number.of.stomachs)
hist(ray$Mean.length.of.predator)


########## V. Manipulate data ##########

# Refresh yourself on the ray data
str(ray)
head(ray)

# Create a new column of predator-prey length ratio (PPMR)
ray$PPMR = ray$Mean.length.of.predator / ray$Prey.Length
head(ray)

# Calculate standard stats of PPMR
mean(ray$PPMR, na.rm = T) # Remove the NA values with na.rm = T
sd(ray$PPMR, na.rm = T)

# See ICES subdivisions
ray$Ices.division
ray[, "Ices.division"]
ray[, 5]
ray[1,5]

# See all possible prey items
unique(ray$Prey.common.name)

# Only look at sandeels
ray[ray$Prey.common.name == "SANDEELS", ]

# Only look at non-pooled data and select only relevant columns
ray.inds = subset(x = ray, subset = Pooled == "n", select = c(Year,Date,Sea,Predator.ID,Mean.length.of.predator,Prey.common.name,Prey.Length,PPMR))
head(ray.inds)
str(ray.inds)

# Split data into years
ray.years = split(ray.inds, ray.inds$Year)
length(ray.years)
lapply(ray.years, nrow)


########## VI. Analyze data ##########

# Does the percentage sandeels in blonde ray stomachs change over time?'

# Create output data frame
psandeels = data.frame(Year = unique(ray.inds$Year))
psandeels$percentage = NA

# Calculate the percentage of sandeels in each stomach in each year
for(i in 1:length(ray.years)) {
  
  # Subset data by fish
  fish = unique(ray.years[[i]]$Predator.ID)
  
  # Create temporary vector to store each individual's sandeel percentage
  ptemp = vector(length = length(fish))
  
  # For each unique fish, get percentage of sandeels
  for(j in 1:length(fish)) {
    
    # Get data for year i and fish j
    tempdat = ray.years[[i]][ray.years[[i]]$Predator.ID == fish[j], ]
    
    # Get number of items
    nitems = nrow(tempdat)
    
    # Get number of sandeels
    nsandeels = nrow(tempdat[tempdat$Prey.common.name == "SANDEELS", ])
    
    # Calculate percentage
    ptemp[j] = 100 * (nsandeels / nitems)
  }
  
  # Store mean percentage sandeels for all j fish in each year i
  psandeels$percentage[i] = mean(ptemp)
}
psandeels


########## VII. Function-ize it ##########

# Does the percentage sandeels in blonde ray stomachs change over time?

# Get percentage of any prey item
# x: a stomach contents data frame
# item: user decides which prey item of which to calculate percentage
# returns a vector with percentage of item in each unique fish's stomach
percent_item = function(x, item) {
  
  # Get a vector with the name of each fish
  fish = unique(x$Predator.ID)
  
  # Create new vector to house percentage item data
  pitem = vector(length = length(fish))
  
  # Loop through each fish to get the percentage of sandeels in each stomach
  for(i in 1:length(fish)) {
    
    # Look at the i^th fish
    tempdat = x[x$Predator.ID == fish[i], ]
    
    # How many items are in its stomach?
    nitems = nrow(tempdat)
    
    # How many of those items are the user's chosen item?
    nuser = nrow(tempdat[tempdat$Prey.common.name == item, ])
    
    # Calculate percentage
    pitem[i] = 100 * (nuser / nitems)
  }
  
  # Return the vector with percentage of item
  return(mean(pitem))
}

# Apply this function to each year
( sandeels.perc = lapply(ray.years, percent_item, item = "SANDEELS") )


########## VIII. BONUS - simulation ##########

# Look at simulation functions
?rnorm
?rpois
?rbinom

# Simulate some basic data
normSim = rnorm(n = 20, mean = 5, sd = 2)
poisSim = rpois(n = 20, lambda = 10)
binSim = rbinom(n = 20, size = 1, prob = 0.5)

# Plot the normal data
plot(normSim)
hist(normSim)
boxplot(normSim)

# Plot the poisson data
plot(poisSim)
hist(poisSim)
boxplot(poisSim)

# Plot the binomial data
plot(binSim)
hist(binSim)
boxplot(binSim)

# Make life more fun; add some slope to your data
m = 2
b = 1
x = seq(from= -5, to = 5, by = 0.2)
y = rnorm(n = length(x), mean = m*x + b, sd = 2)

# Plot the outcome
plot(y ~ x)

# Run a linear model to see if the results are as expected
xy.lm = lm(y ~ x)
summary(xy.lm)