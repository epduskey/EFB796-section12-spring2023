# Homework 7 - due 27 Mar 2023

# Set working directory (if not using R Project)
setwd("/Users/epdus/OneDrive/Breathless/Teaching/EFB796")

# Contents (ctrl-f):
# I. Load data
# II. Bubble plot
# III. Cohort analysis


########## I. Load data ##########

# Read in the three halibut data frames.  These data come from 
# Table 8.16 in Quinn and Deriso's (1999) Quantitative Fish Dynamics.
# The book is available from Moon Library either as a pdf or
# a physical copy.  I also have a copy, if you would like to borrow it.
halibut = 
lh = 
wt = 

# Use whatever functions you'd like to look at the data


# Columns marked as "a#" denote halibut of age #.  The "Et" column gives
# effort (number of skates i.e. 1800 feet of ground line with 100 equally
# spaced hooks).  If we wanted to do VPA on a given year class, how would
# we extract that data from the halibut data frame?
#
#


########## II. Bubble plot ##########

# This shows you how to replicate the plot from the lecture

# Re-arrange data for bubble plot
bubble = data.frame(N = c(as.matrix(halibut[,2:14])))
bubble$Year = halibut$Year
bubble$Age = rep(seq(8,20),each=length(halibut$Year))

# Make sure "size" is in aes, and use "scale_size()" to make the bubbles
# The "alpha" arguments makes the fill color transparent
ggplot(bubble, aes(x=Age,y=Year,size=N)) +
  scale_x_continuous(name = "Age", breaks = seq(8,20), labels = seq(8,20)) +
  geom_point(alpha = 0.6) +
  scale_size(name = "Population (N)", range = c(0.1,10)) +
  theme_bw() +
  theme(text = element_text(size=15)) +
  ggtitle("Halibut")


########## III. Cohort analysis ##########

# Put the halibut data in matrix form. HINT: Each row will be
# a year and each column will be an age.  It will be 16x13
hmat = 

# Extract the cohorts from the matrix. The code below splits the
# matrix you created above into diagonals. You should have 28 cohorts.
# Each one is named for the year the individuals were born.  We subtract
# 8 because we only have data for ages 8+
ind_diag = row(hmat) - col(hmat)
cohorts = split(hmat, ind_diag)
names(cohorts) = 1955:1982 - 8

# Copy the cohorts matrix structure to store population and fishing mortality estimates
N = 
fmort = 

# Last year's fishing mortality for each cohort (I also copied these
# from the Quinn and Deriso book, Table 8.18)
FA = c(0.277, 0.241, 0.322, 0.340, 0.293, 0.269, 0.246, 
       0.167, 0.207, 0.197, 0.147, 0.123, 0.118, 0.129, 
       0.120, 0.110, 0.110, 0.110, 0.110, 0.110, 0.110, 
       0.102, 0.089, 0.084, 0.065, 0.052, 0.035, 0.020)

# Set the last element of each cohort's fishing mortality to the provided values
for(i in 1:length(FA)) fmort[[i]][length(fmort[[i]])] = FA[i]

# Notice that natural mortality is assumed to be 0.2 for all ages
lh
MA = 0.2

# Write a function to estimate the last year's population size for each cohort
# using John Gulland's method (check code8_20Mar2023.R for first step in the method).
# I've got your started here with your arguments and function name.
gulland = function(Fmort, Mmort, catch) {
}

# Apply function to every cohort using either a loop or an apply function
# to get estimated population size in the last year's population size
# for each cohort
N.last = 

# Store in last element of each vector in the N estimates list


# Pope's backwards equation (he's got a forwards one too) can be used to 
# sequentially approximate population estimates.  It can be written as follows:
#
# N[a] = N[a+1] * exp(M[a]) + C[a] * exp(M[a]/2)
#
# where a is age, C is catch, and M is natural mortality.  Fishing mortality
# is approximated by the following equation:
#
# F[a] = -log(N[a+1]/N[a]) - M[a]
#
# Write function(s) that work backwards from the estimated values in N.last 
# to estimate population size for each cohort. You can write two
# functions, one that estimates N and another that calculates fishing
# mortality estimates from N, or you can write one that returns both,
# or that returns one or the other depending on what the user asks for.
# HINT: The function will need a loop!
pope = function() {
}

# Apply function to each cohort (that needs it! the ones with only one element don't)


# Re-structure funky lists into matrix form.  You can unsplit your split stuff!  Very handy.
N_mat = matrix(unsplit(N, ind_diag), nrow = nrow(hmat), ncol = ncol(hmat))
F_mat = matrix(unsplit(fmort, ind_diag), nrow = nrow(hmat), ncol = ncol(hmat))

# Sum N_mat by rows using the rowSums function and plot as a function of year


# Use matplot to look at fishing mortality for each age class over time
# Remember that matplot goes by columns
matplot(F_mat, type = "l")

# How does it look like the halibut fishery was doing in the late 60s through early 80s?
#
#