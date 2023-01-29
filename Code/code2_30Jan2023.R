# Lecture 2 - Introduction to R

# Created: 29 January, 2023
# Last modified: 29 January, 2023 by EPD

# Set working directory
setwd("/Users/epdus/OneDrive/Breathless/Teaching/EFB796/")

# Load packages
library(readxl)

# Contents (ctrl-f):
# I. Basics
# II. Read in data
# III. Data types


########## I. Directories ##########

# Show current working directory
getwd()

# Change working directory
setwd("/Users/epdus/OneDrive/Breathless/")
getwd()
setwd("/Users/epdus/OneDrive/Breathless/Teaching/EFB796/")

# List files in current directory
list.files()

# Create new directory (i.e. folder) within current directory
dir.create("new_dir")

# Create new directory one level up
dir.create("../new_dir")

# Create new directories two levels up
dir.create("../../new_dir")


########## II. Read in data ##########

# Read in a text file
df.txt = read.table("data/code1_df.txt", header = T)

# Read in a file with comma separated values (CSV)
df.csv = read.csv("Data/code1_df.csv")
df.csv = read.table("Data/code1_df.csv", sep = ",", header = T)

# Read in an excel file
df.xl = read_excel("Data/code1_df.xlsx")

# Write data to a new file
write.table(df.txt, "Data/mydata.txt")


########## III. Data types ##########

# Number
( x = 21 )
( y = 2/3 )
( z = sqrt(2) )

# String
( color = "blue" )

# Vector
( values = c(1,3,5,7,11) )

# Array
( x.arr = array(dim = c(3,3,2)) )

# Data frame
( x.df = data.frame(stuff = c(1,2,3), things = c("lamp","spider","coaster")) )

# List
( x.list = list(numbers = c(2,3), strings = c("yes","no","maybe")) )


########## IV. Calculations and variables ##########

# Use R as a calculator
2+2
8.5 * sin(2)/pi
2^12

# Declare variables
A = 2 + 2
a = 2^4
b = exp(a)
c = log(b * a)

# Case sensitivity
A
a

# Functions
sin(0)
cos(0)
tan(0)
log(10)
log(10, base = 10)
exp(2)
sqrt(4)

# Remember PEMDAS
2 + (3*2)
(2+3) *2
x = 3
y = (x + 2*sqrt(x)) / (x - 5*sqrt(x))
y
z = x + 2*sqrt(x) / x - 5*sqrt(x)


########## V. Vectors ##########

# Concatenate
( a = c(1,2,3) )
b = 2; ( a = c(1,b,3) )

# Combining data types
( a = c("cat", 10, "mouse") )

# Sequences
( a = c(1:10) )
( a = seq(from = 0, to = 15, by = 3) )
( a = seq(1, 15, 3) )
( a = seq(from = 0, to = 2*pi, by = pi/2) )
( a = seq(1:100, 10) ) # Error

# Using the rep (i.e. repetition) command
( a = rep(1, times = 5) )
( a = rep(c(1,3), times=5) )
( a = rep(c(1:3), times=5) )
( a = rep(c(1:3), each = 5) )

# How many elements are in your vector?
length(a)


########## VI. Matrices ##########

# Combining columns (cbind)
a = c(1:5)
b = c(6:10)
( C = cbind(a,b) )

# Combining rows (rbind)
a = c(1:5)
b = c(6:10)
( C = rbind(a,b) )

# Accessing elements of a matrix
C
C[1,2]
C[2,5]
C[3,2] # Error

# What are the dimensions of your matrix? (# rows, # columns)
C
dim(C)
nrow(C)
ncol(C)


########## VII. Data frames ##########

# Data frames are basically R's version of Excel files
# They combine multiple data types into columns of values
pet = c("cat", "dog", "dog", "hamster")
petName = c("Felix", "Fido", "Missy", "Spotty")
petWeight = c(5, 45, 30, 0.3)
( MyPets = data.frame(PetType = pet, Name = petName, Weight = petWeight) )


########## VIII. Functions ##########

# Simple user-defined function
AreaOfCircle = function(r) pi * r^2
AreaOfCircle(10)

# More complex functions
Sphere <- function(radius) { 
  volume = 4/3 * pi * radius^3 
  surface = 4 * pi * radius^2
  return(list(volume=volume, surface=surface)) 
}
Sphere(10)

# Functions with conditional statements
Dummy = function (x) {
  if (x<0) {
    string = "x < 0" 
  } else if (x<2) {
    string = "0 >= x < 2"
  } else {
    string = "x >= 2" 
  }
  print(string) 
}
Dummy(2)
Dummy(-1)
Dummy(1)


########## IX. Loops ##########

# Simple loop
for (i in 1:10) print(i)

# More complex loops
c = vector(length = 10)
for(i in 1:length(c)) {
  a = i*pi
  b = i^2
  c[i] = ifelse(a < b, b, a)
}
c


########## X. Packages ##########

# See all current packages
library()

# Load a packages
library(deSolve)

# Get help with a package
library(help = deSolve)
help(package = deSolve)


########## XI. Plotting ##########

# Use the built-in data set Orange (there are many built-in datasets)
head(Orange)

# Simple scatterplot
plot(circumference ~ age, data = Orange)

# Prettier scatterplot
plot(x = Orange$age, # x-axis value
     y = Orange$circumference,# y-axis value
     xlab= "Age in days", # x-axis label
     ylab= "Trunk circumference, mm", # y-axis label
     pch = 17, # Shape of individual points
     col = "lightblue" # Color of points
)

# See variables in the data frame
Orange$age