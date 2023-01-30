# Homework 2 - due 6 February 2023

# Contents (ctrl-f):
# I. Set working directory
# II. Load data
# III. Examine data
# IV. Manipulate data


########## I. Set working directory ##########

# See your current directory
getwd()

# Set your preferred working directory
setwd("/Users/epdus/OneDrive/Breathless/Teaching/EFB796")


########## II. Load data ##########

# Go to a repository of your choice, copy the url of the 
# download link, and load it here. Pay attention to the file 
# extension of the data set to choose the correct loading function
mydata = read.csv("https://zenodo.org/record/7502556/files/PrairieDogData_Dryad.csv?download=1")


########## III. Manipulate data ##########

# Use a couple of functions of your choice to examine the data here
head(mydata)
str(mydata)

# Use a few plots to look at various values
hist(mydata$Weight.g.)


########## IV. Manipulate data ##########

# Add a new column to your data
mydata$newColumn = mydata$Weight.g./1000

# Re-examine your data set here to see your new column
head(mydata)

# Subset your data to look at just one year/species/category
subdata = mydata[mydata$Sex == "F", ]

# Add another EMPTY column to the data subset
subdata$emptyColumn = NA

# Use a for loop to populate the new column with some values
for(i in 1:nrow(subdata)) {
  subdata$emptyColumn[i] = mydata$Weight.g.[i]/1000
}
head(subdata)

# Use an ifelse statement to make yet another column
subdata$yaColumn = ifelse(subdata$newColumn > 0.01, "big", "small")
head(subdata)


########## IV. Analyze data ##########

# Ask a (perhaps deceptively) simple question about your data here:
# (for example) are adult female prairie dogs bigger than adult males?

# There are many ways to do this, but I will create two new
# data sets, one for adult females and one for adult males
females = subset(mydata, Sex == "F" & Age == "A")
males = subset(mydata, Sex == "M" & Age == "A")

# Calculate mean weight of both
mean(females$Weight.g.)
mean(males$Weight.g.)

# Now write a function to help you answer your question
# x: a data frame containing sex and weight information
# sex: a string designating each individual as male "M" or female "F"
myMeanWeight = function(x, sex) {
  
  # Subset your data by user's chosen sex
  dat = x[x$Sex == sex & x$Age == "A", ]
  
  # Calculate the mean weight
  meanWeight = sum(dat$Weight.g.) / nrow(dat)
  
  return(meanWeight)
}

# Try your function
myMeanWeight(mydata, "F")
myMeanWeight(mydata, "M")