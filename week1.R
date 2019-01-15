# Developing the "Score" for Data Analysis
# reproducibility
# a score -- a book that lists every single part of every instrument, 
## and what they need to play, and what instructions they have.
# a Score -- how something should look like
# how do you develop the score for data analysis
## to communicate to someone what was done and how to do it to reproduce them.
# nothing->reproducibility(bridge the gap)->replication
# validation of the data analysis

# Internet-based Health and Air Pollution Surveillance System (iHAPSS)
## https://www.healtheffects.org/publication/internet-based-health-and-air-pollution-surveillance-system

# Reproducible Research in Computational Science R. Peng article

# The Duke Saga
## https://simplystatistics.org/2011/09/11/the-duke-saga/

# What do we need?
## Analytic data
## Analytic code
## Documentation of code and data
## Standard means of distribution

# Literate (Statistical) Programming [umiejętne programowanie]
## text is readable by people
## code is readable by computers {Sweave used a documentation program called LaTeX}
## a series of text and code chunks
## LaTeX -- it lack a lot of features like caching, multiple plots per page and mixing programming languages
## knitr (an alternative to Sweave in R), can use variety of documentation languages:
### LaTeX, Markdown, HTML
install.packages("knitr")
library(knitr)
ls("package:knitr") # 105 functions
install.packages("xtable")
library(xtable)
ls("package:xtable") # 29 functions

# Structure of a Data Analysis 
## Dan Myer

# Our example from Google data centers have some very high security
## https://www.google.com/about/datacenters/inside/

# We use Machine Learning Repository
## http://archive.ics.uci.edu/ml/datasets/Spambase

# URL and the time and date that you access that.

# D
## http://search.r-project.org/library/kernlab/html/spam.html
## if data are from survey you need to know how the sampling was done!

library(kernlab)
ls("package:kernlab") # 89 functions
?spam
data(spam) # cleaned up
str(spam[,1:5])

# Steps in a data analysis
## Define the questio
## Define the ideal data set
## Determine what data you can access
## Obtain the data
## Clean the data

## Exploratory data analysis
## Statistical prediction/modeling
## Interpret results
## Challenge redults
## Synthesize/write up results
## Create reproducible code

## Perform the subsampling (flip coin)
set.seed(3435)
trainIndicator=rbinom(4601,size = 1,prob = 0.5)
table(trainIndicator)
trainSpam=spam[trainIndicator==1,]
testSpam=spam[trainIndicator==0,]

## EDA -- one dimensional, two dimensional summaries of the data
names(trainSpam)
head(trainSpam)
table(trainSpam$type)
plot(log10(trainSpam$capitalAve+1)~trainSpam$type)
### conclusion: capital letters are occurring much higher in spam emails
plot(log10(trainSpam[,1:4]+1))
hCluster=hclust(dist(t(trainSpam[,1:57])))
plot(hCluster)
hClusterUpdated=hclust(dist(t(log10(trainSpam[,1:55]+1))))
plot(hClusterUpdated)
### conclusion: capitalAve is separete cluster and you,will,your are separate cluster

## Statistical modelling
trainSpam$numType<-as.numeric(trainSpam$type)-1
costFunction=function(x,y) sum(x!=(y>0.5))
cvError=rep(NA,55) # cross validated error rate of predicting spam emails from a single variable
library(boot)
### logistic regression by single variable using for loop
for(i in 1:55) {
  lmFormula=reformulate(names(trainSpam)[i],response = "numType")
  glmFit=glm(lmFormula,family = "binomial",data = trainSpam)
  cvError[i]=cv.glm(trainSpam,glmFit,costFunction,2)$delta[2]
}
### which predictor has minimum cross-validated error?
names(trainSpam)[which.min(cvError)] # charDollar -- $; dollar sign
### each of these models only have a single predictor in it
### Use the best model from the group (logistic regression model)
predictionModel=glm(numType~charDollar,family="binomial",data = trainSpam)
### Get predictions on the test set
predictionTest=predict(predictionModel,testSpam)
predictedSpam=rep("nonspam",dim(testSpam)[1])
### Classify as 'spam' for those with prob > 0.5
predictedSpam[predictionModel$fitted > 0.5] = 'spam'
### Classification table
table(predictedSpam,testSpam$type)
### Error rate
(61+458)/(1346+458+61+449) # 22 %
### conclusions: 61 classify as spam but they were not and 458 spam uncorrectly classify as non spam.