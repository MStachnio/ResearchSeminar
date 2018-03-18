# Date: 17.03.2018
# Authors: Michal Stachnio & Jérôme Voelke

# To-Do log:
# 1) add input variables
# 2) Create the "Testing" part

#Up-Date Version 2.0
# 1) I added a buy signal attribute variables which takes the value 1 when the 5 day MA cross the 10 day MA
# 2) I added a risk attribute varibales which is the change in percentage in comparaison to the previous day for the principale risk component
# 3) The weight for the buy signal is positive and negative for risk which appears to be in line with the theory.
# 4) The most urgent task is to determine how good our model is in terme of prediction

# Potential Problems
# Signal outputs 1 when comparing NA.




# Personal notes: 
# 1) What's nice about logistic regression is that we can decide to invest only on those days we have 99.9% certainty. 
# 2) We need to make sure there is no tail risk. i.e. if you invest with 99.9% certainty and you earn only 1.- but with 0.01% chance you loose 1m.-, it's not great

#
#
#         Part 1: Creating the dataFrame
#-----------------------------------------------------------------------------------------------------

#setwd("~/Desktop/MBF Second Semester/Research Seminar/Regression")

#install.packages("TTR")



library(xts)
library(zoo)
library(ggplot2)
library(TTR) #Technical Trading Rule
library(gridExtra) #Allow to place 2 graphs on the same grids
library(tseries)



# Opening the core data file
csvFilePathCoreData = ("~/Desktop/MBF Second Semester/Research Seminar/Regression/New_Data.csv")
coreData = read.csv(file = csvFilePathCoreData, # path of the file
                    header = TRUE, # include the headers
                    sep = ",") # How the data is separated
coreData[coreData == ""] <- NA # replace all empty spaces with NA

# OPTIONAL: Truncating the size for testing
coreData = coreData[200:1000, c('Time', 'SP_500', 'SP_500_Volume','P1_risk')] #Missing value in 2010-2011 => start in 2012

# Create a Timeseries of tedRate indexed by date
coreData$Time = as.Date(coreData$Time, format = "%d.%m.%Y") # Bug: output pour les années c'est 0010 à la place de 2010.
coreData = xts(coreData[, 2:4], order.by = coreData$Time)


#Graph of the SP 500
ggplot(coreData, aes(x=time(coreData))) + #aes() creates a mapping of variables to various parts of the plot
  geom_line(aes(y=coreData$SP_500,colour = "SP_500")) +
  labs(x="Time ", y="") + #no axes labels
  scale_color_discrete(name="Legend") 



#-----------------------------------------------------------------------------------------------------
#         Part 2: Data Manipulation
##-----------------------------------------------------------------------------------------------------

# The following Variables are calculated:
# 1) spEvolution            = Vector of 1 and 0s depending on whether the S&P500 index increased (1) or decreased (0)
# 2) spSimpleMoving Average = Simple Moving Average over 10 days
# 3) spWeightedAverage      = Weighted Average over 10 days
# ...


# Creating the S&P evolution vector, a vector with binary data (1 if the index value increases, 0 otherwise)
# Personal ToDo: if I have time I could define this as a function
BinominalEvolution = function (x){
  for (i in 1:length(x)) {
    if (i == length(x)) { # When we get to the last data entry, the output is always NA because we cannot compare it to a future value. Without this, there is an error message
      x[i, ] = NA
    } else if (is.na(x[i+1, ]) || is.na(x[i, ])) { # If comparing with a NA, the output is automatically NA.
      x[i, ] = NA
    } else if (coredata(x[i+1, ]) > coredata(x[i, ])) {
      x[i, ] = 1
    } else {
      x[i, ] = 0
    }
  }
  return (x)
}
spEvolution = BinominalEvolution(coreData$SP_500)
colnames(spEvolution) = "spEvolution"

# Calculating the simple 10-day moving average of the S&P500 index
spSimpleMovingAverage = SMA(coreData$SP_500, 10)
colnames(spSimpleMovingAverage) = "spSimpleMovingAverage"

# Calculating the weighted 10-day moving average of the S&P 500 index
# We need to define a function for this purpose
WeightedMovingAverageTS = function (x, n) { # The function takes two inputs: x = the timeseries that is weighted, n = the amount of days of the weighting
  divisor = 0
  y = x
  for (q in 1:n) { # This part calculates the divisor (i.e. the denominator of the weighted sum)
    divisor = divisor + q
  }
  for (l in 1:length(x)) { # This part calculates the weighted sum
    weightedSum = 0
    if (l < n) {
      y[l, ] = NA
    } else {
      for (i in 1:n) {
        weightedSum = weightedSum + coredata(x[l - i + 1]) * (n - i + 1)
      }
      y[l, ] = weightedSum
    }
  }
  return (y/divisor) # The function outputs a weighted timeseries
}
spWeightedAverage = WeightedMovingAverageTS(coreData$SP_500, 10)
colnames(spWeightedAverage) = "spWeightedAverage"

#Create the Buy Signal and Sell Signal 
coreData$SP_500_SMA_5 = SMA(coreData$SP_500, n = 5) # 5 Day Moving Average
coreData$SP_500_SMA_10 = SMA(coreData$SP_500, n = 10) # 10 Day Moving Average
SMA_5 = (coreData$SP_500_SMA_5)
SMA_10 = (coreData$SP_500_SMA_10)
Diff_SMA = SMA_5 - SMA_10 
colnames(Diff_SMA) = "Diff_SMA"
Diff_SMA <- Diff_SMA[!is.na((Diff_SMA[,1]))] #remove the N.A from the XTS object
Signal_SMA = matrix(0, length(Diff_SMA), 1)
# if the line of short moving average cross the line of the longer moving average, this can be interpretated as a buy signal => Signal =1 
for (i in 1:length(Signal_SMA)){
  if (Diff_SMA[i] > 0){
    Signal_SMA[i] = 1 
  } else {
    Signal_SMA[i] = 0 
  }
}
Signal = xts(Signal_SMA, order.by = time(Diff_SMA)) # create the XTS object

#Recombine the data and remove the NA:
nrow(Data)
Data = merge(coreData$SP_500, spEvolution, SMA_5, SMA_10, Diff_SMA, Signal)
Data = Data[!is.na((Data[, 6]))] #remove the N.A from the XTS object
colnames(Data) = cbind("SP_500","Evolution", "SMA_5", "SMA_10", "Diff", "Signal") 

#Plot the Moving Average (5 and 10), the difference, and the signal 
plot1 = ggplot(Data, aes(x = time(Data))) + #aes() creates a mapping of variables to various parts of the plot
  geom_line(aes(y = Data$SMA_5, colour = "SP_500_SMA_5")) +
  geom_line(aes(y = Data$SMA_10, colour = "SP_500_SMA_10")) +
  labs(x = "Time", y = "") + #no axes labels
  scale_color_discrete(name = "Legend") 

plot2 = ggplot(Data, aes(x = time(Data))) + #aes() creates a mapping of variables to various parts of the plot
  geom_line(aes(y = Data$Diff, colour = "Difference")) +
  labs(x = "Time", y = "") + #no axes labels
  scale_color_discrete(name = "Legend") 

plot3 = ggplot(Data, aes(x = time(Data))) + #aes() creates a mapping of variables to various parts of the plot
  geom_line(aes(y = Data$Signal, colour = "Buy_Sell")) +
  labs(x = "Time", y = "") + #no axes labels
  scale_color_discrete(name = "Legend") 

grid.arrange(plot1, plot2, plot3, nrow = 3, ncol = 1)  #2 graphes sur 2 lignes et une colone


#Create the attribute variable risk which is the princpal component of the risk as showed by the CDS of different bank
risk = coreData$P1_risk

ggplot(risk, aes(x = time(risk))) + #aes() creates a mapping of variables to various parts of the plot
  geom_line(aes(y = risk,colour = "Risk")) +
  labs(x = "Time ", y = "") + #no axes labels
  scale_color_discrete(name = "Legend") 

#Calculte the change in the risk (in percentage)
change_risk = diff(risk)/lag(risk)
change_risk = lag(change_risk, k = -1) #too proceed in the same way as the calculation of the SPEvolution
                                    # this means that if the change between t and t+1 is postive, this value will be indexed to t (and not to t+1)

Data$change_risk = change_risk
Data = Data[!is.na((Data[, 6]))] #remove the N.A from the XTS object


#Create the attribute variables MA-10:






#Create the vector of Attribute
Attribute = (cbind(Data$Signal, Data$change_risk))


#-----------------------------------------------------------------------------------------------------
#         Part 3: Logistical Regression
#-----------------------------------------------------------------------------------------------------


# Initial parameters
randomWeight = 0.00005 # The value of the initial weights. note: we can randomize this later on
attributeVariables = Attribute # matrix(rnorm(numberObservations * numberAttributeVariables, mean = 0, sd = 1), nrow = numberAttributeVariables, ncol = numberObservations)# !!!!!!!!! Need to define this from the data !!!!!!!!!!
resultVector = Data$Evolution # rbinom(10, 1, 0.5) # !!!!!!!!! Need to define this from the data !!!!!!!!!! => This the vector of return of the SP&500
outerloop = 1 # How many times we go over the data
eta = 0.1 # error adaptation coefficient
flagPerceptronLearning = 0 # 0 if using an ADALINE-like learning algorithm, 1 if using an PERCEPTRON-like learning algorithm.


# Creating the x_0 vector
x_0 = xts(rep(1, length(Data$Evolution)), order.by = index(Data))

# Merging all the data into the dataFrame
dataSet = merge(x_0, attributeVariables, resultVector)

# VERY IMPORTANT: TAKING OUT ALL THE ROWS WITH NAs
dataSet = dataSet[complete.cases(dataSet), ]

# Key variables for calculations
numberAttributeVariables = ncol(dataSet) - 2 #the number of coloum in data Set is 4 and we have two attriubute variables 
numberObservations = nrow(dataSet)

# Creating the weight vector
weightVector = matrix(randomWeight, # define the vector of weights
                      nrow = numberAttributeVariables+1, # we Create a weight for each dependent variable, plus one for w0.
                      ncol= 1,
                      byrow = TRUE
)

# naming the rows in weightVector
weightRowNames = matrix("0", nrow = numberAttributeVariables + 1, ncol = 1, byrow = TRUE)
for (i in 1:nrow(weightRowNames)) {
  weightRowNames[i, ] = paste("w", i - 1, 
                              sep = "") # sep="" means that there is no space between w and the number
}
rownames(weightVector) = c(weightRowNames)




# Defining the key variables for the Logistic Regression Algorithm
index = matrix(0, 1, numberObservations * outerloop)
indexPrediction = 0 
weightMatrix = matrix(NA, nrow(weightVector),  numberObservations * outerloop) # WeightMatrix is used to record the weights through the various iterations.
weightMatrix[, 1] = weightVector
rownames(weightMatrix) = c(weightRowNames)
error = 0
predictionAccuracy = matrix(NA, 1,  numberObservations * outerloop)
lastError = 0 
l = 1
i = 1

# Core Algorithm: reweighting based on observation data
for (l in 1 : outerloop) {
  for (i in 1 : numberObservations) {
    weightMatrix [,i + (l - 1) * numberObservations] = weightVector
    index[i] = 1/(1 + exp(-(sum(coredata(dataSet[i,0 : numberAttributeVariables + 1]) %*% weightVector )))) 
    if (index[i] < 0.5) {
      indexPrediction[i] = 0
    } else {indexPrediction[i] = 1
    }
    error[i] = dataSet[i, numberAttributeVariables + 2] - flagPerceptronLearning * indexPrediction[i] + (-1 + flagPerceptronLearning) * index[i] # flagPerceptronLearning defines whether we use an ADALINE or Perceptron-like algorithm
    # Also tests whether our prediction was accurate and stores it in predictionAccuracy
    if ((index[i] < 0.5) & (dataSet[i, numberAttributeVariables + 2] < 0.5) | (( index[i]) >= 0.5) & (dataSet[i, numberAttributeVariables + 2] >= 0.5)) {
      predictionAccuracy[, i + (l - 1) * numberObservations] = 1
    } else {
      predictionAccuracy[, i + (l - 1) * numberObservations] = 0
      lastError = i + (l - 1) * numberObservations
    }
    weightVector =  weightVector + eta * error[i] * index[i] * (1 - index[i]) * as.vector(dataSet[i, 0 : numberAttributeVariables + 1]) #Update
  }
}

weightVector