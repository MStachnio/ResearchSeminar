# Notes --------------------------------------------------------------------------------------------------------------
# Date: 17.03.2018
# Authors: Michal Stachnio & Jérôme Voelke

#To-Do log:
# 1) add input variables => line 446 NEED to be modified to account for the new variables
# 2) Create the "Testing" part
# 3) Do we need to normalize change_risk ?
# 4) How should we use the RSI ?
# 5) SMAs are not normally distributed.
# 6) MACD outputs two columns. I assumed we need only the first one and did this change in part 2
# 7) we would need to deduct the risk free rate to all return variables.

#Up-Date Version 2.0
# 1) I added a buy signal attribute variables which takes the value 1 when the 5 day MA cross the 10 day MA
# 2) I added a risk attribute varibales which is the change in percentage in comparaison to the previous day for the principale risk component
# 3) The weight for the buy signal is positive and negative for risk which appears to be in line with the theory.
# 4) The most urgent task is to determine how good our model is in terme of prediction

# Notes concerning standardizing:
# In order to standardize our data we can either - A) Standardize B) Normalize 
# https://machinelearningmastery.com/normalize-standardize-time-series-data-python/
# https://vitalflux.com/data-science-scale-normalize-numeric-data-using-r/

# Personal notes: 
# 1) What's nice about logistic regression is that we can decide to invest only on those days we have 99.9% certainty. 
# 2) We need to make sure there is no tail risk. i.e. if you invest with 99.9% certainty and you earn only 1.- but with 0.01% chance you loose 1m.-, it's not great

# Importing Libraries --------------------------------------------------------------------------------------------------------------

library(xts)
library(zoo)
library(ggplot2)
library(TTR) #Technical Trading Rule
library(gridExtra) #Allow to place 2 graphs on the same grids
library(tseries)



# Part 1: Creating the dataFrame-----------------------------------------------------------------------------------------------------

#setwd("~/Desktop/MBF Second Semester/Research Seminar/Regression")
#setwd("~/Users/Michal/Dropbox/UNISG/16. Research Seminar/New_Data.csv")
#install.packages("TTR")

# Opening the core data file
# Michal Path: csvFilePathCoreData = "/Users/Michal/Dropbox/UNISG/16. Research Seminar/4. Data/New_Data.csv"
csvFilePathCoreData = "/Users/Michal/Dropbox/UNISG/16. Research Seminar/4. Data/New_Data.csv"
coreData = read.csv(file = csvFilePathCoreData, # path of the file
                    header = TRUE, # include the headers
                    sep = ",") # How the data is separated
coreData[coreData == ""] <- NA # replace all empty spaces with NA

# OPTIONAL: Truncating the size for testing
coreData = coreData[200:nrow(coreData), c('Time', 'SP_500', 'SP_500_Volume','P1_risk','Gold','Vix')] #Missing value in 2010-2011 => start in 2012

# Create a Timeseries of coreData indexed by date
coreData$Time = as.Date(coreData$Time, format = "%d.%m.%Y") # Bug: output pour les années c'est 0010 à la place de 2010.
coreData = xts(coreData[, 2:ncol(coreData)], order.by = coreData$Time)


# Part 2a: Data Manipulation -----------------------------------------------------------------------------------------------------

# Defining usefull functions:
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

normalize <- function(x) {
  return ((x - min(x))/(max(x) - min(x)))
}

# The following Variables are calculated:
# 1) spEvolution                          = Vector of 1 and 0s depending on whether the S&P500 index increased (1) or decreased (0)
# 2) SP_WMA_10_return                     = Weighted Average over 10 days
# 3) SP_SMA_5_return, SP_SMA_10_return    = Simple Moving Average over 10 days and 5 days
# 4) Signal                               = Difference between 5 days moving average and 10 day (=1 when the MA(5) is over the MA(10, else =0 ))
# 5) Momentum_5                           = Calculate the rate of change in percent of a series over n periods. (here 5 days)
# 6) RSI_15                               = The Relative Strength Index (RSI) calculates a ratio of the recent upward price movements to the absolute price movement.
# 7) MACD                                 = Moving Average Convergence Divergence (The MACD function either subtracts the fast MA from the slow MA, or finds the rate of change between the fast MA and the slow MA. => The one I used => return percentage Time periods for the MACD are often given as 26 and 12, When the MACD is above the 9 day line => Buy Signal, When the MACD is below te 9 day line => Sell Signal Or When the MACD is above zero, the short-term average is above the long-term average, which signals upward momentum. The opposite is true when the MACD is below zero source: https://www.investopedia.com/terms/m/macd.asp
# 8) CCI_15                               = Commodity Channel Index 
# 9) change_risk                          = Calculte the change in the risk (in percentage). Princpale component out of the CDS of JPM, BOFA, CITI. 
# 10) change_gold                         = change in gold commodity price            
# 11) change_VIX                          = change Vix variable

# 1) Creating the S&P evolution vector, a vector with binary data (1 if the index value increases, 0 otherwise)
spEvolution = BinominalEvolution(coreData$SP_500)
colnames(spEvolution) = "spEvolution"
#Graph of the SP 500
ggplot(coreData, aes(x=time(coreData))) + #aes() creates a mapping of variables to various parts of the plot
  geom_line(aes(y=coreData$SP_500,colour = "SP_500")) +
  labs(x="Time ", y="") + #no axes labels
  scale_color_discrete(name="Legend") 

# 2)  Calculating the weighted 10-day moving average of the S&P 500 index
SP_WMA_10 = WeightedMovingAverageTS(coreData$SP_500, 10)
colnames(SP_WMA_10) = "SP_WMA_10"
#Normalize
SP_WMA_10_return = 100 * diff(log(SP_WMA_10))
colnames(SP_WMA_10_return) = "SP_WMA_10_return"
#SP_WMA_10_return = WMA_10_return[!is.na((SP_WMA_10_return[, 1]))]
#SP_WMA_10_Normalized = normalize(SP_WMA_10_return)
hist <- ggplot(SP_WMA_10_return, aes(x = SP_WMA_10_return)) +
  geom_histogram(aes(y = ..density..), bins = 20,
                 color = "black", fill = "white")

# 3) Calculating the simple moving average 10-day and 5-days of the S&P500 index
SP_SMA_5 = SMA(coreData$SP_500, 5)
SP_SMA_10 = SMA(coreData$SP_500, 10)
colnames(SP_SMA_10) = "SP_SMA_10"





#We can Standardize or Normalize. Standardizing requires Normal Distribution
SP_SMA_5_return = 100 * diff(log(SP_SMA_5))
SP_SMA_10_return = 100 * diff(log(SP_SMA_10))
colnames(SMA_10_return) = "SP_SMA_10_return"


# jarque.bera.test p-value is equal 0, this means that we can reject H0: returns are normally distributed 
# The contiunous return of the MA_10 are not normaly distributed => Cannot use the Standardization 
jarque.bera.test(!is.na(SP_SMA_5_return))
jarque.bera.test(!is.na(SP_SMA_10_return))
#Ploting
hist <- ggplot(SP_SMA_10_return, aes(x = SP_SMA_10_return)) +
  geom_histogram(aes(y = ..density..), bins = 20,
                 color = "black", fill = "white")
hist <- hist + stat_function(fun = dnorm, args = list(mean(SP_SMA_10_return), sd(SP_SMA_10_return)), size = 1.2) #stat_function() makes it easy to superimpose a function (here dnorm) on top of an existing plot
# Normalizing (note: not ideal, "if your time series is trending up or down, estimating these expected values may be difficult and normalization may not be the best method to use on your problem." But one can try to normalize the retrun of this times series.
SP_SMA_5_Normalized = normalize(SP_SMA_5_return)
SP_SMA_10_Normalized = normalize(SP_SMA_10_return)

# 4) Create Signal (i.e. if 5 days SMA crosses 10 days SMA) 
Diff_SMA = SP_SMA_5 - SP_SMA_10 
colnames(Diff_SMA) = "Diff_SMA"
Diff_SMA <- Diff_SMA[!is.na((Diff_SMA[, 1]))] #remove the N.A from the XTS object
Signal_SMA = matrix(0, length(Diff_SMA), 1)
for (i in 1:length(Signal_SMA)){
  if (Diff_SMA[i] > 0){
    Signal_SMA[i] = 1 
  } else {
    Signal_SMA[i] = 0 
  }
} # if the line of short moving average cross the line of the longer moving average, this can be interpretated as a buy signal => Signal =1 
Signal = xts(Signal_SMA, order.by = time(Diff_SMA)) # create the XTS object
#Plot the Moving Average (5 and 10), the difference, and the signal 
plot1 = ggplot(merge(SP_SMA_5, SP_SMA_10), aes(x = time(merge(SP_SMA_5, SP_SMA_10)))) +  # --> @Jérôme: J''ais du déplacer ce code et mtn ça marche pas psk Data est pas encore défini.
  geom_line(aes(y = SP_SMA_5, colour = "SP_500_SMA_5")) +
  geom_line(aes(y = SP_SMA_10, colour = "SP_500_SMA_10")) +
  labs(x = "Time", y = "") +
  scale_color_discrete(name = "Legend") 
plot2 = ggplot(Diff_SMA, aes(x = time(Diff_SMA))) +
  geom_line(aes(y = Diff_SMA, colour = "Difference")) +
  labs(x = "Time", y = "") +
  scale_color_discrete(name = "Legend") 
plot3 = ggplot(Signal, aes(x = time(Signal))) +
  geom_line(aes(y = Signal, colour = "Buy_Sell")) +
  labs(x = "Time", y = "") + 
  scale_color_discrete(name = "Legend") 
grid.arrange(plot1, plot2, plot3, nrow = 3, ncol = 1)  #2 graphes sur 2 lignes et une colone

# 5) Create the momentum indicators: 
Momentum_5 = ROC(coreData$SP_500, n = 5, type = c("continuous"), na.pad = TRUE)
Momentum_5 = Momentum_5[!is.na((Momentum_5[, 1]))]
ggplot(Momentum_5, aes(x = time(Momentum_5))) + #aes() creates a mapping of variables to various parts of the plot
  geom_line(aes(y = Momentum_5, colour = "Momentum_5")) +
  labs(x = "Time", y = "") + #no axes labels
  scale_color_discrete(name = "Legend") 
hist <- ggplot(Momentum_5, aes(x = Momentum_5)) +
  geom_histogram(aes(y = ..density..), bins = 20,
                 color = "black", fill = "white")
# Momentum_5 = normalize(Momentum_5)

# 6) RSI - RSI > 70 Overbaught market, RSI < 30 Oversold market
RSI_15 = RSI(coreData$SP_500, n = 15)
ggplot(RSI_15, aes(x = time(RSI_15))) + #aes() creates a mapping of variables to various parts of the plot
  geom_line(aes(y = RSI_15, colour = "RSI_15")) +
  labs(x = "Time", y = "") + #no axes labels
  scale_color_discrete(name = "Legend") 
#Normalize the RSI: Since the RSI is already between 0 and 100, we will juste scale it down to 0 to 1
RSI_15 = RSI_15/100

# 7) MACD:
MACD = MACD(coreData$SP_500, nFast = 12, nSlow = 26, nSig = 9, percent = TRUE)
MACD = MACD[,1]
ggplot(MACD, aes(x = time(MACD))) + 
  geom_line(aes(y = MACD[, 1], colour = "RSI_15")) +
  labs(x = "Time", y = "") + 
  scale_color_discrete(name = "Legend") 
#Normalize MACD:
# MACD = MACD[!is.na((MACD[, 1]))]
# MACD = normalize(MACD[,1] )

# 8) CCI
CCI_15 =  CCI(coreData$SP_500, n = 15, c = 0.015)
ggplot(CCI_15, aes(x = time(CCI_15))) + 
  geom_line(aes(y = CCI_15[, 1], colour = "RSI_15")) +
  labs(x = "Time", y = "") + 
  scale_color_discrete(name = "Legend") 
#Normalize CCI:
CCI_15 = CCI_15[!is.na((CCI_15[, 1]))]
CCI_15 = normalize(CCI_15)

# 9) Change_risk --> @Jérôme: J''ais du déplacer ce code et mtn ça marche pas psk Data est pas encore défini.
risk = coreData$P1_risk
ggplot(risk, aes(x = time(risk))) + 
  geom_line(aes(y = risk,colour = "Risk")) +
  labs(x = "Time ", y = "") + 
  scale_color_discrete(name = "Legend") 
change_risk = diff(risk)/lag(risk)
ggplot(change_risk, aes(x = time(change_risk))) + 
  geom_line(aes(y = change_risk,colour = "change_risk")) +
  labs(x = "Time ", y = "") + 
  scale_color_discrete(name = "Legend") 
#Normalize
# change_risk = change_risk[!is.na((change_risk[, 1]))]
# change_risk_normalized = normalize(change_risk)

# 10) change_gold
Gold = coreData$Gold
change_gold = diff(Gold)/lag(Gold) # no laging --> to be approved by Jérôme
ggplot(change_gold, aes(x = time(change_gold))) + 
  geom_line(aes(y = change_gold, colour = "change_gold")) +
  labs(x = "Time ", y = "") + 
  scale_color_discrete(name = "Legend") 
hist <- ggplot(change_gold, aes(x = change_gold)) +
  geom_histogram(aes(y = ..density..), bins = 20,
                 color = "black", fill = "white")
# change_gold = lag(change_gold, k = -1) # too proceed in the same way as the calculation of the SPEvolution 
#change_gold = change_gold[!is.na((change_gold[, 1]))]
#change_gold = normalize(change_gold )

# 11) Vix Variable
VIX = coreData$Vix
change_VIX = diff(VIX)/lag(VIX) # no laging --> to be approved by Jérôme
ggplot(change_VIX, aes(x = time(change_VIX))) + 
  geom_line(aes(y = change_VIX, colour = "change_VIX")) +
  labs(x = "Time ", y = "") + 
  scale_color_discrete(name = "Legend")
# change_VIX = lag(change_VIX, k = -1) # too proceed in the same way as the calculation of the SPEvolution # this means that if the change between t and t+1 is postive, this value will be indexed to t (and not to t+1)
#change_VIX = change_VIX[!is.na((change_VIX[, 1]))]
#change_VIX = normalize(change_VIX )


# Part 2b: Merge data, remove NAs, divide training/testing --------------------------------------------------------------------------------



Data = merge(spEvolution, SP_WMA_10_return, SP_SMA_5_return, SP_SMA_10_return ,
               Momentum_5 ,RSI_15, MACD ,CCI_15, Signal, change_risk, 
               change_gold, change_VIX)
Data = Data[complete.cases(Data), ] #remove the N.A from the XTS object
colnames(Data) = c("spEvolution","SP_WMA_10_return", "SP_SMA_5_return", "SP_SMA_10_return",
                       "Momentum_5", "RSI_15","MACD","CCI_15","Signal","change_risk",
                       "change_gold","change_VIX") 
                      
# Parameter of training
ratioTraining = 0.1            

#Divided into part the whole dataset, learning and testing stet: => more simple , avoid to recalculate all the depende variables
trainingLimit = round(nrow(Data) * ratioTraining, digits = 0)
Data_Learning = Data[0:trainingLimit,]
Data_Testing = Data[(trainingLimit + 1):nrow(Data), ]

# Part 3: Logistical Regression -----------------------------------------------------------------------------------------------------



# Initial parameters
attributeVariables = Data_Learning[, 2:ncol(Data)] # matrix(rnorm(numberObservations * numberAttributeVariables, mean = 0, sd = 1), nrow = numberAttributeVariables, ncol = numberObservations)# !!!!!!!!! Need to define this from the data !!!!!!!!!!
randomWeight = rnorm(n = ncol(dataSet) -1 , mean = 0, sd = sqrt(1)) # The value of the initial weights. note: we can randomize this later on
resultVector = Data_Learning[, 1] # rbinom(10, 1, 0.5) # !!!!!!!!! Need to define this from the data !!!!!!!!!! => This the vector of return of the SP&500
outerloop = 10 # How many times we go over the data
eta = 0.3 # error adaptation coefficient
flagPerceptronLearning = 1 # 0 if using an ADALINE-like learning algorithm, 1 if using an PERCEPTRON-like learning algorithm.


# Creating the x_0 vector
x_0 = xts(rep(1, length(resultVector)), order.by = index(resultVector))

# Merging all the data into the dataFrame
dataSet = merge(x_0, attributeVariables, resultVector)

# Key variables for calculations
numberAttributeVariables = ncol(dataSet) - 2
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


# Part 4: Testing -----------------------------------------------------------------------------------------------------

#Define the new Set of data:


#Recombine the data and remove the NA:
coreData_Testing$Constant = xts(rep(1, length(coreData_Testing$Evolution)), order.by = index(coreData_Testing))

# Inputs
attributeVariablesTesting = Data_Testing[, 2:ncol(Data)] # matrix(rnorm(numberObservations * numberAttributeVariables, mean = 0, sd = 1), nrow = numberAttributeVariables, ncol = numberObservations)# !!!!!!!!! Need to define this from the data !!!!!!!!!!
resultVectorTesting = Data_Testing[, 1] # rbinom(10, 1, 0.5) # !!!!!!!!! Need to define this from the data !!!!!!!!!! => This the vector of return of the SP&500
weightVectorTesting = weightVector

# Variabes calculation
x_0_Testing = xts(rep(1, length(resultVectorTesting)), order.by = index(resultVectorTesting))
dataSetTesting = merge(x_0_Testing, attributeVariablesTesting, resultVectorTesting)
numberObservationsTesting = nrow(dataSetTesting)
numberAttributeVariablesTesting = ncol(dataSetTesting) - 2
indexTesting  = matrix(NA, 1, numberObservationsTesting)
predictionTesting = matrix(NA, 1, numberObservationsTesting)
predictionAccuracyTesting = matrix(NA, 1, numberObservationsTesting)

# Testing the prediction of the model
for (i in 1 : numberObservationsTesting) {
  indexTesting[i] = 1/(1 + exp(-(sum(coredata(dataSetTesting[i, 0:numberAttributeVariablesTesting + 1]) %*% weightVectorTesting )))) 
  if (indexTesting[i] < 0.5) {
    predictionTesting[i] = 0
  } else { 
    predictionTesting[i] = 1
  }
  # Also tests whether our prediction was accurate and stores it in predictionAccuracy
  if ((indexTesting[i] < 0.5) & (dataSetTesting[i, numberAttributeVariablesTesting + 2] < 0.5) | (( indexTesting[i]) >= 0.5) & (dataSetTesting[i, numberAttributeVariablesTesting + 2] >= 0.5)) {
    predictionAccuracyTesting[, i] = 1
  } else {
    predictionAccuracyTesting[, i] = 0
  }
}

# Transforming some results into time series
predictionAccuracyTesting = xts(x = t(predictionAccuracyTesting), order.by = index(dataSetTesting))
indexTesting = xts(x = t(indexTesting), order.by = index(dataSetTesting))

# Plotting results:
ggplot(predictionAccuracyTesting, aes(x = time(predictionAccuracyTesting))) + #aes() creates a mapping of variables to various parts of the plot
  geom_line(aes(y = predictionAccuracyTesting, colour = "predictionAccuracyTesting")) +
  labs(x = "Time", y = "") + #no axes labels
  scale_color_discrete(name = "Legend") 

ggplot(indexTesting, aes(x = time(indexTesting))) + #aes() creates a mapping of variables to various parts of the plot
  geom_line(aes(y = indexTesting, colour = "indexTesting")) +
  labs(x = "Time", y = "") + #no axes labels
  scale_color_discrete(name = "Legend") 



# Most important value !
successRatio = sum(predictionAccuracyTesting)/nrow(dataSetTesting)
print(successRatio)


# Values where we are kinda sure
extremeValues = 0.2
Bets = indexTesting[(indexTesting[] > 1 - extremeValues) | (indexTesting[] < extremeValues)]
Result = predictionAccuracyTesting[(indexTesting[] > 1 - extremeValues) | (indexTesting[] < extremeValues)]
Bets = round(Bets, digits = 0)
Success = 0
for (i in 1:nrow(Bets)) {
  if (Bets[i] == Result[i]) {
    Success = Success + 1
  }
}
Success/nrow(Bets)



