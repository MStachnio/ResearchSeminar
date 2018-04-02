library(xts)
library(zoo)
#------------------------------------------------------------------------------------

beginingDate = as.Date(index(head(coreData, 1)), format = "%Y.%m.%d") 
endDate      = as.Date(index(tail(coreData, 1)), format = "%Y.%m.%d") 
dateSequence = seq(from = beginingDate, to = endDate, by = 1)

#------------------------------------------------------------------------------------

x_0 = matrix(1, nrow = 1, ncol = numberObservations)

#------------------------------------------------------------------------------------

# Assigns the right names to rows in dataSet
dataSetRowNames = matrix("0", nrow = nrow(dataSet), ncol= 1, byrow = TRUE)
for (i in 1:nrow(dataSet)) {
  if (i == 1) {
    dataSetRowNames[i, ] = "x0"
  } else if (i < nrow(dataSet)) {
    dataSetRowNames[i, ] = paste("x", i - 1, sep = "") # sep = "" means that there is no space between w and the number
  } else {
    dataSetRowNames[i, ] = "y"
  }
}
rownames(dataSet) = c(dataSetRowNames)

#------------------------------------------------------------------------------------

#---------------------------------------------------------------------------------------------------------------------------------------
# Other usefull commands:
# storage.mode(tedRate) <- "numeric" # Converts the data into numeric value (sometimes xts automatically converts it into strings)

# # Creates a sequence of dates
# startingDate = "2000-01-01"
# endDate = "2018-03-05"
# dates = seq(as.Date(startingDate), as.Date(endDate), by=1)

# # Lag 1 of data:
# tedRate2 = lag(tedRate, k=1, na.pad =FALSE)

# # Merging data
# merge(tedRate, tedRate2, all=TRUE, fill = NA)
#---------------------------------------------------------------------------------------------------------------------------------------
SimpleMovingAverageTS = function (x, n) {
  divisor = n
  y = x
  for (l in 1:length(x)) { # This part calculates the simple sum
    weightedSum = 0
    if (l < n) { #to not take into account the first 9 steps
      y[l,] = NA
    } else {
      for (i in 1:n) {
        weightedSum = weightedSum + coredata(x[l - i + 1]) #the addition are done backward: first: 10-1+1 = 10, snd 10-2+1 =9.... 10-10+1
      }
      y[l, ] = weightedSum
    }
  }
  return(y/divisor) # The function outputs a weighted timeseries
}
#---------------------------------------------------------------------------------------------------------------------------------------
PLR = function(eta, dataSet, outerloop, flagPerceptronLearning, stdRandomWeight, learningFlag){
  
  # Description: No Hidden Layer Perceptron Logistic Regression optimised with gradient descent optimisation. 
  
  # Authors: Michal Stachnio & J√©r√¥me Voelke
  
  # Variables:
  #           eta             = Learning rate
  #           dataSet         = Observations. Need to have x_0 = 1 as first column, attribute variables in the middle and the result (y) as the last column
  #           outerloop       = number of times the network learns over the dataSet. A higher number is advised due to slow learning
  #           stdRandomWeight = Standard deviation used for the initialisation of the random weights
  #           learningFlag    = if 1, the algorithm updates the weights
  
  # Outputs:
  #           success            = rate with which the outputed weight are able to predict the target variable accuratelly
  #           hiddenLayerWeights = weights for all the input data for all the neurons
  #           outputLayerWeights = weights for the outputs of the neurons
  
  
  # Key dimensions for calculations
  numberAttributeVariables = ncol(dataSet) - 2
  numberObservations = nrow(dataSet)
  
  # Creating the weight vector
  randomWeight = rnorm(n = ncol(dataSet) -1 , mean = 0, sd = stdRandomWeight) # The value of the initial weights. note: we can randomize this later on
  weightVector = matrix(randomWeight, # define the vector of weights
                        nrow = numberAttributeVariables+1, # we Create a weight for each dependent variable, plus one for w0.
                        ncol= 1,
                        byrow = TRUE)
  # naming the rows in weightVector
  weightVector = NamingRows("w", weightVector, 0)
  
  # Defining the key variables for the Logistic Regression Algorithm
  index = matrix(0, 1, numberObservations * outerloop)
  indexPrediction = 0 
  weightMatrix = matrix(NA, nrow(weightVector),  numberObservations * outerloop) # WeightMatrix is used to record the weights through the various iterations.
  weightMatrix[, 1] = weightVector
  weightMatrix = NamingRows("w", weightMatrix, 0)
  error = 0
  predictionAccuracy = matrix(NA, numberObservations,  1)
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
      } else {
        indexPrediction[i] = 1
      }
      error[i] = dataSet[i, numberAttributeVariables + 2] - flagPerceptronLearning * indexPrediction[i] + (-1 + flagPerceptronLearning) * index[i] # flagPerceptronLearning defines whether we use an ADALINE or Perceptron-like algorithm
      # Also tests whether our prediction was accurate and stores it in predictionAccuracy
      if ((index[i] < 0.5) & (dataSet[i, numberAttributeVariables + 2] < 0.5) | (( index[i]) >= 0.5) & (dataSet[i, numberAttributeVariables + 2] >= 0.5)) {
        predictionAccuracy[i, ] = 1
      } else {
        predictionAccuracy[i, ] = 0
        lastError = i + (l - 1) * numberObservations
      }
      if (learningFlag == 1) {
        weightVector =  weightVector + eta * error[i] * index[i] * (1 - index[i]) * as.vector(dataSet[i, 0 : numberAttributeVariables + 1]) #Update
      }
    }
  }
  
  success = sum(predictionAccuracy) / nrow(dataSet)
  Results = list(success = success, weightVector = weightVector) 
  return(Results) 
  
}

