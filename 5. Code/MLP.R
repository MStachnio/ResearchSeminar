# Defining two functions to name data structures
NamingRows = function(String, data, startingValue) {
 
  # Input Variables:
  # String        = value in "" that will be the name that will be iterated
  # data          = data structure that will have a new name for rows
  # startingValue = allows to start with 0 or 1 etc...
  
  dataNames = matrix("0", nrow = nrow(data), ncol = 1, byrow = TRUE)
  for (i in 1:nrow(dataNames)) {
    dataNames[i, ] = paste(String, i - 1 + startingValue, sep = "") # sep="" means that there is no space between w and the number
  }
  rownames(data) = c(dataNames)
  return (data)
}

NamingCols = function(String, data, startingValue) {
  dataNames = matrix("0", nrow = 1, ncol = ncol(data), byrow = TRUE)
  for (i in 1:ncol(dataNames)) {
    dataNames[, i] = paste(String, i - 1 + startingValue, sep = "") # sep="" means that there is no space between w and the number
  }
  colnames(data) = c(dataNames)
  return (data)
}


# Example Parameters 
eta = 0.1
numberNeurons = 10
x_0 = xts(rep(1, nrow(Data_Learning)), order.by = index(Data_Learning))
dataSet = merge(x_0, Data_Learning[, 2:ncol(Data_Learning)], Data_Learning[, 1])
outerloop = 100
stdRandomWeight = 1
learningFlag = 1

#MLP(0.1, 10,dataSet, outerloop )


MLP = function(eta, numberNeurons, dataSet, outerloop, stdRandomWeight, learningFlag) {
  
# Description: 1 Hidden Layer Perceptron Neural Network using Logistic Regression 
#             as activation function with gradient descent optimisation. 

# Authors: Michal Stachnio & Jérôme Voelke

# Variables:
#           eta           = Learning rate
#           numberNeurons = number of neurons in the hiddenLayer
#           dataSet       = Observations. Need to have x_0 = 1 as first column and the result (y) as the last column
#           outerloop     = number of times the network learns over the dataSet. A higher number is advised due to slow learning
#           learningFlag  = if 1, the weights are adjusted against the data
  
# Outputs:
#           success            = rate with which the outputed weight are able to predict the target variable accuratelly
#           hiddenLayerWeights = weights for all the input data for all the neurons
#           outputLayerWeights = weights for the outputs of the neurons
  

  
# Part 1: Creation of required Variable structures  ------------------------------------------------------------------------------------------------------------------

  # Hidden Layer Weights
  hiddenLayerWeights = matrix(rnorm(n = (ncol(dataSet) - 1) * (numberNeurons + 1), mean = 0, sd = stdRandomWeight), # Initial weights are initialized using a normal distribution
                              nrow = ncol(dataSet) - 1, ncol = numberNeurons + 1, byrow = TRUE)
  hiddenLayerWeights = NamingRows("w", hiddenLayerWeights, 0)
  hiddenLayerWeights = NamingCols("n", hiddenLayerWeights, 0)
  
  # Output Layer Weights
  outputLayerWeights = matrix(rnorm(n = numberNeurons + 1, mean = 0, sd = stdRandomWeight), # Initial weights are initialized using a normal distribution
                              nrow = numberNeurons + 1, ncol = 1, byrow = TRUE)
  outputLayerWeights = NamingRows("n", outputLayerWeights, 0)
  
  
  # Hidden Index 
  hiddenIndex = matrix(0, nrow = nrow(dataSet), ncol = numberNeurons + 1)
  hiddenIndex = NamingCols("n", hiddenIndex, 0)
  hiddenIndex[, 1] = 1 # We set n0 to 1 because we want to include the offset in our calculations
  hiddenIndexSubstraction = matrix(1, nrow = 1, ncol = numberNeurons +1)
  hiddenIndexSubstraction[1, 1] = 2 # Used for substraction in the computation of the derivative of the activation function. Since the offset is non dependent on the parameters, we simply multiply by 1.
  
  # Error of Hidden Layer
  errorHidden = matrix(0, nrow = nrow(dataSet), ncol = numberNeurons + 1)
  errorHidden = NamingCols("n", errorHidden, 0)
  
  # Output Index
  outputIndex = matrix(0, nrow = nrow(dataSet), ncol = 1) 
  
  # Error of the Output Layer
  errorOutput = matrix(0, nrow = nrow(dataSet), ncol = 1)
  
  # Key dimensions for calculations
  numberAttributeVariables = ncol(dataSet) - 2
  numberObservations = nrow(dataSet)
  
  
# Part 2: Finding the weights  ------------------------------------------------------------------------------------------------------------------
  
  for (i in 1:outerloop){
    success = 0
    for (l in 1:nrow(dataSet)) {
      # Calculating Indexes
      hiddenIndex[l, 2:ncol(hiddenIndex)] = 1/(1 + exp(-(dataSet[l, 0:(numberAttributeVariables + 1)] %*% hiddenLayerWeights[, 2:ncol(hiddenLayerWeights)])))
      outputIndex[l] =  1/(1 + exp(-(sum(hiddenIndex[l, ] %*% outputLayerWeights))))
      
      # Calculating Errors
      errorOutput[l] = dataSet[l, ncol(dataSet)] - outputIndex[l]
      errorHidden[l, ] = errorOutput[l] * outputLayerWeights
      
      if (learningFlag == 1) {
          # Recalibrating weights
          outputLayerWeights = outputLayerWeights + eta * hiddenIndex[l, ] * errorOutput[l] * outputIndex[l] * (1 - outputIndex[l]) 
          hiddenLayerWeights = hiddenLayerWeights + eta * as.vector(dataSet[l, 1:(ncol(dataSet) - 1)]) * +
                                t(as.vector(hiddenIndex[l, ] * (hiddenIndexSubstraction - hiddenIndex[l, ])) * t(hiddenLayerWeights) *+
                                errorOutput[l] * outputIndex[l] * + (1 - outputIndex[l])) 
        }
      
      if ((outputIndex[l] < 0.5) & (dataSet[l, ncol(dataSet)] < 0.5) | ((outputIndex[l]) >= 0.5) & (dataSet[l, ncol(dataSet)] >= 0.5)) {
        success = success + 1
      }
      
    }
  }
  
  
  success = success / nrow(dataSet)
  Results = list(success = success, hiddenLayerWeights = hiddenLayerWeights, outputLayerWeights = outputLayerWeights) 
  return(Results)
}


# Initial parameters
attributeVariables = Data_Learning[, 2:ncol(Data) - 1] # matrix(rnorm(numberObservations * numberAttributeVariables, mean = 0, sd = 1), nrow = numberAttributeVariables, ncol = numberObservations)# !!!!!!!!! Need to define this from the data !!!!!!!!!!
resultVector = Data_Learning[, ncol(Data)]
outerloop = 10 # How many times we go over the data
eta = 0.3 # error adaptation coefficient
flagPerceptronLearning = 1 # 0 if using an ADALINE-like learning algorithm, 1 if using an PERCEPTRON-like learning algorithm.
dataSet = Data_Learning
stdRandomWeight = 5
learningFlag = 1
PLR(eta, Data_Learning, outerloop, flagPerceptronLearning, stdRandomWeight)

PLR = function(eta, dataSet, outerloop, flagPerceptronLearning, stdRandomWeight, learningFlag){

  # Description: No Hidden Layer Perceptron Logistic Regression optimised with gradient descent optimisation. 
  
  # Authors: Michal Stachnio & Jérôme Voelke
  
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