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

#MLP(0.1, 10,dataSet, outerloop )


MLP = function(eta, numberNeurons, dataSet, outerloop) {
  
# Description: 1 Hidden Layer Perceptron Neural Network using Logistic Regression 
#             as activation function with gradient descent optimisation. 

# Authors: Michal Stachnio & Jérôme Voelke

# Variables:
#           eta           = Learning rate
#           numberNeurons = number of neurons in the hiddenLayer
#           dataSet       = Observations. Need to have x_0 = 1 as first column and the result (y) as the last column
#           outerloop     = number of times the network learns over the dataSet. A higher number is advised due to slow learning
  
  
# Outputs:
#           success            = rate with which the outputed weight are able to predict the target variable accuratelly
#           hiddenLayerWeights = weights for all the input data for all the neurons
#           outputLayerWeights = weights for the outputs of the neurons
  

  
# Part 1: Creation of required Variable structures  ------------------------------------------------------------------------------------------------------------------

  # Hidden Layer Weights
  hiddenLayerWeights = matrix(rnorm(n = (ncol(dataSet) - 1) * (numberNeurons + 1), mean = 0, sd = sqrt(1)), # Initial weights are initialized using a normal distribution
                              nrow = ncol(dataSet) - 1, ncol = numberNeurons + 1, byrow = TRUE)
  hiddenLayerWeights = NamingRows("w", hiddenLayerWeights, 0)
  hiddenLayerWeights = NamingCols("n", hiddenLayerWeights, 0)
  
  # Output Layer Weights
  outputLayerWeights = matrix(rnorm(n = numberNeurons + 1, mean = 0, sd = sqrt(1)), # Initial weights are initialized using a normal distribution
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
      
      # Recalibrating weights
      outputLayerWeights = outputLayerWeights + eta * hiddenIndex[l, ] * errorOutput[l] * outputIndex[l] * (1 - outputIndex[l]) 
      hiddenLayerWeights = hiddenLayerWeights + eta * as.vector(dataSet[l, 1:(ncol(dataSet) - 1)]) * +
                            t(as.vector(hiddenIndex[l, ] * (hiddenIndexSubstraction - hiddenIndex[l, ])) * t(hiddenLayerWeights) *+
                            errorOutput[l] * outputIndex[l] * + (1 - outputIndex[l])) 
    
      if ((outputIndex[l] < 0.5) & (dataSet[l, ncol(dataSet)] < 0.5) | ((outputIndex[l]) >= 0.5) & (dataSet[l, ncol(dataSet)] >= 0.5)) {
        success = success + 1
      }
      
    }
  }
  
  
  success = success / nrow(dataSet)
  Results = list(success = success, hiddenLayerWeights = hiddenLayerWeights, outputLayerWeights = outputLayerWeights) 
  return(Results)
}

# 
# LogitFunction = function(x) {
#   Result = 1/(1 + exp(-(sum(x))))
#   return (Result)
# }
#   