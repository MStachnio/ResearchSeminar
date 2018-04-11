MLP = function(eta, numberNeurons, dataSet, outerloop, initialHiddenWeights, initialOutputWeights, learningFlag, momentumConstant) {
  
  # Description: One Hidden Layer Perceptron Neural Network using Logistic Regression 
  #             as activation function with gradient descent optimisation. 
  
  # Authors: Michal Stachnio & Jérôme Voelke - 2018.04.05
  
  # Variables:
  #           eta                   = Learning rate
  #           numberNeurons         = number of neurons in the hiddenLayer
  #           dataSet               = Observations. Need to have x_0 = 1 as first column and the result (y) as the last column
  #           outerloop             = number of times the network learns over the dataSet. A higher number is advised due to slow learning
  #           initialHiddenWeights  = initial weights at the hidden level (size: (numberNeurons + 1) x (number attribute variables + 1). note: The first column of this matrix is not used and could be eventually disposed of (was created for historical reasons).
  #           initialOutputWeights  = initial weights at the output level.
  #           learningFlag          = if 1, the weights are adjusted against the data, if 0, weights do not change
  #           momentumConstant      = Used to increase the learning speed of the algorithm, recommended between 0.4 and 0.7.
  
  # Outputs:
  #   The algorithm outputs a list variable "Results" that includes:
  #           success            = rate with which the model is able to predict the target variable accuratelly
  #           hiddenLayerWeights = hidden weights at the end. 
  #           outputLayerWeights = output weights at the end.
  #           outputIndex        = The value of the model for the different data lines
  #           errorIndexing      = evolution of the error at each iteration
  #           difference         = deviation of our model's prediction from the actual values
  
  
  # Part 1: Creation of required Variable structures  ------------------------------------------------------------------------------------------------------------------
  
  # Hidden Layer Weights
  hiddenLayerWeights = matrix(initialHiddenWeights, nrow = ncol(dataSet) - 1, ncol = numberNeurons + 1, byrow = FALSE)
  hiddenLayerWeights = NamingRows("w", hiddenLayerWeights, 0)
  hiddenLayerWeights = NamingCols("n", hiddenLayerWeights, 0)
  
  # Output Layer Weights
  outputLayerWeights = matrix(initialOutputWeights, nrow = numberNeurons + 1, ncol = 1, byrow = FALSE)
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
  
  # Error of the Output Layer multiplied by the derivative of the integration function
  errorOutput = matrix(0, nrow = nrow(dataSet), ncol = 1)
  
  # The difference between the target value and the output
  difference = matrix(0, nrow = nrow(dataSet), ncol = 1)
  
  # Key dimensions for calculations
  numberAttributeVariables = ncol(dataSet) - 2
  numberObservations = nrow(dataSet)
  
  # Current output correction (variable used to optimise calculations)
  currentOutputCorrection = matrix(0, nrow = numberNeurons + 1, ncol = 1, byrow = FALSE)
  currentOutputCorrection = NamingRows("n", currentOutputCorrection, 0)
  
  currentHiddenCorrection = matrix(0, nrow = ncol(dataSet) - 1, ncol = numberNeurons + 1, byrow = FALSE)
  currentHiddenCorrection  = NamingRows("w", currentHiddenCorrection, 0)
  currentHiddenCorrection  = NamingCols("n", currentHiddenCorrection, 0)
  
  # Previous Output correction (for momentum convergence)
  previousOutputCorrection = matrix(0, nrow = numberNeurons + 1, ncol = 1, byrow = TRUE)
  previousOutputCorrection = NamingRows("n", previousOutputCorrection, 0)
  
  previousHiddenCorrection = matrix(0, nrow = ncol(dataSet) - 1, ncol = numberNeurons + 1, byrow = TRUE)
  previousHiddenCorrection  = NamingRows("w", previousHiddenCorrection, 0)
  previousHiddenCorrection  = NamingCols("n", previousHiddenCorrection, 0)
  
  errorIndexing = matrix(NA, outerloop, ncol = 1)
  
  
  
  # Part 2: Finding the weights  ------------------------------------------------------------------------------------------------------------------
  
  i=1
  l=1
  
  for (i in 1:outerloop){
    success = 0
    for (l in 1:nrow(dataSet)) {
      # Calculating Indexes
      hiddenIndex[l, 2:ncol(hiddenIndex)] = 1/(1 + exp(-(dataSet[l, 1:(numberAttributeVariables + 1)] %*% hiddenLayerWeights[, 2:ncol(hiddenLayerWeights)])))
      outputIndex[l] =  1/(1 + exp(-(hiddenIndex[l, ] %*% outputLayerWeights)))
      
      if (learningFlag == 1) {
        
        # Calulcate the error
        difference[l] = (dataSet[l, ncol(dataSet)] - outputIndex[l])  
        errorOutput[l] = - difference[l] * outputIndex[l] * (1 - outputIndex[l])
        errorHidden[l,2:ncol(errorHidden)] = errorOutput[l] * outputLayerWeights[2:nrow(outputLayerWeights)] * as.vector(hiddenIndex[l, 2:ncol(hiddenIndex)] * (hiddenIndexSubstraction[,2:ncol(hiddenIndexSubstraction)] - hiddenIndex[l,2:ncol(hiddenIndex)]))
      
        # Calculate the Correction:
        currentOutputCorrection = eta * hiddenIndex[l, ] * errorOutput[l] + momentumConstant * previousOutputCorrection
        
        currentHiddenCorrection = eta * t(dataSet[l, 1:(ncol(dataSet) - 1)]) %*% errorHidden[l, ]  + momentumConstant * previousHiddenCorrection
        currentHiddenCorrection = NamingRows("w", currentHiddenCorrection, 0)

        # Update the weights:
        outputLayerWeights = outputLayerWeights - currentOutputCorrection 
        hiddenLayerWeights = hiddenLayerWeights - currentHiddenCorrection

        # Recording previous weights
        previousOutputCorrection = currentOutputCorrection  
        previousHiddenCorrection = currentHiddenCorrection
        
      } else {
        difference[l] = -(dataSet[l, ncol(dataSet)] - outputIndex[l]) # We still want to know the difference even when we're not learning
      }
      
    if ((outputIndex[l] < 0.5) && (dataSet[l, ncol(dataSet)] < 0.5) || ((outputIndex[l]) >= 0.5) && (dataSet[l, ncol(dataSet)] >= 0.5)) {
        success = success + 1
    }
      
  }
    
  # used to give a nice visual feedback on the progress
  errorIndexing[i] = sum(1/2*difference^2)
  print("error:")
  print(errorIndexing[i])
  print("outerloop:")
  print(i)
  
  if (outerloop > 100) {
    print("success ratio:")
    print(success/nrow(dataSet))
    plot(errorIndexing[1:i], type="p", col="black") # We include this in a for condition because we do not want to get this graph in the simulation phase.
  }
  
  }
  
  outputIndex = xts(outputIndex, order.by = index(dataSet))
  success = success / nrow(dataSet)
  Results = list(success = success, hiddenLayerWeights = hiddenLayerWeights, outputLayerWeights = outputLayerWeights, outputIndex = outputIndex, errorOutput = errorOutput,  errorIndexing =  errorIndexing, difference = difference) 
  return(Results)
}
