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

