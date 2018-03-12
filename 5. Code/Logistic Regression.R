library(xts)
library(zoo)

#
#
#         Part 1: Creating the dataFrame
#
#



# Opening the data
csvFilePathTedRate = "/Users/Michal/Dropbox/UNISG/16. Research Seminar/4. Data/TED Spread.csv"
tedRate = read.csv(file=csvFilePathTedRate, header=TRUE, sep=",")

# Replaces all "." as "NA"
tedRate[tedRate == "."] <- NA

# Creates a sequence of dates
startingDate = "2000-01-01"
endDate = "2018-03-05"
dates = seq(as.Date(startingDate), as.Date(endDate), by=1)

# OPTIONAL: Truncating the size for testing
tedRate = tedRate[1:20, c('DATE', 'TEDRATE')]
dates = dates[1:29]

# Create a Timeseries of tedRate indexed by date
tedRate$DATE = as.Date(tedRate$DATE, format="%Y-%m-%d")
tedRate = xts(tedRate[,2], order.by = tedRate$DATE)

#Converts the data into numeric value (somehow xts automatically converts it into characters)
storage.mode(tedRate) <- "numeric"

tedRate2 = lag(tedRate, k=1, na.pad =FALSE)
merge(tedRate, tedRate2, all=TRUE, fill = NA)

#
#
#         Part 2: Logistical Regression
#
#

