#Q1
#setwd("") #set your working directory using the "Session" menu

#Q2
library(ggplot2) #package needed for nice plots
library(xts) #package needed in order to create/use time series objects
library(fBasics) #package for detailed summary statistics
library(tseries) #package for JB test

#Q3
data <- read.table("s1_data.txt",header = TRUE,sep= "\t") #import the data (a tab delimited txt file with headers)

#Q4
class(data) #class or type of an object
str(data) #evaluate the object structure and the data types
date <- as.Date(data$date) # $ sign is used to extract a variable/column
ts <- xts(x = data[-1], order.by = date) #we leave out the first column of data which is the date ([-1])
remove(data,date) #we don't need this data anymore (we have it in ts)
start(ts) #start date of our time series
end(ts) #end date of our time series
periodicity(ts) # tells us the periodicity/frequency (daily, weekly, monthly)

#Q5 plot of SP500 & SMIUSD
#using ggplot() from ggplot2
plot1 <- ggplot(ts, aes(x=time(ts))) + #aes() creates a mapping of variables to various parts of the plot
  geom_line(aes(y=SP500,colour = "sp500")) +
  geom_line(aes(y=SMIUSD,colour = "smi")) + 
  labs(x="", y="") + #no axes labels
  scale_color_discrete(name="Legend") 
plot1
plot2 <- plot1 + theme_bw() #if we want to change the default theme_gray
plot2
#using autoplot.zoo() from zoo (returns a ggplot2 object, like ggplot())
series <- cbind(ts$SP500,ts$SMIUSD) #cbind() is used to combine vectors
autoplot.zoo(series) 
plot3 <- autoplot.zoo(series,facets = NULL) + theme_bw() #we need to specify "facets = NULL" to draw on a single axis
plot3

#Q6 logreturns (in %)
ts$sp500 <- 100*diff(log(ts$SP500))
ts$smiusd <- 100*diff(log(ts$SMIUSD))
ts$mscie <- 100*diff(log(ts$MSCIE))

#Q7 subsample
ts <- ts['1990-01/']

#Q8 deannualizing rf
ts$RF <- ts$USRF/12

#Q9 Sharpe ratios
ret <- cbind(ts$sp500,ts$smiusd,ts$mscie,ts$RF) #combining several vectors with cbind()
SR <- data.frame(0,0,0) #initializing our dataframe for our SRs
colnames(SR) <- colnames(ret[,1:3]) #give names to SR columns
for (i in 1:3)  SR[,i] <- mean(ret[,i]-ts$RF)/sd(ret[,i]) #we select only the first three columns of ret (1:3)
SR

#Q10 correlation matrix
cor(ret)

#Q11 t-test for sp500
suppressWarnings(t.test(ts$sp500, alternative="two.sided",
                        mu=0,conf.level=0.99))

#if we want to check manually...
T <- nrow(ts$sp500)
se <- sd(ts$sp500)/sqrt(T) #Calculation of the standard-errors (Sample based) / NOT the standard-deviations !! (Which is the true moment)
xbar <- mean(ts$sp500)
tstat <- xbar/se
qt <- qt(.995, df=T-1)  #quantile for the t-distribution (99% two-tailed test => 99.5% quantile)
ci_hi <- xbar + qt*se #we could also have used 2.58 (the normal quantile) instead of qt, which is a good approximation
ci_lo <- xbar - qt*se

#Q12 paired sample t-test for sp500 and RF
suppressWarnings(t.test(ts$sp500,ts$RF,alternative="two.sided",
                        conf.level=0.99,paired=TRUE))

#Q13 summary statistics + p-value
basicStats(ret) #summary statistics from fBasics
#Note that the kurtosis computed by basicStats() is in fact the excess kurtosis. Add 3 to get the real kurtosis.
pval <- data.frame(0,0,0)
colnames(pval) <- colnames(ret[,1:3])
for (i in 1:3) pval[,i] <- pnorm(min(ret[,i]), 
                                 mean=mean(ret[,i]),sd=sd(ret[,i]))#pnorm => done l'air sous la value pour une distribtuion normal
pval

#Q14 testing normality
smi <- ts$smiusd
hist <- ggplot(ret, aes(x=smi)) +
  geom_histogram(aes(y=..density..),bins=20,
                 color="black",fill="white")
hist
#stat_function() makes it easy to superimpose a function (here dnorm) on top of an existing plot
hist <- hist + stat_function(fun=dnorm,args = list(mean(smi), 
                                                   sd(smi)),size=1.2)
hist
#Following a question asked during the session: The y-axis gives us the density of each bin
#(i.e. the proportion of observations within each bin divided by the width of the bin).
#The area of each bin equals the proportion of observations falling in that bin.
#Summing the areas of all bins, we get 1.
jarque.bera.test(smi) #Jarque-Bera test from the package tseries

#Q15 Normal QQ plot
# = scatterplot of the empirical quantiles (y axis) against the standard normal quantiles (x axis)
windows()
qqnorm(smi) 
qqline(smi) #could also be done using ggplot2 but not as easily 

#Q16 Scatter plot
scplot <- ggplot(ret, aes(x=smiusd,y=sp500)) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0)
scplot
scplot1 <- scplot + geom_point()
scplot1
scplot2 <- scplot1 + geom_smooth(method="lm")
#By default, geom_smooth(method="lm") plots a 95% confidence level interval around the regression line.
scplot2
#optional part
scplot3 <- scplot +
  geom_text(aes(label=format(time(ts), "%Y.%m")), size=3)
scplot3
