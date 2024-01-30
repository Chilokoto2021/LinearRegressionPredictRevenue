
#Load the data and inspect the data----------------------------------------------------
data <- read.csv("revenue.csv")
summary(data)
plot(data)
---------------------------------------------------------------------------------------
  #Load the libraries and split the data into train and test-----------------------------
library(caTools)
set.seed(2)
sample = sample.split(data, SplitRatio = 0.7)

test = subset(data, sample == 'TRUE')
train = subset(data, sample == 'FALSE')
---------------------------------------------------------------------------------------
  summary(test)
summary(train)

#Train the model ----------------------------------------------------------------------
mod <-  lm(Profit ~ ., data = train)
---------------------------------------------------------------------------------------
  
  #Inspect the model summary and plot a histogram of the residuals ---------------------
summary(mod)

hist(mod$residuals)

#Test the model accuracy -------------------------------------------------------------
pred <- predict(mod, test)
plot(test$Profit, type = "l", lty = 1.8, col ="red")
lines(pred, type = "l", lty = 1.8, col ="blue")

accur <- sqrt(mean(pred - data$Profit)^2) 
accur
#---------------------------------------------------------------------------------------