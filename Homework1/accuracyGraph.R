# import ggplot2 
# and my code (for calculating fmeasure, precision, and recall)
library(ggplot2)
source("~/Code/Homework1/FmeasurePrecisionRecall.R")

# Create some data from the specifications:
# 3 true positives, 1 false negative, an increasing 
# number of false negatives from 0 to 100, no false positives
emergencyData <- data.frame(c(3), c(1), c(0:100))
colnames(emergencyData) <- c("truePositives", "falseNegatives", "trueNegatives")

# Make vectors to store accuracy and fMeasure 
# (fMeasure is  named fMeas to avoid the overwriting tendancy
# of source())
accuracy = c(0:100)
fMeas = c(0:100)

# Iterate through the data, calculating accuracy and fMeasure
# for each row and storing it in the vectors
for (i in 0:101) {
  numerator = emergencyData[i, "truePositives"] + emergencyData[i, "trueNegatives"]
  denominator = numerator + emergencyData[i, "falseNegatives"]
  accuracy[i] = numerator / denominator
  prec <- precision(emergencyData[i, "truePositives"], 0)
  rec <- recall(emergencyData[i, "truePositives"], emergencyData[i, "falseNegatives"])
  fMeas[i] = fMeasure(1, prec, rec) 
}

# plot the calculated accuracy and fmeasure on the y axis,
# with the y range set to [0.0, 1.0]
qplot(0:100, accuracy, ylim = c(0.0, 1.0)) + geom_point(aes(color = accuracy)) 
qplot(0:100, fMeas, ylim = c(0.0, 1.0)) + geom_point(aes(color = fMeas))
