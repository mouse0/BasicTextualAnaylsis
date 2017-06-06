# Import the provided data and set the falsePositive/trueNegative/etc
# counters to 0
data = read.csv("Homework01.calculate.tp.fp.fn.csv")
falsePositive = 0
falseNegative = 0
truePositive = 0
trueNegative = 0

# define the functions to calculate precision, recall, and fmeasure
# Also a fairly arbirtrary function to increment the counters, which
# I imagined would make my code more elegant but did not

increment <- function(x) { x <- x + 1 }
precision <- function(trueP, falseP) {
  trueP / (trueP + falseP)
}

recall <- function(trueP, falseN) {
  trueP / (trueP + falseN)
}

# In which beta is the wieght, assigned to 1 in the function
# call below

fMeasure <- function(beta, precision, recall) {
  numerator = ((beta ^ 2) + 1) * precision * recall
  denominator = ((beta ^ 2) *  precision) + recall
  numerator / denominator
}

#iterate through the dataset and determine the numbers of false 
# positives, false Negatives, true positives, and false positives

for (i in 1:nrow(data)) {
  if ((data[i, 1] == ("n")) & (data[i, 2] == ("y"))) {
    falsePositive <- increment(falsePositive)
  } else if ((data[i, 1] == ("y")) & (data[i, 2] == ("n"))) {
    falseNegative <- increment(falseNegative)
  } else if ((data[i, 1] == ("y")) & (data[i, 2] == ("y"))) {
    truePositive <- increment(truePositive)
  } else {
    trueNegative <- increment(trueNegative)
  }
}


# call the corresponding functions to calculate the precision,
# recall, and fmeasure

prec <- precision(truePositive, falsePositive)
rec <- recall(truePositive, falseNegative)
beta <- 1
fMeas <- fMeasure(beta, prec, rec)

# calculate the accuracy
accuracy <- (truePositive + trueNegative) / (truePositive + trueNegative + falseNegative + falsePositive)

# Print the results
print(fMeas)
print(rec)
print(prec)
print(accuracy)
