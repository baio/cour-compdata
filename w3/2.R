#hist(outcome[,11], xlab="30-day Death Rate", ylab="Heart Attack 30-day Death Rayte", main="Heart Attack 30-day Death Rate")
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
#Number 19 (\Outcome of Care Measures.csv")
#1. Identify which columns of the data frame contain the 30-day death rate from heart attack, heart failure,
#and pneumonia.
#13 lower mortality estimate
#14 upper mortality estimate
#15 number of patients mortality estimate (heart attack)

#21 by heart failure
#27 pneumonia

res.ha <- as.numeric(outcome[,11])
res.hf <- as.numeric(outcome[,17])
res.p <- as.numeric(outcome[,23])

#2. Coerce these columns to be numeric using the as.numeric function as above. You may receive warnings
#about NAs but that is okay.

#3. Make histograms of the death rates for each outcome and put the histograms on the same plot window.
#This can be done by running par(mfrow = c(3, 1)) before calling hist. This sets the plot window
#to have 3 rows and 1 column.

#ylab="Frequency", xlab="30−day Death Rate", 
xlim <- range(c(res.ha, res.hf, res.p), na.rm = T)
par(mfrow = c(3, 1))
hist(res.ha, main="Heart Attack", xlab="30−day Death Rate", ylab="Frequency", xlim=xlim)
hist(res.hf, main="Heart Failure", xlab="30−day Death Rate", ylab="Frequency", xlim=xlim)
hist(res.p, main="Pneumonia", xlab="30−day Death Rate", ylab="Frequency", xlim=xlim)

hist(ylab="Frequency", xlab="30−day Death Rate")


