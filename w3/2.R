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

res.ha <- res.ha[!is.na(res.ha)]
res.hf <- res.hf[!is.na(res.hf)]
res.p <- res.p[!is.na(res.p)]


#2. Coerce these columns to be numeric using the as.numeric function as above. You may receive warnings
#about NAs but that is okay.

#3. Make histograms of the death rates for each outcome and put the histograms on the same plot window.
#This can be done by running par(mfrow = c(3, 1)) before calling hist. This sets the plot window
#to have 3 rows and 1 column.

#ylab="Frequency", xlab="30−day Death Rate", 
xlim <- range(c(res.ha, res.hf, res.p))
par(mfrow = c(3, 1))
z <- mean(res.ha)
hist(res.ha, main=substitute("Heart attack (" * bar(X) == k * ")", list(k=mean(res.ha))), xlab="30-??day Death Rate", ylab="Frequency", xlim=xlim)
abline(v = median(res.ha))
hist(res.hf, main=substitute("Heart failure (" * bar(X) == k * ")", list(k=mean(res.hf))), xlab="30-??day Death Rate", ylab="Frequency", xlim=xlim)
abline(v = median(res.hf))
hist(res.p, main=substitute("Pneumonia (" * bar(X) == k * ")", list(k=mean(res.p))), xlab="30-??day Death Rate", ylab="Frequency", xlim=xlim)
abline(v = median(res.p))


