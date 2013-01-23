outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
outcome[, 11] <- as.numeric(outcome[, 11])
outcome2 <- subset(outcome, table(outcome$State)[State] >= 20)
#outcome2 <- subset(outcome, State %in% names(table(State)[table(State)>20]))
#statesOverThreshold <- names(subset(table(outcome$State), table(outcome$State) >= 20))
#outcome2 <- outcome[outcome$State %in% statesOverThreshold, ]

death <- outcome2[, 11]
state <- outcome2$State
par(las=2)

outcome2$Death <- as.numeric(outcome2[, 11])
#bymedian <- with(outcome2, reorder(state, death, median, na.rm=T))
bymedian <- reorder(state, death, median, na.rm=T)
boxplot(death ~ bymedian, data = outcome2, xaxt="n", 
        ylab="30-day Death Rate", main="Heart Attack 30-day Death Rate by State")
#axis(1, cex.axis=0.5, labels=levels(bymedian), at=1:length(levels(bymedian)))

axis(1, cex.axis=0.7, labels=paste(levels(bymedian), " (", table(bymedian)[levels(bymedian)], ")") , at=1:length(levels(bymedian)))

#boxplot(death ~ state)
#title(main="Heart Attack 30-day Death Rate by State", ylab="30-day Death Rate")