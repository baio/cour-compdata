rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
  i <- switch(
    outcome,
    "heart attack" = 11,
    "heart failure" = 17,
    "pneumonia" = 23 
  )
  
  if (is.null(i))
  {
    stop("invalid outcome")
  }
  
  d <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  d <- d[which(d$State==state),]   
  
  d[,11] <- as.numeric(d[,11])
  d[,17] <- as.numeric(d[,17])
  d[,23] <- as.numeric(d[,23])
  
  d <- d[is.na(d[,i]) == F,]
  
  if (num == "best")
  {
    num = 1
  }
  
  if (num == "worst")
  {
    num = nrow(d)  
  }
  
  
  states <- sort(unique(d$State))
  
  z <- lapply(states, function(state) {
    x <- d[which(d$State==state),]
    n = num
    if (num == "best")
    {
      n = 1
    }    
    if (num == "worst")
    {
      n = nrow(x)  
    }    
    x[order(x[i], x["Hospital.Name"]), ][n, c(2, 7)]
    })
  
  z
}