count <- function(cause = NULL) {
  ## Check that "cause" is non-NULL; else throw error
  ## Check that specific "cause" is allowed; else throw error
  ## Read "homicides.txt" data file
  ## Extract causes of death
  ## Return integer containing count of homicides for that cause
  
  i <- switch(
    cause,
    "asphyxiation" = "asphyxiation",
    "blunt force" = "blunt force",
    "other" = "other",
    "shooting" = "shooting",
    "stabbing" = "stabbing",
    "unknown" = "unknown"    
  )
  
  if (is.null(i))
  {
    stop("invalid cause")
  }
  
  i = paste("cause: ",i,sep = "")
  print(i)
  
  d <- readLines("homicides.txt")
    
  length(grep(i, d,  ignore.case = T))
  
}