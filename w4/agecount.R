agecount <- function(age = NULL) {
  ## Check that "age" is non-NULL; else throw error
  ## Read "homicides.txt" data file
  ## Extract ages of victims; ignore records where no age is
  ## given
  ## Return integer containing count of homicides for that age
  d <- readLines("homicides.txt")
  
  i = paste(" ", age, " years old",sep = "")
  
  length(grep(i, d,  ignore.case = T))
}