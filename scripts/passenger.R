passenger <- function(titanicData, myRow){
  #print(titanicData$pclass[myRow])
  pclass <- titanicData$pclass[myRow]
  #pclass <- 1
  sex <- titanicData$sex[myRow]
  age <- titanicData$age[myRow]
  survived <- titanicData$survived[myRow]
  
  pass <- list("pclass" = pclass, "sex" = sex, "age" = age, "survived" = survived)
  return(pass)
}

#titanicData <- read.csv2("titanic2/data/titanic.csv")
#test <- passenger(titanicData, 3)
#print(test$pclass)