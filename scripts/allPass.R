allPass <- function(actuellPassenger, survivedAll, deadAll){
  # Survived or dead
  if(actuellPassenger$survived == 1) {
    survivedAll <- survivedAll + 1
  }
  
  if(actuellPassenger$survived == 0){
    deadAll <- deadAll + 1
  }
  
  allPassenger <- list("survived" = survivedAll, "dead" = deadAll)
  
  return(allPassenger)
}