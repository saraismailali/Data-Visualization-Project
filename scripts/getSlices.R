getSlices <- function(countedPassenger, input){
  choiceClass <- input$pclassIn;
  choiceSex <- input$sexIn;
  choiceAge <- input$ageIn;
  sclices <- c(0,0);
  lbls <- c();
  
  #all
  if(choiceClass == "all" && choiceSex == "all" && choiceAge == "all"){
    slices <- c(countedPassenger$allPassenger$survived, countedPassenger$allPassenger$dead)
  }
  # class
  if(choiceClass != "all" && choiceSex == "all" && choiceAge == "all"){
    # first class
    if(choiceClass == "first"){
      slices <- c(countedPassenger$firstClassPassenger$survived, countedPassenger$firstClassPassenger$dead)
    }
    # secound class
    if(choiceClass == "secound"){
      slices <- c(countedPassenger$secoundClassPassenger$survived, countedPassenger$secoundClassPassenger$dead)
    }
    # third class
    if(choiceClass == "third"){
      slices <- c(countedPassenger$thirdClassPassenger$survived, countedPassenger$thirdClassPassenger$dead)
    }
  }
  
  # sex
  if(choiceClass == "all" && choiceSex != "all" && choiceAge == "all"){
    # female
    if(choiceSex == "female"){
      slices <- c(countedPassenger$femalePassenger$survived, countedPassenger$femalePassenger$dead)
    }
    # male
    if(choiceSex == "male"){
      slices <- c(countedPassenger$malePassenger$survived, countedPassenger$malePassenger$dead)
    }
  }
  
  # age
  if(choiceClass == "all" && choiceSex == "all" && choiceAge != "all"){
    # children
    if(choiceAge == "children"){
      slices <- c(countedPassenger$childPassenger$survived, countedPassenger$childPassenger$dead)
    }
    # adults
    if(choiceAge == "adults"){
      slices <- c(countedPassenger$adultPassenger$survived, countedPassenger$adultPassenger$dead)
    }
  }
  
  # class sex
  if(choiceClass != "all" && choiceSex != "all" && choiceAge == "all"){
    # first class female
    if(choiceClass == "first" && choiceSex == "female"){
      slices <- c(countedPassenger$firstClassFemalePassenger$survived, countedPassenger$firstClassFemalePassenger$dead)
    }
    # first class male
    if(choiceClass == "first" && choiceSex == "male"){
      slices <- c(countedPassenger$firstClassMalePassenger$survived, countedPassenger$firstClassMalePassenger$dead)
    }
    # secound class female
    if(choiceClass == "secound" && choiceSex == "female"){
      slices <- c(countedPassenger$secoundClassFemalePassenger$survived, countedPassenger$secoundClassFemalePassenger$dead)
    }
    # secound class male
    if(choiceClass == "secound" && choiceSex == "male"){
      slices <- c(countedPassenger$secoundClassMalePassenger$survived, countedPassenger$secoundClassMalePassenger$dead)
    }
    # third class female
    if(choiceClass == "third" && choiceSex == "female"){
      slices <- c(countedPassenger$thirdClassFemalePassenger$survived, countedPassenger$thirdClassFemalePassenger$dead)
    }
    # third class male
    if(choiceClass == "third" && choiceSex == "male"){
      slices <- c(countedPassenger$thirdClassMalePassenger$survived, countedPassenger$thirdClassMalePassenger$dead)
    }
  }
  
  # class age
  if(choiceClass != "all" && choiceSex == "all" && choiceAge != "all"){
    # first class children
    if(choiceClass == "first" && choiceAge == "children"){
      slices <- c(countedPassenger$firstClassChildPassenger$survived, countedPassenger$firstClassChildPassenger$dead)
    }
    # first class adults
    if(choiceClass == "first" && choiceAge == "adults"){
      slices <- c(countedPassenger$firstClassAdultPassenger$survived, countedPassenger$firstClassAdultPassenger$dead)
    }
    # secound class children
    if(choiceClass == "secound" && choiceAge == "children"){
      slices <- c(countedPassenger$secoundClassChildPassenger$survived, countedPassenger$secoundClassChildPassenger$dead)
    }
    # secound class adults
    if(choiceClass == "secound" && choiceAge == "adults"){
      slices <- c(countedPassenger$secoundClassAdultPassenger$survived, countedPassenger$secoundClassAdultPassenger$dead)
    }
    # third class children
    if(choiceClass == "third" && choiceAge == "children"){
      slices <- c(countedPassenger$thirdClassChildPassenger$survived, countedPassenger$thirdClassChildPassenger$dead)
    }
    # third class adults
    if(choiceClass == "third" && choiceAge == "adults"){
      slices <- c(countedPassenger$thirdClassAdultPassenger$survived, countedPassenger$thirdClassAdultPassenger$dead)
    }
  }
  
  # sex age
  if(choiceClass == "all" && choiceSex != "all" && choiceAge != "all"){
    # female children
    if(choiceSex == "female" && choiceAge == "children"){
      slices <- c(countedPassenger$femaleChildPassenger$survived, countedPassenger$femaleChildPassenger$dead)
    }
    # male children
    if(choiceSex == "male" && choiceAge == "children"){
      slices <- c(countedPassenger$maleChildPassenger$survived, countedPassenger$maleChildPassenger$dead)
    }
    # female adults
    if(choiceSex == "female" && choiceAge == "adults"){
      slices <- c(countedPassenger$femaleAdultPassenger$survived, countedPassenger$femaleAdultPassenger$dead)
    }
    # male adults
    if(choiceSex == "male" && choiceAge == "adults"){
      slices <- c(countedPassenger$maleAdultPassenger$survived, countedPassenger$maleAdultPassenger$dead)
    }
  }
  
  # class sex age
  if(choiceClass != "all" && choiceSex != "all" && choiceAge != "all"){
    # first class female children
    if(choiceClass == "first" && choiceSex == "female" && choiceAge == "children"){
      slices <- c(countedPassenger$firstClassFemaleChildPassenger$survived, countedPassenger$firstClassFemaleChildPassenger$dead)
    }
    # first class male children
    if(choiceClass == "first" && choiceSex == "male" && choiceAge == "children"){
      slices <- c(countedPassenger$firstClassMaleChildPassenger$survived, countedPassenger$firstClassMaleChildPassenger$dead)
    }
    # first class female adult
    if(choiceClass == "first" && choiceSex == "female" && choiceAge == "adults"){
      slices <- c(countedPassenger$firstClassFemaleAdultPassenger$survived, countedPassenger$firstClassFemaleAdultPassenger$dead)
    }
    # first class adult
    if(choiceClass == "first" && choiceSex == "male" && choiceAge == "adults"){
      slices <- c(countedPassenger$firstClassMaleAdultPassenger$survived, countedPassenger$firstClassMaleAdultPassenger$dead)
    }
    
    # secound class female children
    if(choiceClass == "secound" && choiceSex == "female" && choiceAge == "children"){
      slices <- c(countedPassenger$secoundClassFemaleChildPassenger$survived, countedPassenger$secoundClassFemaleChildPassenger$dead)
    }
    # secound class male children
    if(choiceClass == "secound" && choiceSex == "male" && choiceAge == "children"){
      slices <- c(countedPassenger$secoundClassMaleChildPassenger$survived, countedPassenger$secoundClassMaleChildPassenger$dead)
    }
    # secound class female adult
    if(choiceClass == "secound" && choiceSex == "female" && choiceAge == "adults"){
      slices <- c(countedPassenger$secoundClassFemaleAdultPassenger$survived, countedPassenger$secoundClassFemaleAdultPassenger$dead)
    }
    # secound class adult
    if(choiceClass == "secound" && choiceSex == "male" && choiceAge == "adults"){
      slices <- c(countedPassenger$secoundClassMaleAdultPassenger$survived, countedPassenger$secoundClassMaleAdultPassenger$dead)
    }
    
    # third class female children
    if(choiceClass == "third" && choiceSex == "female" && choiceAge == "children"){
      slices <- c(countedPassenger$thirdClassFemaleChildPassenger$survived, countedPassenger$thirdClassFemaleChildPassenger$dead)
    }
    # third class male children
    if(choiceClass == "third" && choiceSex == "male" && choiceAge == "children"){
      slices <- c(countedPassenger$thirdClassMaleChildPassenger$survived, countedPassenger$thirdClassMaleChildPassenger$dead)
    }
    # third class female adult
    if(choiceClass == "third" && choiceSex == "female" && choiceAge == "adults"){
      slices <- c(countedPassenger$thirdClassFemaleAdultPassenger$survived, countedPassenger$thirdClassFemaleAdultPassenger$dead)
    }
    # third class adult
    if(choiceClass == "third" && choiceSex == "male" && choiceAge == "adults"){
      slices <- c(countedPassenger$thirdClassMaleAdultPassenger$survived, countedPassenger$thirdClassMaleAdultPassenger$dead)
    }
  }
  
  #print(slices)
  return(slices)
}