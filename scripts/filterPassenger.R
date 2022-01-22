filterPassenger <- function(titanicData){
  allPassenger <-list("survived" = 0, "dead" = 0);
  femalePassenger <-list("survived" = 0, "dead" = 0);
  malePassenger <-list("survived" = 0, "dead" = 0);
  childPassenger <-list("survived" = 0, "dead" = 0);
  adultPassenger <-list("survived" = 0, "dead" = 0);
  femaleChildPassenger <-list("survived" = 0, "dead" = 0);
  maleChildPassenger <-list("survived" = 0, "dead" = 0);
  femaleAdultPassenger <-list("survived" = 0, "dead" = 0);
  maleAdultPassenger <-list("survived" = 0, "dead" = 0);
  firstClassPassenger <-list("survived" = 0, "dead" = 0);
  secoundClassPassenger <-list("survived" = 0, "dead" = 0);
  thirdClassPassenger <-list("survived" = 0, "dead" = 0);
  firstClassFemalePassenger <-list("survived" = 0, "dead" = 0);
  secoundClassFemalePassenger <-list("survived" = 0, "dead" = 0);
  thirdClassFemalePassenger <-list("survived" = 0, "dead" = 0);
  firstClassMalePassenger <-list("survived" = 0, "dead" = 0);
  secoundClassMalePassenger <-list("survived" = 0, "dead" = 0);
  thirdClassMalePassenger <-list("survived" = 0, "dead" = 0);
  firstClassChildPassenger  <-list("survived" = 0, "dead" = 0);
  secoundClassChildPassenger  <-list("survived" = 0, "dead" = 0);
  thirdClassChildPassenger  <-list("survived" = 0, "dead" = 0);
  firstClassAdultPassenger  <-list("survived" = 0, "dead" = 0);
  secoundClassAdultPassenger  <-list("survived" = 0, "dead" = 0);
  thirdClassAdultPassenger  <-list("survived" = 0, "dead" = 0);
  firstClassFemaleChildPassenger  <-list("survived" = 0, "dead" = 0);
  secoundClassFemaleChildPassenger  <-list("survived" = 0, "dead" = 0);
  thirdClassFemaleChildPassenger  <-list("survived" = 0, "dead" = 0);
  firstClassFemaleAdultPassenger  <-list("survived" = 0, "dead" = 0);
  secoundClassFemaleAdultPassenger  <-list("survived" = 0, "dead" = 0);
  thirdClassFemaleAdultPassenger  <-list("survived" = 0, "dead" = 0);
  firstClassMaleChildPassenger  <-list("survived" = 0, "dead" = 0);
  secoundClassMaleChildPassenger  <-list("survived" = 0, "dead" = 0);
  thirdClassMaleChildPassenger  <-list("survived" = 0, "dead" = 0);
  firstClassMaleAdultPassenger  <-list("survived" = 0, "dead" = 0);
  secoundClassMaleAdultPassenger  <-list("survived" = 0, "dead" = 0);
  thirdClassMaleAdultPassenger  <-list("survived" = 0, "dead" = 0);
  
  for (myRow in 1:1309){
    actuellPassenger <- passenger(titanicData, myRow)
    #print(paste0("class: ", actuellPassenger$pclass))
    
    allPassenger <- allPass(actuellPassenger,allPassenger$survived, allPassenger$dead)
    #print(paste0("survived: ", allPassenger$survived))
    #print(paste0("dead: ", allPassenger$dead))
    
    # Survived or dead first class
    if(actuellPassenger$pclass == 1){
      firstClassPassenger <- allPass(actuellPassenger, firstClassPassenger$survived, firstClassPassenger$dead)
    }
    
    # Survived or dead secound class
    if(actuellPassenger$pclass == 2){
      secoundClassPassenger <- allPass(actuellPassenger, secoundClassPassenger$survived, secoundClassPassenger$dead)
    }
    
    # Survived or dead third class
    if(actuellPassenger$pclass == 3){
      thirdClassPassenger <- allPass(actuellPassenger, thirdClassPassenger$survived, thirdClassPassenger$dead)
    }
    
    if(!is.na(actuellPassenger$sex)){
      # Survived or dead female
      if(actuellPassenger$sex == "female"){
        femalePassenger <- allPass(actuellPassenger, femalePassenger$survived, femalePassenger$dead)
      }
      
      # Survived or dead male
      if(actuellPassenger$sex == "male"){
        malePassenger <- allPass(actuellPassenger, malePassenger$survived, malePassenger$dead)
      }
      
      # Survived or dead first class female
      if(actuellPassenger$pclass == 1 && actuellPassenger$sex == "female"){
        firstClassFemalePassenger <- allPass(actuellPassenger, firstClassFemalePassenger$survived, firstClassFemalePassenger$dead)
      }
      
      # Survived or dead secound class female
      if(actuellPassenger$pclass == 2 && actuellPassenger$sex == "female"){
        secoundClassFemalePassenger <- allPass(actuellPassenger, secoundClassFemalePassenger$survived, secoundClassFemalePassenger$dead)
      }
      
      # Survived or dead third class female
      if(actuellPassenger$pclass == 3 && actuellPassenger$sex == "female"){
        thirdClassFemalePassenger <- allPass(actuellPassenger, thirdClassFemalePassenger$survived, thirdClassFemalePassenger$dead)
      }
      
      # Survived or dead first class male
      if(actuellPassenger$pclass == 1 && actuellPassenger$sex == "male"){
        firstClassMalePassenger <- allPass(actuellPassenger, firstClassMalePassenger$survived, firstClassMalePassenger$dead)
      }
      
      # Survived or dead secound class male
      if(actuellPassenger$pclass == 2 && actuellPassenger$sex == "male"){
        secoundClassMalePassenger <- allPass(actuellPassenger, secoundClassMalePassenger$survived, secoundClassMalePassenger$dead)
      }
      
      # Survived or dead third class male
      if(actuellPassenger$pclass == 3 && actuellPassenger$sex == "male"){
        thirdClassMalePassenger <- allPass(actuellPassenger, thirdClassMalePassenger$survived, thirdClassMalePassenger$dead)
      }
    }
    
    if(!is.na(actuellPassenger$age)){
      #print(actuellPassenger$age)
      # Survived or dead child
      if(actuellPassenger$age <= 18){
        childPassenger <- allPass(actuellPassenger, childPassenger$survived, childPassenger$dead)
      }
      
      # Survived or dead adult
      if(actuellPassenger$age >= 18){
        adultPassenger <- allPass(actuellPassenger, adultPassenger$survived, adultPassenger$dead)
      }
      
      # Survived or dead first class child
      if(actuellPassenger$pclass == 1 && actuellPassenger$age <= 18){
        firstClassChildPassenger <- allPass(actuellPassenger, firstClassChildPassenger$survived, firstClassChildPassenger$dead)
      }
      
      # Survived or dead secound class child
      if(actuellPassenger$pclass == 2 && actuellPassenger$age <= 18){
        secoundClassChildPassenger <- allPass(actuellPassenger, secoundClassChildPassenger$survived, secoundClassChildPassenger$dead)
      }
      
      # Survived or dead third class child
      if(actuellPassenger$pclass == 3 && actuellPassenger$age <= 18){
        thirdClassChildPassenger <- allPass(actuellPassenger, thirdClassChildPassenger$survived, thirdClassChildPassenger$dead)
      }
      
      # Survived or dead first class adult
      if(actuellPassenger$pclass == 1 && actuellPassenger$age >= 18){
        firstClassAdultPassenger <- allPass(actuellPassenger, firstClassAdultPassenger$survived, firstClassAdultPassenger$dead)
      }
      
      # Survived or dead secound class adult
      if(actuellPassenger$pclass == 2 && actuellPassenger$age >= 18){
        secoundClassAdultPassenger <- allPass(actuellPassenger, secoundClassAdultPassenger$survived, secoundClassAdultPassenger$dead)
      }
      
      # Survived or dead third class adult
      if(actuellPassenger$pclass == 3 && actuellPassenger$age >= 18){
        thirdClassAdultPassenger <- allPass(actuellPassenger, thirdClassAdultPassenger$survived, thirdClassAdultPassenger$dead)
      }
    }
    
    if(!is.na(actuellPassenger$age) && !is.na(actuellPassenger$sex)){
      # Survived or dead female child
      if(actuellPassenger$age <= 18 && actuellPassenger$sex == "female"){
        femaleChildPassenger <- allPass(actuellPassenger, femaleChildPassenger$survived, femaleChildPassenger$dead)
      }
      
      # Survived or dead female adult
      if(actuellPassenger$age >= 18 && actuellPassenger$sex == "female"){
        femaleAdultPassenger <- allPass(actuellPassenger, femaleAdultPassenger$survived, femaleAdultPassenger$dead)
      }
      
      # Survived or dead male child
      if(actuellPassenger$age <= 18 && actuellPassenger$sex == "male"){
        maleChildPassenger <- allPass(actuellPassenger, maleChildPassenger$survived, maleChildPassenger$dead)
      }
      
      # Survived or dead male adult
      if(actuellPassenger$age >= 18 && actuellPassenger$sex == "male"){
        maleAdultPassenger <- allPass(actuellPassenger, maleAdultPassenger$survived, maleAdultPassenger$dead)
      }
      
      # Survived or dead first class female child
      if(actuellPassenger$pclass == 1 && actuellPassenger$sex == "female" && actuellPassenger$age <= 18){
        firstClassFemaleChildPassenger <- allPass(actuellPassenger, firstClassFemaleChildPassenger$survived, firstClassFemaleChildPassenger$dead)
      }
      
      # Survived or dead secound class female child
      if(actuellPassenger$pclass == 2 && actuellPassenger$sex == "female" && actuellPassenger$age <= 18){
        secoundClassFemaleChildPassenger <- allPass(actuellPassenger, secoundClassFemaleChildPassenger$survived, secoundClassFemaleChildPassenger$dead)
      }
      
      # Survived or dead third class female child
      if(actuellPassenger$pclass == 3 && actuellPassenger$sex == "female" && actuellPassenger$age <= 18){
        thirdClassFemaleChildPassenger <- allPass(actuellPassenger, thirdClassFemaleChildPassenger$survived, thirdClassFemaleChildPassenger$dead)
      }
      
      # Survived or dead first class female adult
      if(actuellPassenger$pclass == 1 && actuellPassenger$sex == "female" && actuellPassenger$age >= 18){
        firstClassFemaleAdultPassenger <- allPass(actuellPassenger, firstClassFemaleAdultPassenger$survived, firstClassFemaleAdultPassenger$dead)
      }
      
      # Survived or dead secound class female adult
      if(actuellPassenger$pclass == 2 && actuellPassenger$sex == "female" && actuellPassenger$age >= 18){
        secoundClassFemaleAdultPassenger <- allPass(actuellPassenger, secoundClassFemaleAdultPassenger$survived, secoundClassFemaleAdultPassenger$dead)
      }
      
      # Survived or dead third class female adult
      if(actuellPassenger$pclass == 3 && actuellPassenger$sex == "female" && actuellPassenger$age >= 18){
        thirdClassFemaleAdultPassenger <- allPass(actuellPassenger, thirdClassFemaleAdultPassenger$survived, thirdClassFemaleAdultPassenger$dead)
      }
      # Survived or dead first class male child
      if(actuellPassenger$pclass == 1 && actuellPassenger$sex == "male" && actuellPassenger$age <= 18){
        firstClassMaleChildPassenger <- allPass(actuellPassenger, firstClassMaleChildPassenger$survived, firstClassMaleChildPassenger$dead)
      }
      
      # Survived or dead secound class male child
      if(actuellPassenger$pclass == 2 && actuellPassenger$sex == "male" && actuellPassenger$age <= 18){
        secoundClassMaleChildPassenger <- allPass(actuellPassenger, secoundClassMaleChildPassenger$survived, secoundClassMaleChildPassenger$dead)
      }
      
      # Survived or dead third class male child
      if(actuellPassenger$pclass == 3 && actuellPassenger$sex == "male" && actuellPassenger$age <= 18){
        thirdClassMaleChildPassenger <- allPass(actuellPassenger, thirdClassMaleChildPassenger$survived, thirdClassMaleChildPassenger$dead)
      }
      
      # Survived or dead first class male adult
      if(actuellPassenger$pclass == 1 && actuellPassenger$sex == "male" && actuellPassenger$age >= 18){
        firstClassMaleAdultPassenger <- allPass(actuellPassenger, firstClassMaleAdultPassenger$survived, firstClassMaleAdultPassenger$dead)
      }
      
      # Survived or dead secound class female adult
      if(actuellPassenger$pclass == 2 && actuellPassenger$sex == "female" && actuellPassenger$age >= 18){
        secoundClassMaleAdultPassenger <- allPass(actuellPassenger, secoundClassMaleAdultPassenger$survived, secoundClassMaleAdultPassenger$dead)
      }
      
      # Survived or dead third class female adult
      if(actuellPassenger$pclass == 3 && actuellPassenger$sex == "female" && actuellPassenger$age >= 18){
        thirdClassMaleAdultPassenger <- allPass(actuellPassenger, thirdClassMaleAdultPassenger$survived, thirdClassMaleAdultPassenger$dead)
      }
    }
    
  }
  return(list("allPassenger" = allPassenger,
              "femalePassenger" =  femalePassenger,
              "malePassenger" = malePassenger,
              "childPassenger" = childPassenger,
              "adultPassenger" = adultPassenger,
              "femaleChildPassenger" = femaleChildPassenger,
              "maleChildPassenger" = maleChildPassenger,
              "femaleAdultPassenger" = femaleAdultPassenger,
              "maleAdultPassenger" = maleAdultPassenger,
              "firstClassPassenger" = firstClassPassenger,
              "secoundClassPassenger" = secoundClassPassenger,
              "thirdClassPassenger" = thirdClassPassenger,
              "firstClassFemalePassenger" = firstClassFemalePassenger,
              "secoundClassFemalePassenger" = secoundClassFemalePassenger,
              "thirdClassFemalePassenger" = thirdClassFemalePassenger,
              "firstClassMalePassenger" = firstClassMalePassenger,
              "secoundClassMalePassenger" = secoundClassMalePassenger,
              "thirdClassMalePassenger" = thirdClassMalePassenger,
              "firstClassChildPassenger" = firstClassChildPassenger,
              "secoundClassChildPassenger" = secoundClassChildPassenger,
              "thirdClassChildPassenger" = thirdClassChildPassenger,
              "firstClassAdultPassenger" = firstClassAdultPassenger,
              "secoundClassAdultPassenger" = secoundClassAdultPassenger,
              "thirdClassAdultPassenger" = thirdClassAdultPassenger,
              "firstClassFemaleChildPassenger" = firstClassFemaleChildPassenger,
              "secoundClassFemaleChildPassenger" = secoundClassFemaleChildPassenger,
              "thirdClassFemaleChildPassenger" = thirdClassFemaleChildPassenger,
              "firstClassFemaleAdultPassenger" = firstClassFemaleAdultPassenger,
              "secoundClassFemaleAdultPassenger" = secoundClassFemaleAdultPassenger,
              "thirdClassFemaleAdultPassenger" = thirdClassFemaleAdultPassenger,
              "firstClassMaleChildPassenger" = firstClassMaleChildPassenger,
              "secoundClassMaleChildPassenger" = secoundClassMaleChildPassenger,
              "thirdClassMaleChildPassenger" = thirdClassMaleChildPassenger,
              "firstClassMaleAdultPassenger" = firstClassMaleAdultPassenger,
              "secoundClassMaleAdultPassenger" = secoundClassMaleAdultPassenger,
              "thirdClassMaleAdultPassenger" = thirdClassMaleAdultPassenger))
  
}