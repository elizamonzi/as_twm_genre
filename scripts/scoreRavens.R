# score ravens

score.ravens <- function(fns=list.files(pattern=".txt")){
  
  for(i in seq(along=fns)){
    
    ravensTrial <- read_eprime(fns[i])
    print(paste("Now Working On File",fns[i]))
    
    ravensFL <- FrameList(ravensTrial)
    
     
    # Subject 
    ravensSubjectFilter <- filter_in(ravensFL, "Running","Header")
    subjectBlock <- ravensSubjectFilter[1]
    subjectNo <- as.numeric(subjectBlock[[1]]$Subject)
    
    # Data
    ravensScoreBlock <- ravensFL[[length(ravensFL)]]
    
    ravensScoreA <- as.numeric(ravensScoreBlock$Score_A)
    ravensScoreB <- as.numeric(ravensScoreBlock$Score_B)
    ravensScoreC <- as.numeric(ravensScoreBlock$Score_C)
    ravenAge <- ravensScoreBlock$Age
    ravenSex <- ravensScoreBlock$Sex
    ravenHand <- ravensScoreBlock$Handedness
    ravenHearing <- ravensScoreBlock$Hearing
    ravenVision <- ravensScoreBlock$Vision
    
    ravens.data <- cbind(subjectNo,ravensScoreA, ravensScoreB, ravensScoreC, ravenAge, ravenSex, ravenHand, ravenHearing, ravenVision)
    
    write.table(ravens.data,paste0(substr(fns[i],1,nchar(fns[i])-4),"ravens_data.csv"),sep=",",col.names=TRUE,row.names=FALSE)
    
    
    
  }
  
}

