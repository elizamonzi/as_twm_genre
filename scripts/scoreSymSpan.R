# Score Sym Span 

score.sym.span <- function(fns=list.files(pattern=".txt")){
 
  for(i in seq(along=fns)){
   
    symmetryTrial <- read_eprime(fns[i])
    print(paste("Now Working On File",fns[i]))
  
    symmetryFL_sym <- FrameList(symmetryTrial)
    symmetryFLfiltered <- filter_in(symmetryFL_sym, "Running","Symmetry")
    
    # Subject 
    SymmetrySubjectFilter <- filter_in(symmetryFL_sym, "Running","Header")
    subjectNo <- SymmetrySubjectFilter[[1]]$Subject
    
    # Data 
    symmetryScoreBlock <- symmetryFLfiltered[5]
    
    symErrorTotal <- as.numeric(symmetryScoreBlock[[1]]$SymmErrorTotal)
    symAccErrorTotal <- as.numeric(symmetryScoreBlock[[1]]$AccErrorTotal)
    symSpeedErorTotal <- as.numeric(symmetryScoreBlock[[1]]$SpeedErrorTotal)
    symSspanscore <- as.numeric(symmetryScoreBlock[[1]]$SspanScore)
    symSspanTotal <- as.numeric(symmetryScoreBlock[[1]]$SspanTotal)
    symSspanPartialScore <- as.numeric(symmetryScoreBlock[[1]]$SspanPartialScore)
    symSspanPartialScoreBlock1 <- as.numeric(symmetryScoreBlock[[1]]$SspanPartialScoreBlock1)
    symSspanPartialScoreBlock2 <- as.numeric(symmetryScoreBlock[[1]]$SspanPartialScoreBlock2)
    symSspanPartialScoreBlock3 <- as.numeric(symmetryScoreBlock[[1]]$SspanPartialScoreBlock3)
    symSspanAbsoluteScore <- as.numeric(symmetryScoreBlock[[1]]$SspanAbsoluteScore)
    symSymmetryCorrect <- as.numeric(symmetryScoreBlock[[1]]$SymmetryCorrect)
  
    
    
    sym.data <- cbind(subjectNo,
                      symErrorTotal,symAccErrorTotal, symSpeedErorTotal,
                      symSspanscore, symSspanTotal, symSspanPartialScore, 
                      symSspanPartialScoreBlock1,symSspanPartialScoreBlock2, symSspanPartialScoreBlock3,
                      symSspanAbsoluteScore, symSymmetryCorrect)
    
    write.table(sym.data,paste0(substr(fns[i],1,nchar(fns[i])-4),"sym_data.csv"),sep=",",col.names=TRUE,row.names=FALSE)
  }
  
  }