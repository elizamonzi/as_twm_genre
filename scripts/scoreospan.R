#--------------------------------------------------
# Score OSpan 
#--------------------------------------------------

score.o.span <- function(fns=list.files(pattern=".txt")){
 
  for(i in seq(along=fns)){
   
    ospanTrial <- read_eprime(fns[i])
    print(paste("Now Working On File",fns[i]))
      
   
    ospanFL <- FrameList(ospanTrial)
    ospanFLfiltered <- filter_in(ospanFL, "Running","OSPAN")
    
    # Subject 
    ospanSubjectFilter <- filter_in(ospanFL, "Running","Header")
    subjectNo <- ospanSubjectFilter[[1]]$Subject
    
    # Data 
    our.length <- length(ospanFLfiltered)
    

    MathErrorTotal <- as.numeric(ospanFLfiltered[[our.length]]$MathErrorTotal)
    OspanScore <- as.numeric(ospanFLfiltered[[our.length]]$OspanScore)
    OspanTotal <- as.numeric(ospanFLfiltered[[our.length]]$OspanTotal)
    SpeedErrorTotal <- as.numeric(ospanFLfiltered[[our.length]]$SpeedErrorTotal)
    AccErrorTotal <- as.numeric(ospanFLfiltered[[our.length]]$AccErrorTotal)
    OspanPartialScore <- as.numeric(ospanFLfiltered[[our.length]]$OspanPartialScore)
    OspanPartialBlock1 <- as.numeric(ospanFLfiltered[[our.length]]$OspanPartialScoreBlock1)
    OspanPartialBlock2 <- as.numeric(ospanFLfiltered[[our.length]]$OspanPartialScoreBlock2)
    OspanPartialBlock3 <- as.numeric(ospanFLfiltered[[our.length]]$OspanPartialScoreBlock3)
    OspanAbsoluteScore <- as.numeric(ospanFLfiltered[[our.length]]$OspanAbsoluteScore)
    MathCorrect <- as.numeric(ospanFLfiltered[[our.length]]$MathCorrect)
    
    
    ospan.data <- cbind(subjectNo,
                        MathErrorTotal,
                        OspanScore,
                        OspanTotal,
                        SpeedErrorTotal,
                        AccErrorTotal,
                        OspanPartialScore,
                        OspanPartialBlock1,
                        OspanPartialBlock2,
                        OspanPartialBlock3,
                        OspanAbsoluteScore,
                        MathCorrect)
    
    write.table(ospan.data,paste0(substr(fns[i],1,nchar(fns[i])-4),"ospan_data.csv"),sep=",",col.names=TRUE,row.names=FALSE)
 }
  
}