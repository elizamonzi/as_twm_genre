# Score Tone Span 

score.tone.span <- function(fns=list.files(pattern=".txt")){
 
 # Set up the For Loop to score all of the .csv files in the directory
  for(i in seq(along=fns)){
   
    toneTrial <- read_eprime(fns[i])
    print(paste("Now Working On File",fns[i]))
    
    toneFL <- FrameList(toneTrial)
    tone.length <- length(toneFL)
    
    # Subject
    subjectNo <- as.numeric(toneFL[[tone.length]]$Subject)
    
    # Data 
    toneFiltered <- filter_in(toneFL, "Running", "OSPAN")
    
    
     tSpanPartialScore  <- as.numeric(toneFiltered[[4]]$OspanPartialScore)
     tsMathErrorTotal   <- as.numeric(toneFiltered[[4]]$MathErrorTotal)
     tspanPartialBlock1 <- as.numeric(toneFiltered[[4]]$OspanPartialScoreBlock1)
     tspanPartialBlock2 <- as.numeric(toneFiltered[[4]]$OspanPartialScoreBlock2)
     tspanPartialBlock3 <- as.numeric(toneFiltered[[4]]$OspanPartialScoreBlock3)
     tsAccErrorTotal    <- as.numeric(toneFiltered[[4]]$AccErrorTotal)
     tsSpeedErrorTotal  <- as.numeric(toneFiltered[[4]]$SpeedErrorTotal)
     tsAbsoluteScore    <- as.numeric(toneFiltered[[4]]$OspanAbsoluteScore)
     tsStrats           <- toneFiltered[[4]]$Strategy.RESP
     
     
     if(length(tsStrats) == 0){
       tsStrats <- "No Data Collected"
     }
     
    tone.data <- cbind(subjectNo,tSpanPartialScore,
                                 tsMathErrorTotal,
                                 tspanPartialBlock1,
                                 tspanPartialBlock2,
                                 tspanPartialBlock3,
                                 tsAccErrorTotal,
                                 tsSpeedErrorTotal,
                                 tsAbsoluteScore,
                                 tsStrats)
                       
                       
                       
    
    
     
    write.table(tone.data,paste0(substr(fns[i],1,nchar(fns[i])-4),"tone_data.csv"),sep=",",col.names=TRUE,row.names=FALSE)
    
    
  }
  
}
