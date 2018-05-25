#--------------------------------------------------
# Create Qualtrics Data

create.qualtrics <- function(){
    
    # Import Spring Data (Has Fall Data)
    beatDataSpring <- fread("Beat_April7.csv", header = TRUE)
    melodicDataSpring <- fread("Melodic_April7.csv",header = TRUE)

    #--------------------------------------------------
    # Spring
    # Clean Beat
    beatSpring <- beatDataSpring[,.(Q62,SC8)]
    setnames(beatSpring,c("Q62","SC8"), c("subjectNo","beatScore"))
    dropBottomBeatSpring <- beatSpring[!(.N):(.N-1)]
    finalBeatSpring <- dropBottomBeatSpring

    # Clean Melodic
    melodicSpring <- melodicDataSpring[,.(Q70, SC9)]
    setnames(melodicSpring,c("Q70", "SC9"), c("subjectNo","melodicScore"))
    dropBottomMelodicSpring <- melodicSpring[!(.N):(.N-1)]
    finalMelodicSpring <- dropBottomMelodicSpring
    

    #--------------------------------------------------
    
    # Write Files 
      fwrite(finalBeatSpring, "../../aggregated_data/BeatData_icmpc18.csv")

      fwrite(finalMelodicSpring, "../../aggregated_data/MelodicData_icmpc18.csv")
}