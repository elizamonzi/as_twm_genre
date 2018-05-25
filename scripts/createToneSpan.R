# Create Tone Span Data 

create.tone.dataset <- function(){
  filenames <- list.files(pattern = "tone_data.csv")
  bigdata <- do.call("rbind", lapply(filenames, read.csv, header = TRUE))
  write.csv(bigdata,"../../../data/aggregated_data/ToneSpanData_icmpc18.csv")
}