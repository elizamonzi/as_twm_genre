#--------------------------------------------------
# Create ospan dataset
#--------------------------------------------------

create.ospan.dataset <- function(){
  filenames <- list.files(pattern = "ospan_data.csv")
  bigdata <- do.call("rbind", lapply(filenames, read.csv, header = TRUE))
  write.csv(bigdata,"../../../data/aggregated_data/OSPAN_data_icmpc18.csv")
}