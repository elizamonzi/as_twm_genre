
create.gmsi.dataset <- function(){
  filenames <- list.files(pattern = "data")
  bigdata <- do.call("rbind", lapply(filenames, read.csv, header = TRUE))
  write.csv(bigdata,"../../../data/aggregated_data/Gold_MSI_icmpc18.csv")
}