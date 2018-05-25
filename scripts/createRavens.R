# Create Ravens

create.ravens.dataset <- function(){
  filenames <- list.files(pattern = "ravens_data.csv")
  bigdata <- do.call("rbind", lapply(filenames, read.csv, header = TRUE))
  write.csv(bigdata,"../../../data/aggregated_data/RavensData_icmpc18.csv")
}
