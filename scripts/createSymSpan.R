# Create Symmetry 

create.sym.dataset <- function(){
  filenames <- list.files(pattern = "sym_data.csv")
  bigdata <- do.call("rbind", lapply(filenames, read.csv, header = TRUE))
  write.csv(bigdata,"../../../data/aggregated_data/SymmetrySpanData_icmpc18.csv")
}
