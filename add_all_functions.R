add_all_functions <- function(){
  pathnames <- list.files(pattern="[.]R$", path="~/swing_miss_percentage", full.names=TRUE)
  sapply(pathnames, FUN=source)
}