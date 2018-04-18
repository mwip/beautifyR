#' @export
extractAlignment <- function(cells, mc = maxColumns){
  ind <- cells[[2]]

  # create empty alignment vector
  align <- rep("l", max(length(ind), mc))
  align[grepl("-:", ind)] <- "r"
  align[grepl(":-+:", ind)] <- "c"
  return(align)
}
