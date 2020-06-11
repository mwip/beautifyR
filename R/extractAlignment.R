#' Extract Alignment from list of RMarkdown table cells cells
#'
#' The function extracts the alignment of all cells and returns a character
#' vector indicating left, right or center alignment for all columns according
#' to the formatting line (i.e. the 2nd)
#'
#' @param cells list of markdown cells created within `beautifyR`
#' @param mc number of columns within the RMarkdown table, derived within
#' `beautifyR`
#' @keywords internal
#' @export
extractAlignment <- function(cells, mc){
  ind <- cells[[2]]

  # create empty alignment vector
  align <- rep("l", max(length(ind), mc))
  align[grepl("-:", ind)] <- "r"
  align[grepl(":-+:", ind)] <- "c"
  return(align)
}
