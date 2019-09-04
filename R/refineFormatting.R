#' refineFomatting
#'
#' Function to refine the width of the formatting row (i.e. the 2nd). It
#' contains information about alignment of the table columns.
#'
#' @param ce list of table cells as created by `beautifyR`
#' @param al alignment vector as created by `extractAlignment`
#' @keywords internal
#' @export
refineFormatting <- function(ce, al = align){
  ce[[2]][is.na(ce[[2]])] <- " "
  ce[[2]] <- gsub(".", "-", ce[[2]])
  ce[[2]][al == "l"] <- gsub("^-", ":", ce[[2]][al == "l"])
  ce[[2]][al == "c"] <- gsub("^-|-$", ":", ce[[2]][al == "c"])
  ce[[2]][al == "r"] <- gsub("-$", ":", ce[[2]][al == "r"])
  return(ce)
}
