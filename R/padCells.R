#' paddCells
#'
#' Function to padd cells according to the column width
#'
#' @param ce list of table cells as created by `beautifyR`
#' @param al alignment vector as created by `extractAlignment`
#' @param mch maximum number of characters in each column (defines the column
#' width)
#' @param mco number of columns in the table
#' @import stringr
#' @keywords internal
#' @export
padCells <- function(ce, al, mch, mco) {
  # fill missing cells with space
  ce <- lapply(ce, function(x) {
    if (length(x) < mco) {
      message("Empty columns were found.
Column was created left-aligned")
      c(x, rep(" ", mco - length(x)))
    } else {
      x
    }
  })

  # reset formatting column ~~ needed for shrinking
  ce[[2]] <- rep("---", mco)

  # transpose list: http://r.789695.n4.nabble.com/transpose-lists-td4660695.html
  ceT <- do.call(c, apply(do.call(rbind, ce), 2, list))

  # padding from alignment
  padding <- ifelse(al == "l", "right", ifelse(al == "r", "left", "both"))

  # pad all columns
  ceT <- lapply(1:length(ceT), function(x) {
    stringr::str_pad(string = ceT[[x]], width = mch[x],
                     side = padding[x], pad = " ")
  })

  # "re"transpose list
  ce <- do.call(c, apply(do.call(rbind, ceT), 2, list))

  # return padded cells
  return(ce)
}
