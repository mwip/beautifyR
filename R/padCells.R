#' @import stringr
#'
#' @export
padCells <- function(ce, al = align, mch = maxChars, mco = maxColumns){
  # transpose list: http://r.789695.n4.nabble.com/transpose-lists-td4660695.html
  ceT <- do.call(c, apply(do.call(rbind, ce), 2, list))

  # padding from alignment
  padding <- ifelse(al == "l", "right", ifelse(al == "r", "left", "both"))

  # pad all columns
  ceT <- lapply(1:length(ceT), function(x){
    str_pad(string = ceT[[x]], width = mch[x], side = padding[x], pad = " ")
  })

  # "re"transpose list
  ce <- do.call(c, apply(do.call(rbind, ceT), 2, list))

  # return padded cells
  return(ce)
}
