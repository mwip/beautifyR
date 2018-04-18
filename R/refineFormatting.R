#' @export
refineFormatting <- function(ce, al = align){
  ce[[2]] <- gsub(".", "-", ce[[2]])
  ce[[2]][al == "l"] <- gsub("^-", ":", ce[[2]][al =="l"])
  ce[[2]][al == "c"] <- gsub("^-|-$", ":", ce[[2]][al =="c"])
  ce[[2]][al == "r"] <- gsub("-$", ":", ce[[2]][al =="r"])
  return(ce)
}
