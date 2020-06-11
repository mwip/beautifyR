#' Beatuify Comment Addin
#'
#' Addin for beautifyR. It collects the selected R code and hands it off
#' to the `beautifyComment` function.
#'
#' @import rstudioapi
#' @keywords internal

beautifycommentaddin <- function(){
  con <- rstudioapi::getActiveDocumentContext()
  
  # Get selection text
  con_text <- con$selection[[1]]$text
  
  # beautify and return the selection
  rstudioapi::insertText(beautifyComment(con_text))
}
