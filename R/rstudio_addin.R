#' Beatuify Addin
#'
#' @keywords internal

beautifyaddin <- function(){
  con <- rstudioapi::getActiveDocumentContext()

  # Get selection text
  con_text <- con$selection[[1]]$text

  # beautify and return the selection
  rstudioapi::insertText(beautifyR(con_text))
}
