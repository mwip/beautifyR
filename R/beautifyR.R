#' @import stringr
#' @export
beautifyR <- function(inputstring){
  # split table at "\n"
  lines <- as.list(stringr::str_split(inputstring, "\n", simplify = TRUE))
  lines <- gsub("^ | $", "", lines)

  # split lines at "|"
  cells <- lapply(stringr::str_split(lines, "\\|"), function(x){
    x[x != ""]
  })

  # count number of cells in each row
  ncolumns <- lapply(cells, length)
  maxColumns <- do.call(max, ncolumns)

  # if no or false alignment row is given return left-aligned and show warning
  if (any(stringr::str_detect(cells[[2]], "[^:-[[:blank:]]]"))){
    cells <- append(cells, list(rep(":-", maxColumns)), 1)
    warning("Fomatting indicator row 2 (e.g. :----) contains invalid values or is not available
  left alignment assumed for all columns", immediate. = TRUE)
  }

  # extract or assume the column alignment (left, center, right)
  align <- extractAlignment(cells, maxColumns)

  # remove spaces at beginning and end of cells
  cells <- lapply(cells, function(x){
    gsub("^[[:blank:]]*|[[:blank:]]*$", "", x)
  })

  # extract maximum characters per column
  chars <- lapply(cells, nchar)
  maxChars <- sapply(1:maxColumns, function(x){
    do.call(max, lapply(chars, `[`, x))
  })

  # Increase too low number of chars
  maxChars[is.na(maxChars)| maxChars < 3] <- 3

  ## build output table
  # TODO insert missing columns


  # pad cells
  cellsPadded <- padCells(cells, align, maxChars, maxColumns)

  # refine formatting row (2nd)
  cellsPaddedRefined <- refineFormatting(cellsPadded, align)

  # combine lines
  linesout <- lapply(cellsPaddedRefined, function(x){
    paste("|",
          paste(x, collapse = " | "),
          "|")
    })

  # create output string
  out <- paste(unlist(linesout), collapse = "\n")
  return(out)
}
