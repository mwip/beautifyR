#' Beautify RMarkdown tables
#'
#' This function beautifys RMarkdown tables in terms of columnwidths, alignment,
#' missing columns and rows. It is called by the beautifyR RStudio addin.
#'
#' @import stringr
#' @param inputstring Charactervector of length 1 containing a RMarkdown table.
#' @export
beautifyR <- function(inputstring){
  # split table at "\n"
  lns <- as.list(stringr::str_split(inputstring, "\n", simplify = TRUE))
  # ignore empty lines
  lns <- lns[unlist(lapply(lns, function(x){x != ""}))]
  lns <- gsub("^ | $", "", lns)
  
  # ignore markdown comments (and keep backup)
  commentsBU <- data.frame(com = lns[grepl("<!--.*-->", lns)], 
                           whichLine = grep("<!--.*-->", lns), 
                           stringsAsFactors = FALSE)
  lns <- lns[!grepl("<!--.*-->", lns)]

  # split lines at "|"
  cells <- lapply(stringr::str_split(lns, "\\|"), function(x){
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
    # chars[-c(2)] will exclude the formatting line from the determination of
    # the column width
    do.call(max, lapply(chars[-c(2)], `[`, x))
  })

  # Increase too low number of chars
  maxChars[is.na(maxChars)| maxChars < 3] <- 3

  ## build output table
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
  
  # insert comments from input 
  for (i in 1:nrow(commentsBU)){
    linesout <- append(linesout, list(commentsBU[i,1]), commentsBU[i,2] - 1)
  }
  
  
  # create output string
  out <- paste(unlist(linesout), collapse = "\n")
  return(out)
}
