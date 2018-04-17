library(stringr)

extractAlignment <- function(cells, mc = maxColumns){
  ind <- cells[[2]]
  
  # create empty alignment vector
  align <- rep("l", max(length(ind), mc))
  align[grepl("-:", ind)] <- "r"
  align[grepl(":-+:", ind)] <- "c"
  return(align)
}

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

refineFormatting <- function(ce, al = align){
  ce[[2]] <- gsub(".", "-", ce[[2]])
  ce[[2]][al == "l"] <- gsub("^-", ":", ce[[2]][al =="l"])
  ce[[2]][al == "c"] <- gsub("^-|-$", ":", ce[[2]][al =="c"])
  ce[[2]][al == "r"] <- gsub("-$", ":", ce[[2]][al =="r"])
  return(ce)
}

beautifyR <- function(inputstring){
  # split table at "\n"
  lines <- as.list(str_split(inputstring, "\n", simplify = TRUE))
  
  # split lines at "|"
  cells <- lapply(str_split(lines, "\\|"), function(x){
    x[x != ""]
  })
  
  # count number of cells in each row  
  ncolumns <- lapply(cells, length)
  maxColumns <- do.call(max, ncolumns)
  
  # if no or false alignment row is given return left-aligned and show warning
  if (any(grepl("[^:|-|[[:blank:]]]", cells[[2]]))){
    cells <- append(cells, list(rep(":--", maxColumns)), 1)
    warning("Fomatting indicator row 2 (e.g. :----) contains invalid values
             left alignment assumed for all columns")
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

cat("print...\n")
print(beautifyR(x))
cat("\n")
cat("\n")
cat("cat...\n")
cat(beautifyR(x))
