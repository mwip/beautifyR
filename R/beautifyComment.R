#' Beautify R comments
#'
#' This function beautifys multi-line R comments to fit the 80 column margin 
#' perfectly without the need of tedious manual formatting.
#'
#' @import stringr
#' @param inputstring Character vector of length 1 containing R code.
#' @export
beautifyComment <- function(inputstring){
  lines <- unlist(str_split(inputstring, "\n"))
  lines_orig <- lines # save for updating after changes have been made
  
  lines_comment <- which(str_detect(lines, "^#'?"))
  
  # groups of consecutive multi-line comments
  groups <- split(lines_comment, cumsum(c(1, diff(lines_comment) != 1)))
  
  lines_aligned <- lapply(groups, function(x) {
    # get current group of lines 
    lines_tmp <- lines[x]
    
    # extract the comment char used (# or #')
    comment_char <- str_extract(lines_tmp[1], "^#'?")
    
    # remove all comment chars
    lines_tmp_uncommented <- str_replace(lines_tmp, "^#'? ?", "")
    
    # split the lines in all the words
    words <- unlist(str_split(lines_tmp_uncommented, " "))
    
    # drop empty entries
    words <- words[nchar(words) > 0]
    
    # count the number of characters per word
    words_charcount <- nchar(words)
    
    # create a table for easy crossreferencing
    charcount_df <- data.frame(idx = 1:length(words_charcount), 
                               charcount = words_charcount)
    
    # create a list to store the index vectors of the new lines.
    new_line_word_idx <- list()
    
    # add words to new lines and remove them from the remaining candidates until
    # no words are left
    while (nrow(charcount_df) > 0) {
      # get the cumulative sum of the chars (+ 1 to account for spaces)
      charcount_df$cumsum <- cumsum(charcount_df$charcount + 1)
      
      # create index for subsetting
      words_to_add <- charcount_df$cumsum < (80 - (nchar(comment_char) + 1))
      
      # identify words that can be added to the new line
      new_line_word_idx[[length(new_line_word_idx) + 1]] <- 
        charcount_df$idx[words_to_add]
      
      # remove words from the data frame
      charcount_df <- charcount_df[!words_to_add, ]
      
      # add at least one word if it is too long
      if (sum(words_to_add) == 0){
        new_line_word_idx[[length(new_line_word_idx) + 1]] <- 
          charcount_df$idx[1]
        charcount_df <- charcount_df[-1, ]
      }
      
    }
    
    new_lines <- lapply(new_line_word_idx, function(x) {
      paste(c(comment_char, words[x]), collapse = " ")
    })
    
    unlist(new_lines)
  })
  
  
  # get groups of lines without comments
  lines_no_comment <- which(!str_detect(lines, "^#'?"))
  groups_no_comment <- split(lines_no_comment, 
                             cumsum(c(1, diff(lines_no_comment) != 1)))
  
  # accumulate the final lines
  lines_out <- lapply(1:max(length(groups), length(groups_no_comment)), 
                      function(x) {
 
                        # catch "subscript out of bounds" error
                        if (x > length(groups)) {
                          return(lines[groups_no_comment[[x]]])
                        }
                        if (x > length(groups_no_comment)) {
                          return(lines_aligned[[x]])
                        }

                        # combine lines according to their indexes
                        if (min(groups[[x]]) < min(groups_no_comment[[x]])) {
                          return(c(lines_aligned[[x]], 
                                   lines[groups_no_comment[[x]]]))
                        } else {
                          return(c(lines[groups_no_comment[[x]]], 
                                   lines_aligned[[x]]))
                        }
                        
                      })
  paste(unlist(lines_out), collapse = "\n")
}