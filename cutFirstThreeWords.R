truncateText <- function(text, wordCount = 3) {
  truncText <- paste(tail(unlist(strsplit(text, " ")),
                          wordCount),
                     collapse = " ")
  return(truncText)
}
