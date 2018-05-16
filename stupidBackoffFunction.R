library(dplyr)
library(stringr)

stupidBackoffPredFunction <- function(impText){
  truncText <- tail(unlist(strsplit(impText, " ")),
                    n = 3)
  finalWords <- filter(quartGramLUT5,
                       firstTerms == paste(truncText, collapse = '_'))
  if(nrow(finalWords) == 5){
    return(finalWords[, c("lastTerm", "Score")])
  } else {
    predWords2 <- filter(triGramLUT5,
                         firstTerms == paste(truncText[2:3], collapse = '_'))
    finalWords <- rbind(finalWords, predWords2)
    finalWords <- finalWords[!duplicated(finalWords$lastTerm), ]
    if(nrow(finalWords) >= 5){
      return(head(finalWords[, c("lastTerm", "Score")], n =5))
    } else {
      predWords3 <- filter(biGramLUT5, 
                           firstTerms == paste(truncText[3], collapse = '_'))
      finalWords <- rbind(finalWords, predWords3)
      finalWords <- finalWords[!duplicated(finalWords$lastTerm), ]
      if(nrow(finalWords) > 0){
        return(head(finalWords[, c("lastTerm", "Score")]))
      } else {
        return(uniGramLUT5[, c("lastTerm", "Score")])
      }
    }
  }
}