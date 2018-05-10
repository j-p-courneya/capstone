topSixWords <-  function(bi_vec = c('more', 'likely')){
  third_words <- triGramTable2[firstTerms == paste(bi_vec, collapse = '_'), lastTerm]
  
  tri_probs <- sapply(paste(paste(bi_vec, collapse = ' '), third_words), 
                       getProbabilityFrom3Gram)
  # second_words <- biGramTable2[firstTerms == bi_vec[2], lastTerm]
  # second_words <- setdiff(second_words, third_words)
  # bi_probs <- sapply(paste(paste(bi_vec, collapse = ' '), second_words), 
  #                    getProbabilityFrom3Gram)) 
  # all_probs <- c(tri_probs, bi_probs)
  top_six <- head(tri_probs, desc = TRUE)
  return(data.frame(word = setdiff(unlist(strsplit(names(top_six), ' ')), bi_vec), 
                           probability = top_six))
}
  
top_six_df <- topSixWords(c('why', 'not'))
# optional - are the trigrams (= row.names(top_six_df)) useful?
# top_six_df <- tibble::rownames_to_column(top_six_df, var = 'trigram')

library(ggplot2)

ggplot(top_six_df, aes(x = reorder(word, probability, function(x) 1/x), y = probability)) +
  geom_bar(stat = 'identity') +
  labs(title = paste(paste(bi_vec, collapse = ' '), '_____'), x = NULL)