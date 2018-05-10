source('probability_functions.R')

# Prediction function -------------------------------------------------------------------------

top_n_words <- function(bi_vec = c('more', 'likely'), n = 6){
  # Obtains third_words & second_words using indexing, so
  # assumes frequencies in triGramTable2 & biGramTable2 
  # are already sorted in descending order
  # If this is not so, could sort explicitly
  
  # Trigrams - take top n (if there are that many)
  matching_trigrams <- triGramTable2[firstTerms == paste(bi_vec, collapse = '_'), ]
  # At most n
  num_trigrams <- min(nrow(matching_trigrams), n)
  third_words <- matching_trigrams[, lastTerm][seq_len(num_trigrams)]
  tri_probs <- sapply(paste(paste(bi_vec, collapse = ' '), third_words), 
                        getProbabilityFrom3Gram)
  
  # Bigrams - take top n of those not previously seen (if there are that many)
  second_words <- biGramTable2[firstTerms == bi_vec[2], lastTerm]
  # Only previously unseen words
  second_words <- setdiff(second_words, third_words)
  # At most n (if length(tri_probs) < n, we'll use at least one)
  second_words <- second_words[seq_len(min(length(second_words), n))]
  bi_probs <- sapply(paste(paste(bi_vec, collapse = ' '), second_words),
                     getProbabilityFrom3Gram)
  all_probs <- c(tri_probs, bi_probs)
  top_n <- head(all_probs, desc = TRUE) # possibly shorter than n?
  top_n_df <- data.frame(word = setdiff(unlist(strsplit(names(top_n), ' ')), bi_vec), 
                         probability = top_six)
  library(ggplot2)
  ggplot(top_n_df, 
         aes(x = reorder(word, probability, function(x) 1/x), y = probability)) +
    geom_bar(stat = 'identity') +
    labs(title = paste(paste(bi_vec, collapse = ' '), '_____'), x = NULL)
  
}

top_six_words(c('ask', 'not'))
top_six_words(c('you', 'very'))
top_six_words(c('dad', 'very'))
