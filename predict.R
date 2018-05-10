source('probability_functions.R')

# Suppose someone types 'I think its more likely'. Ignoring the first three words,
# what's the distribution of probabilities over trigrams of the form 'more likely ___'?
getProbabilityFrom3Gram('more likely than')
# [1] 0.05238095
getProbabilityFrom3Gram('more likely to')
# [1] 0.6857143

bi_vec <- c('more', 'likely')
(third_words <- triGramTable2[firstTerms == paste(bi_vec, collapse = '_'), lastTerm])
# [1] "to"        "than"      "that"      "they"      "the"       "they'll"   "you"
# ...
(tri_probs <- sapply(paste(paste(bi_vec, collapse = ' '), third_words), 
                     getProbabilityFrom3Gram))
# more likely to      more likely than      more likely that      more likely they 
#    0.685714286           0.052380952           0.052380952           0.013889638 
# ...
sum(tri_probs)
# [1] 0.8700368

# Depending on the application, this might be enough:
head(tri_probs, desc = TRUE)
#  more likely to     more likely than     more likely that     more likely they 
#     0.685714286          0.052380952          0.052380952          0.013889638 
# more likely the  more likely they'll 
#     0.013889638          0.009257668 

# Or if a single prediction is wanted, just the maximum, 
# so long as it's greater than any remaining probability
max(tri_probs) > 1 - sum(tri_probs) # TRUE


# To complete the distribution, look for completions of the bigram 'likely ___'
(second_words <- biGramTable2[firstTerms == bi_vec[2], lastTerm])
# [1] "to"                "be"                "that"              "will"   
# ...
# Some of these have already been accounted for in trigrams above, so leave 'em out
(second_words <- setdiff(second_words, third_words))
#  [1] "be"                "will"              "have"              "would"   
# ...

# Now look for trigrams 'more likely be', 'more likely will', etc.
(bi_probs <- sapply(paste(paste(bi_vec, collapse = ' '), second_words), 
                    getProbabilityFrom3Gram)) # long...
# more likely be              more likely will              more likely have 
#   2.109255e-02                  1.265553e-02                  6.327766e-03 
# ...

sum(bi_probs)
# [1] 0.1299632
sum(all_probs <- c(tri_probs, bi_probs))
# [1] 1

# None of the top six overall probabilities come from the long bi_probs 
# calculation - more reason to think about (conditions for) stopping at tri_probs
identical(top_six <- head(all_probs, desc = TRUE), 
                     head(tri_probs, desc = TRUE)) # TRUE

top_six_df <- data.frame(word = setdiff(unlist(strsplit(names(top_six), ' ')), bi_vec), 
                         probability = top_six)
# optional - are the trigrams (= row.names(top_six_df)) useful?
# top_six_df <- tibble::rownames_to_column(top_six_df, var = 'trigram')

library(ggplot2)

ggplot(top_six_df, aes(x = reorder(word, probability, function(x) 1/x), y = probability)) +
  geom_bar(stat = 'identity') +
  labs(title = paste(paste(bi_vec, collapse = ' '), '_____'), x = NULL)
