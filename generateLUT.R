quartGramTableGrouped <- group_by(quartGramTable2, firstTerms) 

system.time(quartGramTableRF <- mutate(quartGramTableGrouped, Score = frequency/sum(frequency)))

quartGramLUT <- filter(quartGramTableRF, frequency >= 4)

object.size(quartGramLUT)

#building tri-gram LUT using piping instead of intermediate steps

system.time(triGramLUT <- triGramTable2 %>%
  group_by(firstTerms) %>%
  mutate(Score = 0.4 * (frequency/sum(frequency))) %>%
  filter(frequency >= 4))

object.size(triGramLUT)

#building bi-gram LUT using piping instead of intermediate steps

system.time(biGramLUT <- biGramTable2 %>%
              group_by(firstTerms) %>%
              mutate(Score = (0.4 * 0.4) * (frequency/sum(frequency))) %>%
              filter(frequency >= 4))

object.size(biGramLUT)

#function to create top n LUT's in case an application requires more than 5 results, default value is 5.
topnLUT <- function(nGramLUT, n = 5){
  top_n(nGramLUT, n, Score)
}