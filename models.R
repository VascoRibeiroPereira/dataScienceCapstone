library(ngram)

my_str <- concatenate(text=unlist(lapply(corp, "[", "content")))

my_input <- "baby shower" ## words input
count_words(my_input)
ng <- ngram(my_str, n = count_words(my_input) + 1) ## the n should change at will, being number of words inputed +1
prob2Ngram <- get.phrasetable(ng)

## before the grep, word should be cleaned as were the tokens!
prob2Ngram[grep(my_input, prob2Ngram$ngrams)[1],]$ngrams




## Tests

head(get.phrasetable(ngram(my_str, n=2)),10)
babble(ng, 2, seed = 10) ## ambitions haha 



termsHighFreq

termsHighFreq_2gram

termsHighFreq_3gram