# Get subseted clean data (1%) and functions

twitterDF <- read_csv("twitter_subset.csv")
source("ngramModel_fun.R")


ngram_freq <- function(x, n) {
        ngram <- NGramTokenizer(x, Weka_control(min = n, max = n))
        return(ngram)
}


unigram <- ngram_freq(twitterDF$text, 1)
bigram <- ngram_freq(twitterDF$text, 2)
trigram <- ngram_freq(twitterDF$text, 3)


## fit

fit_markov <- markovchainFit(unigram, method = "laplace")
markov_bigram <- markovchainFit(bigram, method = "laplace")
markov_trigram <- markovchainFit(trigram, method = "laplace")


save(unigram, file = "unigram.RData")
save(bigram, file = "bigram.RData")
save(trigram, file = "trigram.RData")

save(fit_markov, file = "fit_markov.RData")
save(markov_bigram, file = "markov_bigram.RData")
save(markov_trigram, file = "markov_trigram.RData")

predictive_text("to do what i", 1) ## "love"




