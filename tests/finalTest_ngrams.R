## Generate ngrams with and without stop words (may whant to separate this to another file and only make a call here)

withSW <- read_csv("twitter_subset.csv")
withoutSW <- read_csv("withoutSW.csv")

### Function
ngram_freq <- function(x, n) {
        if (n >= 1) {
                n_gram <- as_tibble(get.phrasetable(ngram(x[count_words(x) >= n], n)))
        }else{
                n_gram <- as_tibble(get.phrasetable(ngram(x, 1)))
        }
        n_gram$ngrams <- gsub(" $", "", n_gram$ngrams)
        return(n_gram)
}

### Whitout Stop Words - withoutSW

unigram <- ngram_freq(withoutSW$text, 1)
bigram <- ngram_freq(withoutSW$text, 2)
trigram <- ngram_freq(withoutSW$text, 3)
tetragram <- ngram_freq(withoutSW$text, 4)
pentagram <- ngram_freq(withoutSW$text, 5)

### Whith Stop Words - withoutSW

unigramSW <- ngram_freq(withSW$text, 1)
bigramSW <- ngram_freq(withSW$text, 2)
trigramSW <- ngram_freq(withSW$text, 3)
tetragramSW <- ngram_freq(withSW$text, 4)
pentagramSW <- ngram_freq(withSW$text, 5)

#### Conjugation in a list

gramList <- list()

gramList$normal <- list(unigram, bigram, trigram, tetragram, pentagram)
gramList$sw <- list(unigramSW, bigramSW, trigramSW, tetragramSW, pentagramSW)

save(gramList, file="gramList.RData")