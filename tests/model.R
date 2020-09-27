library(cmscu)

# Build Frequency Dictionaries

# unigrams
twitter_en_1g <- new(FrequencyDictionary,4,2^26)
twitter_en_2g <- new(FrequencyDictionary,4,2^26)
twitter_en_3g <- new(FrequencyDictionary,4,2^26)
twitter_en_4g <- new(FrequencyDictionary,4,2^26)

## function ngrams

ngrams <- function(lst, n) {
        len <- length(lst);
        sapply(1:(len-n+1), function(i) do.call(paste, as.list(lst[i:(i+n-1)])))
}

# split to unigrams    
unilist <- unlist(strsplit(corp_str, ' ')) 
# store unigrams
twitter_en_1g$store(unilist)
# add beginning and end of sentence tags to unigrams, and subsequent n-grams 
# (crucial for smoothing ngrams in test phase)
bilist <- c("<BOS>",unilist,'<EOS>')
# store bigrams, use the "ngrams" function bind unigrams together
twitter_en_2g$store(ngrams(bilist,2))
# store trigrams    
trilist <- c("<BOS>","<BOS>",unilist,'<EOS>')
twitter_en_3g$store(ngrams(trilist,3))
# store quadgrams
qualist <- c("<BOS>","<BOS>","<BOS>",unilist,'<EOS>')
twitter_en_4g$store(ngrams(qualist,4))


# query the unigram dictionary
twitter_en_1g$query(c("hello","pizza","one"))

# number of total n-grams in the corpus 
twitter_en_1g$entries
