library(readr)
library(RWeka)
library(ngram)
library(tokenizers)

source("ngramModel4_fun.R")

## Get a bib subset of clean data and sample it 
twitterDF <- read_csv("twitter_subset.csv")

set.seed(1558)
lineSelection <- sample(length(twitterDF$line), length(twitterDF$line)*.05)
sampleDF <- twitterDF[lineSelection,]


## remove stop words from the sampleDF to test it

cleanStopWords <- function(n) {
        # n is a char vector
        n_lite <- n %>%
                removeWords(stopwords()) %>%
                str_split(" ") %>%
                unlist()
        n_lite <- paste(n_lite[n_lite != ""], collapse = " ")
        
        return(n_lite)

}

sampleDF_lite <- cleanStopWords(sampleDF$text)


## Generate grams and table their frequency
ngram_freq <- function(x, n) {
        if (n >= 1) {
                n_gram <- as_tibble(get.phrasetable(ngram(x[count_words(x) >= n], n)))
        }else{
                n_gram <- as_tibble(get.phrasetable(ngram(x, 1)))
        }
        n_gram$ngrams <- gsub(" $", "", n_gram$ngrams)
        return(n_gram)
}

unigram <- ngram_freq(sampleDF$text, 1)
bigram <- ngram_freq(sampleDF$text, 2)
trigram <- ngram_freq(sampleDF$text, 3)
tetragram <- ngram_freq(sampleDF$text, 4)
pentagram <- ngram_freq(sampleDF$text, 5)


## Search terms

## testing a 4 word input

## usr_input <- cleanStopWords("when you breathe i want to be the air for you i will be there for you i would live and i would") ##only for case when semove stopwords

usr_input <- "not time to take a"
usr_Grep <- paste("^", usr_input, sep = "")

pentagram[grep(usr_Grep, pentagram$ngrams),]

## testing a 3 word input

usr_split <- unlist(str_split(usr_Grep, " "))
usr_split <- paste("^", paste(usr_split[2:length(usr_split)], collapse = " "), sep="")

tetragram[grep(usr_split, tetragram$ngrams),]

## testing a 2 word input

usr_split <- unlist(str_split(usr_Grep, " "))
usr_split <- paste("^", paste(usr_split[3:length(usr_split)], collapse = " "), sep="")

trigram[grep(usr_split, trigram$ngrams),]


## testing a 1 word input SHOULD BE MADE WITH A MARKOV CHAIN against unigrams without stopwords to check for "themes"

usr_split <- unlist(str_split(usr_Grep, " "))
usr_split <- paste("^", paste(usr_split[4:length(usr_split)], collapse = " "), sep="")

bigram[grep(usr_split, bigram$ngrams),]
