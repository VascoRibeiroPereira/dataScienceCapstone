library(readr)
source("getCleanData_fun.R")

withSW <- read_csv("twitter_subset.csv")
withoutSW <- read_csv("withoutSW.csv")

userInput <- "my best friend is sad, but I'll helping him out"

evaluateCurses <- function(userInput) {
        userInput <- unlist(str_split(userInput, " "))
        curseCount <- as.integer()
        for (i in 1:length(userInput)){
                curseCount <- sum(curseCount, 
                                  grepl(paste("^", userInput[i], "$" ,sep=""),
                                        profaneWords))
        }
        
        if (curseCount > 0){
                returnMessage <- "Don't use curse words please."
                return(returnMessage)
        }
}

evaluateCurses(userInput)

cleanInNoStop <- function(userInput) {
        userInputOut <- userInput %>%
                replace_contraction() %>%
                removePunctuation() %>%
                replace_money() %>%
                replace_emoticon() %>%
                replace_symbol() %>%
                replace_word_elongation() %>%
                replace_ordinal() %>%
                replace_number() %>%
                tolower() %>%
                removeWords(stopwords()) %>%
                stripWhitespace() %>%
                trimws() %>%
                str_split(pattern = " ") %>%
                unlist()
        
        return(userInputOut)
}

userInputClean <- cleanInNoStop(userInput)


if (sum(count_words(userInputClean)) == 0){
        cleanInput <- function(userInput) {
                userInputOut <- userInput %>%
                        replace_contraction() %>%
                        removePunctuation() %>%
                        replace_money() %>%
                        replace_emoticon() %>%
                        replace_symbol() %>%
                        replace_word_elongation() %>%
                        replace_ordinal() %>%
                        replace_number() %>%
                        stripWhitespace() %>%
                        tolower() %>%
                        str_split(pattern = " ") %>%
                        unlist()
                
                return(userInputOut)
        }
        
        userInputCleanStop <- cleanInput(userInput)
        
}

## Generate ngrams with and without stop words (may whant to separate this to another file and only make a call here)

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


## Evaluate against the ngrams

if (exists("userInputCleanStop")) {
        
        
}else{
        
        
}