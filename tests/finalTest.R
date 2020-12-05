library(readr)
source("getCleanData_fun.R")

withSW <- read_csv("twitter_subset.csv")
withoutSW <- read_csv("withoutSW.csv")

userInput <- "my best friend is sad, but I'll helping him" #out" - missing word

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

### Whith Stop Words - withoutSW

unigramSW <- ngram_freq(withSW$text, 1)
bigramSW <- ngram_freq(withSW$text, 2)
trigramSW <- ngram_freq(withSW$text, 3)
tetragramSW <- ngram_freq(withSW$text, 4)
pentagramSW <- ngram_freq(withSW$text, 5)


## Evaluate against the ngrams

### Guarantee a max five word input and evaluate
if (length(userInputClean) > 4) {
        userInput_crop <- userInputClean[(length(userInputClean)-3):length(userInputClean)]
        userInput_crop <- paste("^", paste(userInput_crop, collapse = " "), sep="")
} else {
        userInput_crop <- paste("^", paste(userInputClean, collapse = " "), sep="")
}


if (count_words(userInput_crop) == 4) {
        
        freqGramOutput <- tibble()
        
        pentagram[grep(userInput_crop, pentagram$ngrams),] ## 4 words search in pentagram
        
        userInput_crop <- unlist(str_split(userInput_crop, " "))
        userInput_crop <- paste("^", paste(userInput_crop[2:length(userInput_crop)], collapse = " "), sep="")
        
        freqGramOutput <- rbind(freqGramOutput, tetragram[grep(userInput_crop, tetragram$ngrams),][1:2,]) ## 3 words search in tetragram
        
        userInput_crop <- unlist(str_split(userInput_crop, " "))
        userInput_crop <- paste("^", paste(userInput_crop[2:length(userInput_crop)], collapse = " "), sep="")
        
        freqGramOutput <- rbind(freqGramOutput, trigram[grep(userInput_crop, trigram$ngrams),][1:2,]) ## 2 words search in trigram
        
        userInput_crop <- unlist(str_split(userInput_crop, " "))
        userInput_crop <- paste("^", paste(userInput_crop[2:length(userInput_crop)], collapse = " "), sep="")
        
        freqGramOutput <- rbind(freqGramOutput, bigram[grep(userInput_crop, bigram$ngrams),][1:2,]) ## 1 word search in bigram
        
}










### Number of Terms




if (exists("userInputCleanStop")) {
        
        
}else{
        
        
}