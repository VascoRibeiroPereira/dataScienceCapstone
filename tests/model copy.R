library(R.utils)
library(dplyr)
library(tm)
library(lexicon)
library(stringr)
library(tidyverse)
library(tidytext)
library(textclean)
library(tokenizers)
library(markovchain)
library(ngram)
library(ggplot2)
library(plotly)
library(wordcloud)
library(RColorBrewer)
library(textcat)

userInput <- "The guy in front of me just bought a pound of bacon, a bouquet, and a case of"

## Remove stop words to select (grep) most informative strings from the twitter txt

# Functions
## Functions to clean

## Expanding the contractions lexicon
new_con <- data.frame("here's", "here is") 
names(new_con) <- c("contraction", "expanded")
key_contractions <- rbind(key_contractions, new_con)

## Expanding Slang df
new_slang <- data.frame("RT", "Retweet") 
names(new_slang) <- c("x", "y")
hash_internet_slang <- rbind(hash_internet_slang, new_slang)

## Cleaning and coercing all profanity data sources
alvarez_alternative <- str_replace_all(profanity_alvarez, "\\*", "\\\\*")
alvarez_alternative <- str_replace_all(alvarez_alternative, "\\(", "\\\\(")
anger_alternative <- str_replace_all(profanity_zac_anger, "\\*", "\\\\*")
anger_alternative <- str_replace_all(anger_alternative, "\\(", "\\\\(")
profaneWords <- unique(tolower(c(profanity_arr_bad, 
                                 profanity_banned,
                                 profanity_racist,
                                 alvarez_alternative,
                                 anger_alternative)))

clean_User <- function(corpus){
        
        for (i in 1:length(corpus)) corpus[[i]] <- corpus[[i]] %>%
                        replace_contraction(contraction.key = key_contractions) %>%
                        removePunctuation(preserve_intra_word_contractions = TRUE,
                                          preserve_intra_word_dashes = TRUE,
                                          ucp = TRUE) %>%
                        replace_money() %>%
                        replace_emoticon() %>%
                        replace_symbol() %>%
                        replace_word_elongation() %>%
                        replace_ordinal() %>%
                        replace_internet_slang(slang = paste0("\\b",
                                                              hash_internet_slang[[1]], "\\b"),
                                               replacement = hash_internet_slang[[2]], ignore.case = TRUE) %>%
                        replace_number() %>%
                        replace_emoji() %>%
                        replace_non_ascii() %>%
                        stripWhitespace() %>%
                        tolower() %>%
                        removeWords(profaneWords) %>%
                        removeWords(stopwords()) %>% ## removing stop words (less informative)
                        strsplit(split = " ")
        
        corpus <- unlist(corpus) 
        corpus <- corpus[corpus %>% nchar() > 0]
        #corpus <- corpus[(length(corpus)-1):length(corpus)]
        #corpus <- paste(corpus, collapse = "|")
        
        return(corpus)
}

# Make ngrams

ngram_freq <- function(x, n) {
        if (n > 1) {
                n_gram <- as_tibble(get.phrasetable(ngram(x[count_words(x) > n], n)))
        }else{
                n_gram <- as_tibble(get.phrasetable(ngram(x, 1)))
        }
        n_gram$ngrams <- gsub(" $", "", n_gram$ngrams)
        return(n_gram)
}

usrInClean <- clean_User(userInput)

#usrInClean <- paste("^", usrInClean, "$", sep="")

#usrInput <- paste(usrInClean, collapse = " ")
#usrInClean[1] <- paste("^",usrInClean[1], sep="")
#usrInClean[length(usrInClean)] <- paste(usrInClean[length(usrInClean)],"$", sep="")
#usrInClean_reg <- paste(usrInClean, collapse = "$|^")

#usrInClean_reg <- ng_1$ngrams[grep(usrInClean_reg,ng_1$ngrams)]


#ngrams <- usrInClean %>% ngram_freq(1)
#userMostFrequent <- min(grep(paste(ngrams$ngrams, collapse = "|"), ng_1$ngrams))
#userDataSearch <- ng_1$ngrams[userMostFrequent]




## subset data based on user input

subsetImportantData <- function(file_Path, x) {
        con <- file(file_Path, "r")
        fileRead <- readLines(con, skipNul = TRUE)
        the_grep <- as.integer()
        for (i in 1:length(x)){
                the_subset_tmp <- grep(x[i], fileRead)
                the_grep <- c(the_grep, the_subset_tmp)
        }
        the_grep <- unique(the_grep)
        finalRead <- fileRead[the_grep]
        close(con)
        return(finalRead)
        
}

twitterSubset <- subsetImportantData(paste(getwd(), "/reports/final/en_US/en_US.twitter.txt", sep=""), 
                                     usrInClean)

## limit time - reduce the max subset to 100

set.seed(251)
twitterSubset <- sample(twitterSubset, 100)

clean_subset <- function(corpus){
        
        for (i in 1:length(corpus)) corpus[[i]] <- corpus[[i]] %>%
                        replace_contraction(contraction.key = key_contractions) %>%
                        removePunctuation(preserve_intra_word_contractions = TRUE,
                                          preserve_intra_word_dashes = TRUE,
                                          ucp = TRUE) %>%
                        replace_money() %>%
                        replace_emoticon() %>%
                        replace_symbol() %>%
                        replace_word_elongation() %>%
                        replace_ordinal() %>%
                        replace_internet_slang(slang = paste0("\\b",
                                                              hash_internet_slang[[1]], "\\b"),
                                               replacement = hash_internet_slang[[2]], ignore.case = TRUE) %>%
                        replace_number() %>%
                        replace_emoji() %>%
                        replace_non_ascii() %>%
                        stripWhitespace() %>%
                        tolower() %>%
                        removeWords(profaneWords) %>%
                        removeWords(stopwords())
        return(corpus)
}


newcorp <- clean_subset(twitterSubset)

## 

## Corpus to string
corp_str <- as.character(unlist(newcorp))
## Remove NA introduced by the cleaning algorithm
corp_str <- corp_str[!is.na(corp_str)]
## Preprocess

corp_final <- as.character()

for (i in 1:length(corp_str)){
        
        tmp <- preprocess(corp_str[i], case = "lower", 
                          remove.punct = TRUE, 
                          remove.numbers = TRUE, 
                          fix.spacing = FALSE)
        
        corp_final <- c(corp_final, tmp)
        
}

## Fit

text_term_unclean <- strsplit(corp_final, split = " ") %>% unlist()

text_term <- character()

for (i in 1:length(text_term_unclean)) {
        if (count_characters(text_term_unclean[i]) > 0) {
                text_term <- c(text_term, text_term_unclean[i])
        }
        
}

## unigram

fit_markov <- markovchainFit(text_term, method = "laplace")

## bigram

mydf <- as.data.frame(text_term)

bigram_twitter <- mydf %>%
        unnest_tokens(bigram, text_term, token = "ngrams", n = 2) %>% 
        pull(bigram)

markov_bigram <- markovchainFit(bigram_twitter)

## trigram

trigram_twitter <- mydf %>%
        unnest_tokens(trigram, text_term, token = "ngrams", n = 3) %>% 
        pull(trigram)

markov_trigram <- markovchainFit(trigram_twitter)

## Fusion of the uni, bi and trigram models to predict next word

predictive_text <- function(text, num_word){
        
        word_count <- count_words(text)
        
        # Check if it has 3 or more words
        if (word_count >= 3) {
                input_text <- strsplit(text, " ") %>% unlist() %>% tail(3) %>% paste(collapse = " ")
                
                # If cannot find the transition in trigram
                if (isTRUE(tryCatch(markov_trigram$estimate[ input_text, ], error = function(e) 1) == 1)) {
                        
                        input_text <- strsplit(text, " ") %>% unlist() %>% tail(2) %>% paste(collapse = " ")
                        
                        # If cannot find the transition in bigram
                        if (isTRUE(tryCatch(markov_bigram$estimate[ input_text, ], error = function(e) 1) == 1)) {
                                
                                input_text <- strsplit(text, " ") %>% unlist() %>% tail(1) %>% paste(collapse = " ")
                                
                                suggest <- fit_markov$estimate[ tolower(input_text), ] %>%
                                        sort(decreasing = T) %>% 
                                        head(num_word) 
                                
                                suggest[suggest > 0] %>% 
                                        names() 
                        } else {
                                
                                # Can find the transition in bigram
                                suggest <- markov_bigram$estimate[ tolower(input_text), ] %>%
                                        sort(decreasing = T) %>% 
                                        head(num_word) 
                                
                                suggest[suggest > 0] %>% 
                                        names() %>%
                                        str_extract(pattern = "\\s(.*)") %>% 
                                        str_remove("[ ]")
                        }
                } else {
                        
                        # Can find the transition in trigram
                        suggest <- markov_trigram$estimate[ tolower(input_text), ] %>%
                                sort(decreasing = T) %>% 
                                head(num_word) 
                        
                        suggest[suggest > 0] %>% 
                                names() %>%
                                str_extract(pattern = "\\s(.*)") %>% 
                                str_remove("[ ]") %>%  
                                str_extract(pattern = "\\s(.*)") %>% 
                                str_remove("[ ]") 
                }
                
        } else {
                
                # Check if the words is 2 or more
                if (word_count >= 2) {
                        
                        input_text <- strsplit(text, " ") %>% unlist() %>% tail(2) %>% paste(collapse = " ")
                        
                        # If cannot find the transition in bigram
                        if (isTRUE(tryCatch(markov_bigram$estimate[ input_text, ], error = function(e) 1) == 1)) {
                                
                                input_text <- strsplit(text, " ") %>% unlist() %>% tail(1) %>% paste(collapse = " ")
                                
                                suggest <- fit_markov$estimate[ tolower(input_text), ] %>%
                                        sort(decreasing = T) %>% 
                                        head(num_word) 
                                
                                suggest[suggest > 0] %>% 
                                        names() 
                        } else{
                                
                                # Can find the transition in bigram
                                suggest <- markov_bigram$estimate[ tolower(input_text), ] %>%
                                        sort(decreasing = T) %>% 
                                        head(num_word) 
                                
                                suggest[suggest > 0] %>% 
                                        names() %>%
                                        str_extract(pattern = "\\s(.*)") %>% 
                                        str_remove("[ ]")
                        }
                        
                } else {
                        
                        # If only has 1-gram
                        input_text <- strsplit(text, " ") %>% unlist() %>% tail(1) %>% paste(collapse = " ")
                        
                        # exclude punctuation
                        punctuation <- which(fit_markov$estimate[ tolower(input_text), ] %>% names() %>% str_detect("[:punct:]"))
                        
                        suggest <- fit_markov$estimate[ tolower(input_text), -punctuation] %>%
                                sort(decreasing = T) %>% 
                                head(num_word) 
                        
                        suggest[suggest > 0] %>% 
                                names() 
                }
                
        }
}

userInput
predictive_text(usrInput, 5)
