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

## Remove stop words to select (grep) most informative strings from the twitter txt

mywd <- getwd()

subsetBigData <- function(file_Path, percentage) {
        file_length <- length(readLines(file_Path, warn = FALSE))
        number_Lines <- as.integer(file_length*(percentage/100))
        set.seed(123)
        lineSelection <- sample(file_length, number_Lines)
        con <- file(file_Path, "r")
        fileRead <- readLines(con, skipNul = TRUE)[lineSelection]
        close(con)
        return(fileRead)
}



enDataSubset <- subsetBigData(paste(mywd, "/reports/final/en_US/en_US.twitter.txt", sep=""), .01) ## original 0.2% too big for model>unigram

corp_toClean <- VCorpus(VectorSource(enDataSubset))


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

## Functions to clean
clean_corpus <- function(corpus){
        
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
                        removeWords(profaneWords)        
        return(corpus)
}


corp <- clean_corpus(corp_toClean)


## Corpus to string
corp_str <- as.character(unlist(corp))
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


ngram_freq <- function(x, n) {
        if (n > 1) {
                n_gram <- as_tibble(get.phrasetable(ngram(x[count_words(x) > n], n)))
        }else{
                n_gram <- as_tibble(get.phrasetable(ngram(x, 1)))
        }
        n_gram$ngrams <- gsub(" $", "", n_gram$ngrams)
        return(n_gram)
}

ng_1 <- ngram_freq(corp_final, 1)

my_stopWords <- paste(stopwords(), collapse = "|")
uninformative <- (unique(grep(my_stopWords, ng_1$ngrams)))

wordsImportant <- ng_1[-uninformative,]$ngrams  ## important words?
wordsImportant <- wordsImportant[nchar(wordsImportant)>3]

## should I grep all instances that have this words!?

subsetImportantData <- function(file_Path, x) {
        the_pattern <- paste(x, collapse = "|")
        con <- file(file_Path, "r")
        fileRead <- readLines(con, skipNul = TRUE)
        the_subset <- unique(grep(the_pattern, fileRead))
        finalRead <- fileRead[the_subset]
        close(con)
        return(finalRead)
        
}

enDataSubset_Final <- subsetImportantData(paste(mywd, "/reports/final/en_US/en_US.twitter.txt", sep=""), wordsImportant)


file_length <- length(enDataSubset_Final)
number_lines <- as.integer(file_length*(.01/100))
lineSelection <- sample(file_length, number_lines)

enDataSubset <- enDataSubset_Final[lineSelection] ## temp measure


#### NEW IDEA

## GREP THE DATA BASED ON USER INPUT AND BUILD A MODEL BASED ON IT!?





## Check most frequent words that cover 90% of instances in our text (in standby)

#ng_1 <- ngram_freq(corp_final, 1)
#ng_1_90 <- ng_1 %>%
#        mutate(Cumulative = cumsum(prop)) %>%
#        filter(Cumulative <= .900) %>%
#        pull(ngrams)



my_text <- strsplit(corp_final, split = " ") %>% unlist()

text_uni <- character()

for (i in 1:length(my_text)) {
        if (count_characters(my_text[i]) > 0) {
                text_uni <- c(text_uni, my_text[i])
        }
        
}


###############
## unigram

fit_markov <- markovchainFit(text_uni, method = "laplace")

## bigram

mydf <- as.data.frame(text_uni)

bigram_twitter <- mydf %>%
        unnest_tokens(bigram, text_uni, token = "ngrams", n = 2) %>% 
        pull(bigram)

markov_bigram <- markovchainFit(bigram_twitter)

## trigram

trigram_twitter <- mydf %>%
        unnest_tokens(trigram, text_uni, token = "ngrams", n = 3) %>% 
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


predictive_text("to do what i", 5) ## "love"