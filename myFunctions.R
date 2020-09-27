library(tm)
library(lexicon)
library(stringr)
library(tidyverse)
library(tidytext)
library(textclean)
library(tokenizers)
library(markovchain)
library(dplyr)
library(ngram)
library(ggplot2)

clean_corpus <- function(corpus){
        corpus <- tm_map(corpus, stripWhitespace)
        corpus <- tm_map(corpus, content_transformer(tolower))
        corpus <- tm_map(corpus, removeWords, c(stopwords("en")))
        
        alvarez_alternative <- str_replace_all(profanity_alvarez, "\\*", "\\\\*")
        alvarez_alternative <- str_replace_all(alvarez_alternative, "\\(", "\\\\(")
        anger_alternative <- str_replace_all(profanity_zac_anger, "\\*", "\\\\*")
        anger_alternative <- str_replace_all(anger_alternative, "\\(", "\\\\(")
        
        profaneWords <- unique(tolower(c(profanity_arr_bad, 
                                         profanity_banned,
                                         profanity_racist,
                                         alvarez_alternative,
                                         anger_alternative)))
        
        corpus <- tm_map(corpus, removeWords, profaneWords)
        corpus <- tm_map(corpus, removePunctuation)
        corpus <- tm_map(corpus, removeNumbers)
        return(corpus)
}

corpus_stopWords <- function(corpus){
        corpus <- tm_map(corpus, removeWords, c(stopwords("en")))
        return(corpus)
}

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