library(markovchain)
library(tidytext)
library(ngram)
library(dplyr)
source("getCleanData.R")

corp_str <- concatenate(text=unlist(lapply(corp, "[", "content"))) ## with the stopwords
corp_str <- preprocess(corp_str, case = "lower", 
                       remove.punct = TRUE, 
                       remove.numbers = TRUE, 
                       fix.spacing = FALSE)


text_term_unclean <- strsplit(corp_str, split = " ") %>% unlist()

text_term <- character()

for (i in 1:length(text_term_unclean)) {
        if (count_characters(text_term_unclean[i]) > 0) {
                text_term <- c(text_term, text_term_unclean[i])
        }
        
}
                

fit_markov <- markovchainFit(text_term, method = "laplace")

## Predictive text

predictive_text <- function(text, num_word){
        text <- strsplit(text, " ") %>% unlist() %>% tail(1)

        suggest <- fit_markov$estimate[ tolower(text) ] %>%
                sort(decreasing = T) %>% 
                head(num_word) 
        
        suggest <- suggest[suggest > 0] %>% 
                names()
        
        return(suggest)
}

## Testing
predictive_text("what", 10) ## "a"        "you"      "i"        "is"       "are"      "do"       "the"      "did"      "happened" "it"  


## bigram

mydf <- as.data.frame(text_term[1:500])

bigram_twitter <- mydf %>%
        unnest_tokens(bigram, text_term, token = "ngrams", n = 2) %>% 
        pull(bigram)



markov_bigram <- markovchainFit(bigram_twitter[1:3000])

predictive_text <- function(text, num_word){
        
        suggest <- markov_bigram$estimate[ tolower(text), ] %>%
                sort(decreasing = T) %>% 
                head(num_word) 
        
        suggest <- suggest[ suggest > 0] %>% 
                names() %>% 
                str_extract(pattern = "\\s(.*)") %>% 
                str_remove("[ ]")
        
        return(suggest)
}

predictive_text("do what", 5) ## "i"  "we"

## trigram

trigram_twitter <- mydf %>%
        unnest_tokens(trigram, text_term, token = "ngrams", n = 3) %>% 
        pull(trigram)

markov_trigram <- markovchainFit(trigram_twitter[1:3000])

predictive_text <- function(text, num_word){
        
        suggest <- markov_trigram$estimate[ tolower(text), ] %>%
                sort(decreasing = T) %>% 
                head(num_word) 
        
        suggest <- suggest[ suggest > 0 ] %>% 
                names() %>% 
                str_extract(pattern = "\\s(.*)") %>% 
                str_remove("[ ]") %>%  
                str_extract(pattern = "\\s(.*)") %>% 
                str_remove("[ ]")
        
        return(suggest)
}

predictive_text("to do what", 5) ## "i"  "we"


## fusion

source("markov_predictive.R")

predictive_text("to do what i", 5) ## "love"

