source("getCleanData.R")

# Corpus to string with stopWords

corp_str <- concatenate(text=unlist(lapply(corp_StopWords, "[", "content"))) ## with the stopwords
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
                
## unigram

fit_markov <- markovchainFit(text_term, method = "laplace")

## bigram

mydf <- as.data.frame(text_term[1:500])

bigram_twitter <- mydf %>%
        unnest_tokens(bigram, text_term, token = "ngrams", n = 2) %>% 
        pull(bigram)

markov_bigram <- markovchainFit(bigram_twitter[1:3000])

## trigram

trigram_twitter <- mydf %>%
        unnest_tokens(trigram, text_term, token = "ngrams", n = 3) %>% 
        pull(trigram)

markov_trigram <- markovchainFit(trigram_twitter[1:3000])

## Fusion of the uni, bi and trigram models to predict next word

predictive_text("to do what i", 5) ## "love"

