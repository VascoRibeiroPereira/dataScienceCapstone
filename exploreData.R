source("getCleanData.R")
library(dplyr)
library(ggplot2)

## Uni-gram

corpTDM <- TermDocumentMatrix(corp)

frequentTerms <- findFreqTerms(corpTDM, 25, Inf)

termsHighFreq <- tibble()

for (i in 1:length(frequentTerms)){
        
        tmp <- tm_term_score(corpTDM, frequentTerms[i], 
                             FUN = function(x) sum(x, na.rm = TRUE))
        
        termsHighFreq <- bind_rows(termsHighFreq, 
                                     tibble(Terms = frequentTerms[i], Frequency = tmp))

}

rm(tmp, i, frequentTerms)

termsHighFreq <- arrange(termsHighFreq, desc(Frequency))

plot_1_gram <- ggplot(subset(termsHighFreq, Frequency %in% 100:500), aes(x = reorder(Terms, Frequency), Frequency)) + 
        geom_col(aes(fill = Frequency)) + 
        xlab("Terms") +
        ggtitle("1-gram Frequency Chart", subtitle = waiver()) +
        scale_fill_gradient(low = "green", 
                             high = "red") +
        coord_flip()


## 2-grams

corp_2gramTDM <- TermDocumentMatrix(
        corp,
        control = list(tokenize = tokenizer2))


frequentTerms_2gram <- findFreqTerms(corp_2gramTDM, 5, Inf)

termsHighFreq_2gram <- tibble()

for (i in 1:length(frequentTerms_2gram)){
        
        tmp <- tm_term_score(corp_2gramTDM, frequentTerms_2gram[i], 
                             FUN = function(x) sum(x, na.rm = TRUE))
        
        termsHighFreq_2gram <- bind_rows(termsHighFreq_2gram, 
                                         tibble(Terms = frequentTerms_2gram[i], Frequency = tmp))
        
}

rm(tmp, i, frequentTerms_2gram)

termsHighFreq_2gram <- arrange(termsHighFreq_2gram, desc(Frequency))

plot_2_gram <- ggplot(subset(termsHighFreq_2gram, Frequency %in% 10:50), aes(x = reorder(Terms, Frequency), Frequency)) + 
        geom_col(aes(fill = Frequency)) + 
        xlab("Terms") +
        ggtitle("2-gram Frequency Chart", subtitle = waiver()) +
        scale_fill_gradient(low = "green", 
                            high = "red") +
        coord_flip()



## 3-grams

corp_3gramTDM <- TermDocumentMatrix(
        corp,
        control = list(tokenize = tokenizer3))


frequentTerms_3gram <- findFreqTerms(corp_3gramTDM, 2, Inf)

termsHighFreq_3gram <- tibble()

for (i in 1:length(frequentTerms_3gram)){
        
        tmp <- tm_term_score(corp_3gramTDM, frequentTerms_3gram[i], 
                             FUN = function(x) sum(x, na.rm = TRUE))
        
        termsHighFreq_3gram <- bind_rows(termsHighFreq_3gram, 
                                         tibble(Terms = frequentTerms_3gram[i], Frequency = tmp))
        
}

rm(tmp, i, frequentTerms_3gram)

termsHighFreq_3gram <- arrange(termsHighFreq_3gram, desc(Frequency))

plot_3_gram <- ggplot(subset(termsHighFreq_3gram, Frequency %in% 3:10), aes(x = reorder(Terms, Frequency), Frequency)) + 
        geom_col(aes(fill = Frequency)) + 
        xlab("Terms") +
        ggtitle("3-gram Frequency Chart", subtitle = waiver()) +
        scale_fill_gradient(low = "green", 
                            high = "red") +
        coord_flip()


## Frequency sorted dictionary

frequentTerms <- findFreqTerms(corpTDM, 1, Inf)

freq_sorted <- tibble()

for (i in 1:length(frequentTerms)){
        
        tmp <- tm_term_score(corpTDM, frequentTerms[i], 
                             FUN = function(x) sum(x, na.rm = TRUE))
        
        freq_sorted <- bind_rows(freq_sorted, 
                                   tibble(Terms = frequentTerms[i], Frequency = tmp))
        
}
rm(tmp, frequentTerms)

freq_sorted <- freq_sorted %>% 
        arrange(desc(Frequency)) %>% 
        mutate(Percentage = Frequency * 100 / sum(freq_sorted$Frequency)) %>%
        mutate(Cumulative = cumsum(Percentage))

### Frequency sorted dictionary table 

str(freq_sorted)


### Plot of frequency sorted dictionary expressed in Cumulative Percentages
plot_cumPerc <- ggplot(freq_sorted, aes(1:dim(freq_sorted)[1], Cumulative)) + 
        geom_line() +
        annotate("rect", xmin = 0, xmax = dim(freq_sorted %>% filter(Cumulative <= 50.0))[1], 
                 ymin = 0, ymax = 50, alpha = .4) +
        annotate("text", x = 1000, y = 25, label = "<<<< 50% Cover") +
        annotate("rect", xmin = 0, xmax = dim(freq_sorted %>% filter(Cumulative <= 90.0))[1], 
                 ymin = 0, ymax = 90, alpha = .3) +
        annotate("text", x = 6000, y = 96, label = "<<<< 90% Cover") +
        annotate("rect", xmin = 0, xmax = dim(freq_sorted %>% filter(Cumulative <= 100.0))[1], 
                 ymin = 0, ymax = 100, alpha = .2) +
        annotate("text", x = 8500, y = 105, label = "<<<< 100% Cover") +
        xlab("Number of Terms") +
        ylab("Cumulative Percentage")

### Number of unique words to cover 50%
dim(freq_sorted %>% filter(Cumulative <= 50.0))[1]

### Number of unique words to cover 90%
dim(freq_sorted %>% filter(Cumulative <= 90.0))[1]


## Evaluate non-english words

library(textcat)

corp_eval_lan <- TermDocumentMatrix(corp_toClean) ## using the unclean data to reduce id error
term_lang <- textcat(corp_eval_lan$dimnames$Terms)

lang_df <- as_tibble(table(term_lang)) %>% arrange(desc(n))
