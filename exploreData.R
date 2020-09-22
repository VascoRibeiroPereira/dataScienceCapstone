source("getCleanData.R")
library(dplyr)
library(ggplot2)

## Uni-gram

corpTDM <- TermDocumentMatrix(corp)

frequentTerms <- findFreqTerms(corpTDM, 100, Inf)

termsHighFreq <- tibble()

for (i in 1:length(frequentTerms)){
        
        tmp <- tm_term_score(corpTDM, frequentTerms[i], 
                             FUN = function(x) sum(x, na.rm = TRUE))
        
        termsHighFreq <- bind_rows(termsHighFreq, 
                                     tibble(Terms = frequentTerms[i], Frequency = tmp))

}

rm(tmp, i, frequentTerms)

termsHighFreq <- arrange(termsHighFreq, desc(Frequency))

ggplot(termsHighFreq, aes(x = reorder(Terms, Frequency), Frequency)) + 
        geom_col(aes(fill = Frequency)) + 
        xlab("Terms") +
        scale_fill_gradient(low = "green", 
                             high = "red") +
        coord_flip()


## 2-grams

corp_2gramTDM <- TermDocumentMatrix(
        corp,
        control = list(tokenize = tokenizer2))


frequentTerms_2gram <- findFreqTerms(corp_2gramTDM, 10, Inf)

termsHighFreq_2gram <- tibble()

for (i in 1:length(frequentTerms_2gram)){
        
        tmp <- tm_term_score(corp_2gramTDM, frequentTerms_2gram[i], 
                             FUN = function(x) sum(x, na.rm = TRUE))
        
        termsHighFreq_2gram <- bind_rows(termsHighFreq_2gram, 
                                         tibble(Terms = frequentTerms_2gram[i], Frequency = tmp))
        
}

rm(tmp, i, frequentTerms_2gram)

termsHighFreq_2gram <- arrange(termsHighFreq_2gram, desc(Frequency))

ggplot(termsHighFreq_2gram, aes(x = reorder(Terms, Frequency), Frequency)) + 
        geom_col(aes(fill = Frequency)) + 
        xlab("Terms") +
        scale_fill_gradient(low = "green", 
                            high = "red") +
        coord_flip()



## 3-grams

corp_3gramTDM <- TermDocumentMatrix(
        corp,
        control = list(tokenize = tokenizer3))


frequentTerms_3gram <- findFreqTerms(corp_3gramTDM, 3, Inf)

termsHighFreq_3gram <- tibble()

for (i in 1:length(frequentTerms_3gram)){
        
        tmp <- tm_term_score(corp_3gramTDM, frequentTerms_3gram[i], 
                             FUN = function(x) sum(x, na.rm = TRUE))
        
        termsHighFreq_3gram <- bind_rows(termsHighFreq_3gram, 
                                         tibble(Terms = frequentTerms_3gram[i], Frequency = tmp))
        
}

rm(tmp, i, frequentTerms_3gram)

termsHighFreq_3gram <- arrange(termsHighFreq_3gram, desc(Frequency))

ggplot(termsHighFreq_3gram, aes(x = reorder(Terms, Frequency), Frequency)) + 
        geom_col(aes(fill = Frequency)) + 
        xlab("Terms") +
        scale_fill_gradient(low = "green", 
                            high = "red") +
        coord_flip()
