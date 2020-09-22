source(getCleanData.R)

library(dplyr)

## Count-based evaluation
corpTDM <- TermDocumentMatrix(corp)

## frequency of terms

lowFreq <- c(50,100,150,200,250,300)

### Find simple frequency and plot

corpTDMHighFreq <- tibble()

for (i in 1:length(lowFreq)){

        tmp <- length(findFreqTerms(corpTDM, lowFreq[i], Inf))
        corpTDMHighFreq <- bind_rows(corpTDMHighFreq, 
                  tibble(Freq = lowFreq[i], NumTerms = tmp))
}

rm(tmp, lowFreq)

library(ggplot2)

ggplot(corpTDMHighFreq, aes(NumTerms, Freq)) + geom_point()


### Compute a score based on the high frequency terms and plot top 20

frequentTerms <- findFreqTerms(corpTDM, 100, Inf)

termsHighFreq <- tibble()

for (i in 1:length(frequentTerms)){
        
        tmp <- tm_term_score(corpTDM, frequentTerms[i], 
                             FUN = function(x) sum(x, na.rm = TRUE))
        
        termsHighFreq <- bind_rows(termsHighFreq, 
                                     tibble(Terms = frequentTerms[i], Frequency = tmp))

}

rm(tmp, i)

termsHighFreq <- arrange(termsHighFreq, desc(Frequency))

ggplot(termsHighFreq, aes(x = reorder(Terms, Frequency), Frequency)) + 
        geom_col(aes(fill = Frequency)) + 
        xlab("Terms") +
        scale_fill_gradient(low = "green", 
                             high = "red") +
        coord_flip()

### Compute a score based on the 2-grams frequency terms and plot


### Word Association - Cluster Analysis

corpTDM2 <- removeSparseTerms(corpTDM, sparse = 0.98)
hc <- hclust(d = dist(corpTDM2, method = "euclidean"), method = "complete")
hcd <- as.dendrogram(hc)

plot(hc)

### Word Association - 1-gram

library(qdap)

associations <- findAssocs(corpTDM, frequentTerms, 0.2)

associations_df <- tibble()


for (i in 1:length(associations)){
        if (length(associations[[i]]) >= 1) {
                tmp <- list_vect2df(associations[i])
                associations_df <- bind_rows(associations_df, tmp)
        }
        
}

associations_df$X1 <- as.factor(associations_df$X1)

ggplot(associations_df, aes(X3, X2, color = X1)) + geom_point(size = 5)

### Word Association - 2-gram
library(janeaustenr)

tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))


bigram_tdm <- TermDocumentMatrix(corp, control = list(tokenize = tokenizer))

frequentTerms_bi <- findFreqTerms(bigram_tdm, 100, Inf)

associations_bi <- findAssocs(bigram_tdm, frequentTerms_bi, 0.2)

associations_bi_df <- tibble()


for (i in 1:length(associations_bi)){
        if (length(associations_bi[[i]]) >= 1) {
                tmp <- list_vect2df(associations_bi[i])
                associations_bi_df <- bind_rows(associations_bi_df, tmp)
        }
        
}

associations_bi_df$X1 <- as.factor(associations_df$X1)

ggplot(associations_bi_df, aes(X3, X2, color = X1)) + geom_point(size = 5)



## Check wordcloud


bigram_dtm <- DocumentTermMatrix(corp, control = list(tokenize = tokenizer))
bigram_dtm_m <- as.matrix(bigram_dtm)
freq <- colSums(bigram_dtm_m)
bi_words <- names(freq)

