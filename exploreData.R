source(getCleanData_alt.R)

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

rm(tmp)

library(ggplot2)

ggplot(corpTDMHighFreq, aes(NumTerms, Freq)) + geom_point()

### Compute a score based on the high frequency terms

frequentTerms <- findFreqTerms(corpTDM, 100, Inf)

termsHighFreq <- tibble()

for (i in 1:length(frequentTerms)){
        
        tmp <- tm_term_score(corpTDM, frequentTerms[i], 
                             FUN = function(x) sum(x, na.rm = TRUE))
        
        termsHighFreq <- bind_rows(termsHighFreq, 
                                     tibble(Terms = frequentTerms[i], Frequency = tmp))

}

rm(tmp)

termsHighFreq <- arrange(termsHighFreq, desc(Frequency))

ggplot(termsHighFreq, aes(x = reorder(Terms, Frequency), Frequency)) + 
        geom_col(aes(fill = Frequency)) + 
        xlab("Terms") +
        scale_fill_gradient(low = "green", 
                             high = "red") +
        coord_flip()
