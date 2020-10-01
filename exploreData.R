source("getCleanData.R")

## Corpus to string
corp_str <- concatenate(text=unlist(lapply(corp, "[", "content")))
corp_str <- preprocess(corp_str, case = "lower", 
                       remove.punct = TRUE, 
                       remove.numbers = TRUE, 
                       fix.spacing = FALSE)


## n-gram tables

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
ng_2 <- ngram_freq(corp_final, 2)
ng_3 <- ngram_freq(corp_final, 3)
ng_4 <- ngram_freq(corp_final, 4)

## plots

plot_1_gram <- ggplot(subset(ng_1, freq %in% 100:500), aes(x = reorder(ngrams, freq), freq)) + 
        geom_col(aes(fill = freq)) + 
        xlab("Terms") +
        ylab("Frequency") +
        ggtitle("1-gram Frequency Chart", subtitle = waiver()) +
        scale_fill_gradient(low = "green", 
                            high = "red") +
        coord_flip()

plot_2_gram <- ggplot(subset(ng_2, freq %in% 10:50), aes(x = reorder(ngrams, freq), freq)) + 
        geom_col(aes(fill = freq)) + 
        xlab("Terms") +
        ylab("Frequency") +
        ggtitle("1-gram Frequency Chart", subtitle = waiver()) +
        scale_fill_gradient(low = "green", 
                            high = "red") +
        coord_flip()

plot_3_gram <- ggplot(subset(ng_3, freq %in% 3:10), aes(x = reorder(ngrams, freq), freq)) + 
        geom_col(aes(fill = freq)) + 
        xlab("Terms") +
        ylab("Frequency") +
        ggtitle("1-gram Frequency Chart", subtitle = waiver()) +
        scale_fill_gradient(low = "green", 
                            high = "red") +
        coord_flip()

## Frequency Sorted dictionary

c_ng1 <- ng_1 %>%
        mutate(Cumulative = cumsum(prop))

c_ng2 <- ng_2 %>%
        mutate(Cumulative = cumsum(prop))

c_ng3 <- ng_3 %>%
        mutate(Cumulative = cumsum(prop))

## Plots of frequency sorted dictionary expressed in Cumulative Proportion

plot_c_ng1 <- ggplot(c_ng1, aes(1:dim(c_ng1)[1], Cumulative)) + 
        geom_line() +
        annotate("rect", xmin = 0, xmax = dim(c_ng1 %>% filter(Cumulative <= .500))[1], 
                 ymin = 0, ymax = .50, alpha = .4) +
        annotate("text", x = 1000, y = .25, label = "<<<< 50% Cover") +
        annotate("rect", xmin = 0, xmax = dim(c_ng1 %>% filter(Cumulative <= .900))[1], 
                 ymin = 0, ymax = .90, alpha = .3) +
        annotate("text", x = 6000, y = .96, label = "<<<< 90% Cover") +
        annotate("rect", xmin = 0, xmax = dim(c_ng1 %>% filter(Cumulative <= 1.000))[1], 
                 ymin = 0, ymax = 1.00, alpha = .2) +
        annotate("text", x = 8500, y = 1.05, label = "<<<< 100% Cover") +
        xlab("Number of Terms") +
        ylab("Cumulative Proportion")

plot_c_ng2 <- ggplot(c_ng2, aes(1:dim(c_ng2)[1], Cumulative)) + 
        geom_line() +
        annotate("rect", xmin = 0, xmax = dim(c_ng2 %>% filter(Cumulative <= .500))[1], 
                 ymin = 0, ymax = .50, alpha = .4) +
        annotate("text", x = 10000, y = .25, label = "<<<< 50% Cover") +
        annotate("rect", xmin = 0, xmax = dim(c_ng2 %>% filter(Cumulative <= .900))[1], 
                 ymin = 0, ymax = .90, alpha = .3) +
        annotate("text", x = 15000, y = .96, label = "<<<< 90% Cover") +
        annotate("rect", xmin = 0, xmax = dim(c_ng2 %>% filter(Cumulative <= 1.000))[1], 
                 ymin = 0, ymax = 1.00, alpha = .2) +
        annotate("text", x = 30000, y = 1.05, label = "<<<< 100% Cover") +
        xlab("Number of Terms") +
        ylab("Cumulative Proportion")

plot_c_ng3 <- ggplot(c_ng3, aes(1:dim(c_ng3)[1], Cumulative)) + 
        geom_line() +
        annotate("rect", xmin = 0, xmax = dim(c_ng3 %>% filter(Cumulative <= .500))[1], 
                 ymin = 0, ymax = .50, alpha = .4) +
        annotate("text", x = 12000, y = .25, label = "<<<< 50% Cover") +
        annotate("rect", xmin = 0, xmax = dim(c_ng3 %>% filter(Cumulative <= .900))[1], 
                 ymin = 0, ymax = .90, alpha = .3) +
        annotate("text", x = 28000, y = .96, label = "<<<< 90% Cover") +
        annotate("rect", xmin = 0, xmax = dim(c_ng3 %>% filter(Cumulative <= 1.000))[1], 
                 ymin = 0, ymax = 1.00, alpha = .2) +
        annotate("text", x = 31000, y = 1.05, label = "<<<< 100% Cover") +
        xlab("Number of Terms") +
        ylab("Cumulative Proportion")


### Number of unique words to cover 50%
dim(c_ng1 %>% filter(Cumulative <= .50))[1]
dim(c_ng2 %>% filter(Cumulative <= .50))[1]
dim(c_ng3 %>% filter(Cumulative <= .50))[1]

### Number of unique words to cover 90%
dim(c_ng1 %>% filter(Cumulative <= .90))[1]
dim(c_ng2 %>% filter(Cumulative <= .90))[1]
dim(c_ng3 %>% filter(Cumulative <= .90))[1]

## Evaluate non-english grams

library(textcat)

lang_ng1 <- textcat(c_ng1$ngrams, p=textcat::ECIMCI_profiles, method = "Dice")
lang_ng1_df <- as_tibble(table(lang_ng1)) %>% arrange(desc(n))
lang_ng1_df <- lang_ng1_df %>% mutate(ngram = "1-gram") %>%
        rename(language = lang_ng1)
 

lang_ng2 <- textcat(c_ng2$ngrams, p=textcat::ECIMCI_profiles, method = "Dice")
lang_ng2_df <- as_tibble(table(lang_ng2)) %>% arrange(desc(n))
lang_ng2_df <- lang_ng2_df %>% mutate(ngram = "2-gram") %>%
        rename(language = lang_ng2)

lang_ng3 <- textcat(c_ng3$ngrams, p=textcat::ECIMCI_profiles, method = "Dice")
lang_ng3_df <- as_tibble(table(lang_ng3)) %>% arrange(desc(n))
lang_ng3_df <- lang_ng3_df %>% mutate(ngram = "3-gram") %>%
        rename(language = lang_ng3)


### Plots

# The language identification inproves whith the for each n+1 gram

#lang_ng_n <- bind_rows(lang_ng1_df, lang_ng2_df, lang_ng3_df)
#langPlot <- ggplot(lang_ng_n, aes(language, n)) + geom_col() # barplot
#langPlot + facet_wrap( ~ ngram, nrow = 3) + theme(legend.position = "none")



