
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

enDataSubset <- subsetBigData(paste(getwd(), "/reports/final/en_US/en_US.twitter.txt", sep=""), .2)

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

## Function
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
                        removeWords(profaneWords) %>%
                        removeWords(stopwords())
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
        if (n >= 1) {
                n_gram <- as_tibble(get.phrasetable(ngram(x[count_words(x) >= n], n)))
        }else{
                n_gram <- as_tibble(get.phrasetable(ngram(x, 1)))
        }
        n_gram$ngrams <- gsub(" $", "", n_gram$ngrams)
        return(n_gram)
}

ng_1 <- ngram_freq(corp_final, 1)




