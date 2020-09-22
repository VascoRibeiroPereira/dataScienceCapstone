library(tm)
library(lexicon)

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