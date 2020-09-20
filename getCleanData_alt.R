## Get the data

if (!file.exists("Coursera-SwiftKey.zip")) {
        download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip",
                      "Coursera-SwiftKey.zip")
        unzip("Coursera-SwiftKey.zip")
}

## Tokenization and Profanity filtering

### Random selection of data
set.seed(123)
lineSelection <- rbinom(10000,1,.5)

con <- file(paste(getwd(), "/final/en_US/en_US.twitter.txt", sep=""), 
            "r")

enDataSubset <- integer()

for (i in 1:length(lineSelection)){
        tmp <- readLines(con, lineSelection[i], skipNul = TRUE)
        enDataSubset <- c(enDataSubset, tmp)
}

close(con)

rm(tmp, lineSelection, i, con) ## clean unused variables

## Stemming

library(tm)

corp <- Corpus(VectorSource(enDataSubset))
dtm <- DocumentTermMatrix(corp) ## para que serve isto?

corp <- tm_map(corp,stemDocument) ## Stem
corp <- tm_map(corp,stripWhitespace) ## Remove white space
corp <- tm_map(corp,content_transformer(tolower)) ## transform all characters to lower case
myStopWords <- stopwords("en") 
corp <- tm_map(corp, removeWords, myStopWords) ## remove low entropy words

library(lexicon)
profaneWords <- unique(tolower(c(profanity_arr_bad, 
                                 profanity_banned,
                                 profanity_racist)))

corp <- tm_map(corp, removeWords, profaneWords) ## remove profanity words


#library(wordnet)
#setDict("/usr/local/Cellar/wordnet/3.1/dict")





