## Get the data

if (!file.exists("Coursera-SwiftKey.zip")) {
        download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip",
                      "Coursera-SwiftKey.zip")
        unzip("Coursera-SwiftKey.zip")
}


## source Libraries and Functions

source("getCleanData_fun.R")

## Clean data

### Random selection of data

enDataSubset <- subsetBigData(paste(getwd(), "/reports/final/en_US/en_US.twitter.txt", sep=""), 2)

## Transform data to Corpus and clean with an anonymous function

corp_toClean <- VCorpus(VectorSource(enDataSubset))

## Without stop words

twitterDataClean <- clean_corpus(corp_toClean)


## Corpus to string
twitter_str <- as.character(unlist(twitterDataClean))
## Remove NA introduced by the cleaning algorithm
twitter_str <- twitter_str[!is.na(twitter_str)]


## Preprocess
twitter_final <- as.character()

for (i in 1:length(twitter_str)){
        
        tmp <- preprocess(twitter_str[i], case = "lower", 
                          remove.punct = TRUE, 
                          remove.numbers = TRUE, 
                          fix.spacing = TRUE) %>% 
                str_trim()
        
        twitter_final <- c(twitter_final, tmp)
        
}

twitter_Subset <- twitter_final[count_words(twitter_final) > 1]

twitterDF <- tibble(line = 1:length(twitter_Subset), text = twitter_Subset)



write_csv(twitterDF, "twitter_subset.csv")

#twitterDF <- read_csv("twitter_subset.csv")
