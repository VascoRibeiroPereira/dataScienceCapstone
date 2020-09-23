## Libraries
library(tokenizers)
source("myFunctions.R")

## Get the data

if (!file.exists("Coursera-SwiftKey.zip")) {
        download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip",
                      "Coursera-SwiftKey.zip")
        unzip("Coursera-SwiftKey.zip")
}

## Clean data

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

## Transform data to Corpus and clean with an anonymous function

corp_toClean <- VCorpus(VectorSource(enDataSubset))
corp <- clean_corpus(corp_toClean)


