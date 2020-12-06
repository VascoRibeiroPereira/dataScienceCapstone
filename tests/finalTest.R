library(readr)
source("getCleanData_fun.R")
source("finalTest_Fun.R")

userInput <- "" ## test it out

evaluateCurses(userInput)

userInputClean <- cleanInNoStop(userInput)

if (sum(count_words(userInputClean)) == 0){

        userInputCleanStop <- cleanInput(userInput)
}

## Evaluate against the ngrams - no sw

if (length(userInputClean) != 0) wordSugestions <- getWordsSuggested(userInputClean, gramList$normal)


if (exists("userInputCleanStop")) {
        wordSugestions <- unique(getWordsSuggested(userInputCleanStop, gramList$sw))
        
}else{
        if (length(wordSugestions) <= 2) { 
                userInputCleanStop <- cleanInput(userInput)
                wordSugestions <- unique(c(wordSugestions, getWordsSuggested(userInputCleanStop, gramList$sw)))
        }
}

wordSugestions
