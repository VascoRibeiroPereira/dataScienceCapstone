library(readr)
source("getCleanData_fun.R")

withSW <- read_csv("twitter_subset.csv")
withoutSW <- read_csv("withoutSW.csv")

userInput <- "my bad"

evaluateCurses <- function(userInput) {
        userInput <- unlist(str_split(userInput, " "))
        curseCount <- as.integer()
        for (i in 1:length(userInput)){
                curseCount <- sum(curseCount, 
                                  grepl(paste("^", userInput[i], "$" ,sep=""),
                                        profaneWords))
        }
        
        if (curseCount > 0){
                returnMessage <- "Don't use curse words please."
                return(returnMessage)
        }
}

evaluateCurses(userInput)

cleanInNoStop <- function(userInput) {
        userInputOut <- userInput %>%
                replace_contraction() %>%
                removePunctuation() %>%
                replace_money() %>%
                replace_emoticon() %>%
                replace_symbol() %>%
                replace_word_elongation() %>%
                replace_ordinal() %>%
                replace_number() %>%
                tolower() %>%
                removeWords(stopwords()) %>%
                stripWhitespace() %>%
                trimws() %>%
                str_split(pattern = " ") %>%
                unlist()

        #userInputOut <- userInputOut[-grep("^$", userInputOut)]
        
        return(userInputOut)
}

userInputOut <- cleanInNoStop(userInput)

cleanInput <- function(userInput) {
        userInputOut <- userInput %>%
                replace_contraction() %>%
                removePunctuation() %>%
                replace_money() %>%
                replace_emoticon() %>%
                replace_symbol() %>%
                replace_word_elongation() %>%
                replace_ordinal() %>%
                replace_number() %>%
                stripWhitespace() %>%
                tolower() %>%
                str_split(pattern = " ") %>%
                unlist()
        
        return(userInputOut)
}

userInputWith <- cleanInput(userInput)


