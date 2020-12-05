# Final Test ngram inport and functions

load("gramList.RData")

## Evaluate curses

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

## Clean without stop words

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
        
        return(userInputOut)
}

## Clean with stop words

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

## Get the words suggested

getWordsSuggested <- function(userInputClean, gramList) {
        
        if (length(userInputClean) > 4) {
                userInput_crop <- userInputClean[(length(userInputClean)-3):length(userInputClean)]
                userInput_crop <- paste("^", paste(userInput_crop, collapse = " "), sep="")
        } else {
                userInput_crop <- paste("^", paste(userInputClean, collapse = " "), sep="")
        }
        
        freqGramOutput <- tibble()
        
        if (count_words(userInput_crop) == 4) {
                
                freqGramOutput <- rbind(freqGramOutput, gramList[[5]][grep(userInput_crop, gramList[[5]]$ngrams),]) ## 4 words search in pentagram
                
                userInput_crop <- unlist(str_split(userInput_crop, " "))
                userInput_crop <- paste("^", paste(userInput_crop[2:length(userInput_crop)], collapse = " "), sep="")
                
                freqGramOutput <- rbind(freqGramOutput, gramList[[4]][grep(userInput_crop, gramList[[4]]$ngrams),][1:2,]) ## 3 words search in tetragram
                
                userInput_crop <- unlist(str_split(userInput_crop, " "))
                userInput_crop <- paste("^", paste(userInput_crop[2:length(userInput_crop)], collapse = " "), sep="")
                
                freqGramOutput <- rbind(freqGramOutput, gramList[[3]][grep(userInput_crop, gramList[[3]]$ngrams),][1:2,]) ## 2 words search in trigram
                
                userInput_crop <- unlist(str_split(userInput_crop, " "))
                userInput_crop <- paste("^", paste(userInput_crop[2:length(userInput_crop)], collapse = " "), sep="")
                
                freqGramOutput <- rbind(freqGramOutput, gramList[[2]][grep(userInput_crop, gramList[[2]]$ngrams),][1:2,]) ## 1 word search in bigram
                
        }
        
        if (count_words(userInput_crop) == 3) {
                
                freqGramOutput <- rbind(freqGramOutput, gramList[[4]][grep(userInput_crop, gramList[[4]]$ngrams),][1:2,]) ## 3 words search in tetragram
                
                userInput_crop <- unlist(str_split(userInput_crop, " "))
                userInput_crop <- paste("^", paste(userInput_crop[2:length(userInput_crop)], collapse = " "), sep="")
                
                freqGramOutput <- rbind(freqGramOutput, gramList[[3]][grep(userInput_crop, gramList[[3]]$ngrams),][1:2,]) ## 2 words search in trigram
                
                userInput_crop <- unlist(str_split(userInput_crop, " "))
                userInput_crop <- paste("^", paste(userInput_crop[2:length(userInput_crop)], collapse = " "), sep="")
                
                freqGramOutput <- rbind(freqGramOutput, gramList[[2]][grep(userInput_crop, gramList[[2]]$ngrams),][1:2,]) ## 1 word search in bigram
                
        }
        
        
        if (count_words(userInput_crop) == 2) {
                
                freqGramOutput <- rbind(freqGramOutput, gramList[[3]][grep(userInput_crop, gramList[[3]]$ngrams),][1:2,]) ## 2 words search in trigram
                
                userInput_crop <- unlist(str_split(userInput_crop, " "))
                userInput_crop <- paste("^", paste(userInput_crop[2:length(userInput_crop)], collapse = " "), sep="")
                
                freqGramOutput <- rbind(freqGramOutput, gramList[[2]][grep(userInput_crop, gramList[[2]]$ngrams),][1:2,]) ## 1 word search in bigram
                
        }
        
        
        if (count_words(userInput_crop) == 1) {
                
                freqGramOutput <- rbind(freqGramOutput, gramList[[2]][grep(userInput_crop, gramList[[2]]$ngrams),][1:2,]) ## 1 word search in bigram
                
        }
        
        
        gramOutput <- drop_na(freqGramOutput)
        gramOutput <- gramOutput$ngrams
        wordSugestions <- unique(unlist(lapply(str_split(gramOutput, " "), last))) ## can turn t6his better by display the most resurrent words first
        
        return(wordSugestions)
        
}




