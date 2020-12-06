## Lines extracted to model - don't need to run, use the rds file in the next function
#lineSelected <- function(file_Path, percentage) {
#        file_length <- length(readLines(file_Path, warn = FALSE))
#        number_Lines <- as.integer(file_length*(percentage/100))
#        set.seed(123)
#        lineSelection <- sample(file_length, number_Lines)
#        BigDataLines <- saveRDS(lineSelection, "linesExtracted.RData")
#        return(lineSelection)
#}

#linesExtracted <- lineSelected(paste(getwd(), "/reports/final/en_US/en_US.twitter.txt", sep=""), 10)


## Lines to be extracted to build an accuracy of the model

subsetTestData <- function(file_Path, percentage) {
        
        BigDataLines <- readRDS("linesExtracted.RData") ## this are the lines to be excluded (same as position lines)
        file_length <- 1:length(readLines(file_Path, warn = FALSE))
        new_length <- file_length[-BigDataLines]
        number_Lines <- as.integer(length(file_length)*(percentage/100))
        set.seed(123)
        lineSelection <- sample(new_length, number_Lines)
        con <- file(file_Path, "r")
        fileRead <- readLines(con, skipNul = TRUE)[lineSelection]
        close(con)
        return(fileRead)
}

### Random selection of data to test

enDataTest <- subsetTestData(paste(getwd(), "/reports/final/en_US/en_US.twitter.txt", sep=""), 1)






