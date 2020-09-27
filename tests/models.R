# Topic models based on the topicmodels R Package
## This can be interesting to predict more accurately the next words (based on topic being written)



## Think of this process (more advanced than simple n-gram models, this is a distributed representation):
## 1 - Data input: "student"
## 2 - topicmodel determination: "academic"
## 3 - next word prediction: "papers"



#To evaluate:

## skip-gram models?
## n-gram models?
## ngram model with Kneser-Ney smoothing? -> cmscu library
## text2vec package?
## cmscu package?

## Test this case:

library(cmscu)
library(rjson)

# Build Frequency Dictionaries

# unigrams
yelp_1g <- new(FrequencyDictionary,4,2^26)
# bigrams
yelp_2g <- new(FrequencyDictionary,4,2^26)
# trigrams
yelp_3g <- new(FrequencyDictionary,4,2^26)
# 4-grams
yelp_4g <- new(FrequencyDictionary,4,2^26)


# a text cleaning function
clean <- function(line) {
        # upper-case everything
        str <- toupper(line);
        # strip-out small html tags
        str <- gsub('<[^>]{1,2}>', '', str);
        # replace all terminal punctuation with a period
        str <- gsub('[[:space:]]*[.?!:;]+[[:space:]]*', '.', str);
        # get rid of anything not A-Z, ', ., or whitespace
        str <- gsub('[^A-Z\'.[:space:]]', ' ', str);
        # crowd apostrophes
        # str <- gsub("[[:space:]]+([A-Z]*'[A-Z]*)", "\\1", str);
        # collapse whitespace
        str <- gsub('[[:space:]]+', ' ', str);
        # make sure contraction's are "tight"
        str <- gsub(" ?' ?", "'", str);
        # make sure terminal . are tight
        str <- gsub(' ?\\. ?', '.', str);
        return(str);
}

# this function lets us create n-grams from a list
ngrams <- function(lst, n) {
        len <- length(lst);
        sapply(1:(len-n+1), function(i) do.call(paste, as.list(lst[i:(i+n-1)])))
}


# connect to the file, but don't load the contents! 
Yelpfile <- file('~/Downloads/yelp_academic_dataset_review.json', 'r', FALSE); 
# I'm using a smaller dataset than ^^ this. I cut the dataset down to roughly 10,000 reviews
# i <- 0 #create an index to break the loop early
repeat {
        # select the number of reviews to read at a time. 500 = ~550kb. 
        reviews <- readLines(con=Yelpfile, n=500);
        # Break loop when you reach the end of the file
        # if (i>1000){ #only loop through 1000 reviews for testing your loop
        if (length(reviews) == 0) { #comment out if you only want to test loop on first 1000 reviews
                # disconnect the file link
                close(Yelpfile);
                # break the loop
                break;
        }
        # read a single review 
        for (review in reviews){
                # parse the current review
                currev <- fromJSON(review)
                # clean the current review
                text <- clean(currev$text)
                # split reviews into sentences
                for (sentence in unlist(strsplit(text, '\\.'))) {
                        # split to unigrams    
                        unilist <- unlist(strsplit(sentence, ' ')) 
                        # store unigrams
                        yelp_1g$store(unilist)
                        # add beginning and end of sentence tags to unigrams, and subsequent n-grams 
                        # (crucial for smoothing ngrams in test phase)
                        bilist <- c("<BOS>",unilist,'<EOS>')
                        # store bigrams, use the "ngrams" function bind unigrams together
                        yelp_2g$store(ngrams(bilist,2))
                        # store trigrams    
                        trilist <- c("<BOS>","<BOS>",unilist,'<EOS>')
                        yelp_3g$store(ngrams(trilist,3))
                        # store quadgrams
                        qualist <- c("<BOS>","<BOS>","<BOS>",unilist,'<EOS>')
                        yelp_4g$store(ngrams(qualist,4))
                }
        }
         cat('\r', paste('Trained', i, 'lines from yelp.')); #this will track your progress through your dataset!
}



# query the unigram dictionary
yelp_1g$query(c("SEE","PIZZA","JUXTAPOSED"))

# query the bigram dictionary
yelp_2g$query(c("<BOS> I","THE PIZZA","SHE JUXTAPOSED"))

# The number of counts we might be over-estimating by
yelp_1g$uncertainty


# probability that we are not over-estimating by more than our 'uncertainty'
yelp_1g$confidence

#how full the your hash tables are. zero means there's no risk in collision
yelp_1g$density

# number of total n-grams in the corpus 
yelp_1g$entries

# number of unique n-grams in the corpus
yelp_1g$unique_entries

# save your dictionary to in .bin
yelp_1g$save("yelp1.bin")


# Load from .bin and Query!
# make sure your dictionary is the same size! 
frombinary_1g <- new(FrequencyDictionary, 4, 2^26)
# load it in
frombinary_1g$read("yelp1.bin") 

# query to test
frombinary_1g$query(c("SEE","PIZZA","JUXTAPOSED"))

yelp_2g$query(c("<BOS> I","THE PIZZA","SHE JUXTAPOSED"))




