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

yelp_1g <- new(FrequencyDictionary,4,2^26)



