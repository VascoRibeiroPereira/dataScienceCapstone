library(quanteda)
library(data.table)
library(stringr)

twitterDF <- read_csv("twitter_subset.csv")

set.seed(1558)
lineSelection <- sample(length(twitterDF$line), length(twitterDF$line)*.05)
sampleDF <- twitterDF[lineSelection,] ## subset 5% of the data


## Add ^"SOS" and $"EOS"

for (i in 1:length(sampleDF$line)) {
        
        sampleDF$text[i] <- paste("SOS", sampleDF$text[i], "EOS", sep=" ")
        
}


ltcorpus <- sampleDF$text



getNgramFreqs <- function(ng, dat, ignores=NULL,
                          sort.by.ngram=TRUE, sort.by.freq=FALSE) {
        
        toks <- tokens(dat, remove_punct = TRUE)
        toks_ngram <- tokens_ngrams(toks, n = ng)
        
        if(is.null(ignores)) {
                #dat.dfm <- dfm(dat, ngrams=ng, toLower = FALSE, removePunct = FALSE,
                #               what = "fasterword", verbose = FALSE)
                dat.dfm <- dfm(toks_ngram)
        } else {
                #dat.dfm <- dfm(dat, ngrams=ng, toLower = FALSE, ignoredFeatures=ignores,
                #               removePunct = FALSE, what = "fasterword", verbose = FALSE)
                dat.dfm <- dfm(toks_ngram, ignoredFeatures=ignores)
        }
        rm(dat)
        # quanteda docfreq will get the document frequency of terms in the dfm
        ngram.freq <- docfreq(dat.dfm)
        if(sort.by.freq) { ngram.freq <- sort(ngram.freq, decreasing=TRUE) }
        if(sort.by.ngram) { ngram.freq <- ngram.freq[sort(names(ngram.freq))] }
        rm(dat.dfm)
        
        return(ngram.freq)
}

## Returns a 2 column data.table. The first column: ngram, contains all the
## unigrams, bigrams, or trigrams in the corpus depending on whether
## ng = 1, 2, or 3 respectively. The second column: freq, contains the
## frequency or count of the ngram found in linesCorpus.
##
## ng - Defines the type of n-gram to be extracted: unigram if ng=1,
##      bigram if ng=2, trigram if n=3, etc.
## linesCorpus - character vector: each element is a line from a corpus file
## prefixFilter - character vector: If not NULL, tells the function to return
##                only rows where the ngram column starts with prefixFilter.
##                If NULL, returns all the ngram and count rows.
getNgramTables <- function(ng, linesCorpus, prefixFilter=NULL) {
        ngrams <- getNgramFreqs(ng, linesCorpus)
        ngrams_dt <- data.table(ngram=names(ngrams), freq=ngrams)
        if(length(grep('^SOS', ngrams_dt$ngram)) > 0) {
                ngrams_dt <- ngrams_dt[-grep('^SOS', ngrams_dt$ngram),]
        }
        if(!is.null(prefixFilter)) {
                regex <- sprintf('%s%s', '^', prefixFilter)
                ngrams_dt <- ngrams_dt[grep(regex, ngrams_dt$ngram),]
        }
        
        return(ngrams_dt)
}

unigs <- getNgramTables(1, ltcorpus)
bigrs <- getNgramTables(2, ltcorpus)
trigs <- getNgramTables(3, ltcorpus)



# trigs[2009] ## a_scary_movie

gamma2 <- 0.5  # bigram discount
gamma3 <- 0.5  # trigram discount
bigPre <- 'live_and'


getObsTrigs <- function(bigPre, trigrams) {
        trigs.winA <- data.frame(ngrams=vector(mode = 'character', length = 0),
                                 freq=vector(mode = 'integer', length = 0))
        regex <- sprintf("%s%s%s", "^", bigPre, "_")
        trigram_indices <- grep(regex, trigrams$ngram)
        if(length(trigram_indices) > 0) {
                trigs.winA <- trigrams[trigram_indices, ]
        }
        
        return(trigs.winA)
}

getObsTriProbs <- function(obsTrigs, bigrs, bigPre, triDisc=0.5) {
        if(nrow(obsTrigs) < 1) return(NULL)
        obsCount <- filter(bigrs, ngram==bigPre)$freq[1]
        obsTrigProbs <- mutate(obsTrigs, freq=((freq - triDisc) / obsCount))
        colnames(obsTrigProbs) <- c("ngram", "prob")
        
        return(obsTrigProbs)
}

obs_trigs <- getObsTrigs(bigPre, trigs)  # get trigrams and counts
# convert counts to probabilities
qbo_obs_trigrams <- getObsTriProbs(obs_trigs, bigrs, bigPre, gamma3)
#qbo_obs_trigrams


getUnobsTrigTails <- function(obsTrigs, unigs) {
        obs_trig_tails <- str_split_fixed(obsTrigs, "_", 3)[, 3]
        unobs_trig_tails <- unigs[!(unigs$ngram %in% obs_trig_tails), ]$ngram
        return(unobs_trig_tails)
}

unobs_trig_tails <- getUnobsTrigTails(obs_trigs$ngram, unigs)
#unobs_trig_tails



getAlphaBigram <- function(unigram, bigrams, bigDisc=0.5) {
        # get all bigrams that start with unigram
        regex <- sprintf("%s%s%s", "^", unigram$ngram[1], "_")
        bigsThatStartWithUnig <- bigrams[grep(regex, bigrams$ngram),]
        if(nrow(bigsThatStartWithUnig) < 1) return(0)
        alphaBi <- 1 - (sum(bigsThatStartWithUnig$freq - bigDisc) / unigram$freq)
        
        return(alphaBi)
}

unig <- str_split(bigPre, "_")[[1]][2]
unig <- unigs[unigs$ngram == unig,]
alpha_big <- getAlphaBigram(unig, bigrs, gamma2)
#alpha_big

getBoBigrams <- function(bigPre, unobsTrigTails) {
        w_i_minus1 <- str_split(bigPre, "_")[[1]][2]
        boBigrams <- paste(w_i_minus1, unobsTrigTails, sep = "_")
        return(boBigrams)
}

getObsBoBigrams <- function(bigPre, unobsTrigTails, bigrs) {
        boBigrams <- getBoBigrams(bigPre, unobsTrigTails)
        obs_bo_bigrams <- bigrs[bigrs$ngram %in% boBigrams, ]
        return(obs_bo_bigrams)
}


getUnobsBoBigrams <- function(bigPre, unobsTrigTails, obsBoBigram) {
        boBigrams <- getBoBigrams(bigPre, unobsTrigTails)
        unobs_bigs <- boBigrams[!(boBigrams %in% obsBoBigram$ngram)]
        return(unobs_bigs)
}

getObsBigProbs <- function(obsBoBigrams, unigs, bigDisc=0.5) {
        first_words <- str_split_fixed(obsBoBigrams$ngram, "_", 2)[, 1]
        first_word_freqs <- unigs[unigs$ngram %in% first_words, ]
        obsBigProbs <- (obsBoBigrams$freq - bigDisc) / first_word_freqs$freq
        obsBigProbs <- data.frame(ngram=obsBoBigrams$ngram, prob=obsBigProbs)
        
        return(obsBigProbs)
}

getQboUnobsBigrams <- function(unobsBoBigrams, unigs, alphaBig) {
        # get the unobserved bigram tails
        qboUnobsBigs <- str_split_fixed(unobsBoBigrams, "_", 2)[, 2]
        w_in_Aw_iminus1 <- unigs[!(unigs$ngram %in% qboUnobsBigs), ]
        # convert to data.frame with counts
        qboUnobsBigs <- unigs[unigs$ngram %in% qboUnobsBigs, ]
        denom <- sum(qboUnobsBigs$freq)
        # converts counts to probabilities
        qboUnobsBigs <- data.frame(ngram=unobsBoBigrams,
                                   prob=(alphaBig * qboUnobsBigs$freq / denom))
        
        return(qboUnobsBigs)
}

bo_bigrams <- getBoBigrams(bigPre, unobs_trig_tails)  # get backed off bigrams
# separate bigrams which use eqn 10 and those that use 16
obs_bo_bigrams <- getObsBoBigrams(bigPre, unobs_trig_tails, bigrs)
unobs_bo_bigrams <- getUnobsBoBigrams(bigPre, unobs_trig_tails, obs_bo_bigrams)
# unobs_bo_bigrams = c("the_buy", "the_EOS", "the_paint", "the_sell", "the_the")
# calc obs'd bigram prob's from eqn 10
qbo_obs_bigrams <- getObsBigProbs(obs_bo_bigrams, unigs, gamma2) #ngram     probs
# calc alpha_big & unobs'd bigram prob's from eqn 16             #the_house 0.3125
unig <- str_split(bigPre, "_")[[1]][2]
unig <- unigs[unigs$ngram == unig,]
# distrib discounted bigram prob mass to unobs bigrams in prop to unigram ML
qbo_unobs_bigrams <- getQboUnobsBigrams(unobs_bo_bigrams, unigs, alpha_big)
qbo_bigrams <- rbind(qbo_obs_bigrams, qbo_unobs_bigrams)
#qbo_bigrams



unobs <- qbo_bigrams[-1,]
#sum(unobs$prob)

getAlphaTrigram <- function(obsTrigs, bigram, triDisc=0.5) {
        if(nrow(obsTrigs) < 1) return(1)
        alphaTri <- 1 - sum((obsTrigs$freq - triDisc) / bigram$freq[1])
        
        return(alphaTri)
}

bigram <- bigrs[bigrs$ngram %in% bigPre, ]
alpha_trig <- getAlphaTrigram(obs_trigs, bigram, gamma3)
#alpha_trig

getUnobsTriProbs <- function(bigPre, qboObsBigrams,
                             qboUnobsBigrams, alphaTrig) {
        qboBigrams <- rbind(qboObsBigrams, qboUnobsBigrams)
        qboBigrams <- qboBigrams[order(-qboBigrams$prob), ]
        sumQboBigs <- sum(qboBigrams$prob)
        first_bigPre_word <- str_split(bigPre, "_")[[1]][1]
        unobsTrigNgrams <- paste(first_bigPre_word, qboBigrams$ngram, sep="_")
        unobsTrigProbs <- alphaTrig * qboBigrams$prob / sumQboBigs
        unobsTrigDf <- data.frame(ngram=unobsTrigNgrams, prob=unobsTrigProbs)
        
        return(unobsTrigDf)
}

qbo_unobs_trigrams <- getUnobsTriProbs(bigPre, qbo_obs_bigrams,
                                       qbo_unobs_bigrams, alpha_trig)
#qbo_unobs_trigrams

getPredictionMsg <- function(qbo_trigs) {
        # pull off tail word of highest prob trigram
        prediction <- str_split(qbo_trigs$ngram[1], "_")[[1]][3]
        result <- sprintf("%s%s%s%.4f", "highest prob prediction is >>> ", prediction,
                          " <<< which has probability = ", qbo_trigs$prob[1])
        return(result)
}

qbo_trigrams <- rbind(qbo_obs_trigrams, qbo_unobs_trigrams)
qbo_trigrams <- qbo_trigrams[order(-qbo_trigrams$prob), ]  # sort by desc prob
out_msg <- getPredictionMsg(qbo_trigrams)
out_msg


