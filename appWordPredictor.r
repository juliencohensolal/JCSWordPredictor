if (!require(stringr)) {install.packages("stringr")}
if (!require(data.table)) {install.packages("data.table")}
library(stringr)
library(data.table)

source("appLangModeler.r")

########################################
########################################

loadModel <- function(level)
{
    fileName <- paste("dictionaries/", level, "grams_40.Rds", sep = "")
    curModel <- readRDS(fileName)
    return(curModel)
}

########################################
########################################

simpleLinearInterpolation <- function(iString, gram4Model = NULL, gram3Model = NULL, gram2Model = NULL, gram1Model = NULL, encoder = NULL, profFilter = FALSE)
{
    scoresSLI <- numeric(160)
    if (is.null(encoder))    {encoder <- readRDS("dictionaries/encoder_40.Rds")}
    
    # Define lambda values
    lambda4 <- 0.55
    lambda3 <- 0.35
    lambda2 <- 0.099
    lambda1 <- 0.001
    
    # First preprocess the strings in the same way the training set has been
    iString <- cleanUpData(iString)
    
    # Get corresponding codes
    word1 <- word(iString[length(iString)], -2)
    word2 <- word(iString[length(iString)], -3)
    word3 <- word(iString[length(iString)], -4)
    code1 <- getCode(encoder, word1)
    code2 <- getCode(encoder, word2)
    code3 <- getCode(encoder, word3)
    if (length(code1) != 0)  
    {
        if ((code1 == 1) && (word1 != getWord(encoder, 1)))  {code1 <- integer(0)}
    }
    if (length(code2) != 0)  
    {
        if ((code2 == 1) && (word2 != getWord(encoder, 1)))  {code2 <- integer(0)}
    }
    if (length(code3) != 0)  
    {
        if ((code3 == 1) && (word3 != getWord(encoder, 1)))  {code3 <- integer(0)}
    }
        
    # Load all models
    if (is.null(gram4Model))    {gram4Model <- loadModel(4)}
    if (is.null(gram3Model))    {gram3Model <- loadModel(3)}
    if (is.null(gram2Model))    {gram2Model <- loadModel(2)}
    if (is.null(gram1Model))    {gram1Model <- loadModel(1)}
    
    # Get n-grams on all levels
    ngram4 <- getMatchingNgrams(code1, code2, code3, 4, gram4Model, gram3Model, gram2Model, gram1Model, encoder)
    ngram3 <- getMatchingNgrams(code1, code2, code3, 3, gram4Model, gram3Model, gram2Model, gram1Model, encoder)
    ngram2 <- getMatchingNgrams(code1, code2, code3, 2, gram4Model, gram3Model, gram2Model, gram1Model, encoder)
    ngram1 <- getMatchingNgrams(code1, code2, code3, 1, gram4Model, gram3Model, gram2Model, gram1Model, encoder)
    
    ngram4$score <- lambda4 * ngram4$proba
    ngram3$score <- lambda3 * ngram3$proba
    ngram2$score <- lambda2 * ngram2$proba
    ngram1$score <- lambda1 * ngram1$proba
    
    # Add respective scores
    # TEMP : only look at first X results of each grams
    i <- 1
    j <- 1
    while(j <= 50)
    {
        # Try to store top quadrigram results first
        score1 <- 0
        score2 <- 0
        score3 <- 0
        if (!is.na(ngram4$score[1]))
        {
            if (is.element(TRUE, (ngram3$W3 == ngram4$W4[1]))) {score3 <- ngram3[ngram3$W3 == ngram4$W4[1]]$score}
            if (is.element(TRUE, (ngram2$W2 == ngram4$W4[1]))) {score2 <- ngram2[ngram2$W2 == ngram4$W4[1]]$score}
            if (is.element(TRUE, (ngram1$W1 == ngram4$W4[1]))) {score1 <- ngram1[ngram1$W1 == ngram4$W4[1]]$score}
            scoresSLI[i] <- ngram4$score[1] + score1 + score2 + score3
            names(scoresSLI)[i] <- ngram4$W4[1]
            
            ngram1 <- ngram1[! ngram1$W1 == ngram4$W4[1]]
            ngram2 <- ngram2[! ngram2$W2 == ngram4$W4[1]]
            ngram3 <- ngram3[! ngram3$W3 == ngram4$W4[1]]
            ngram4 <- ngram4[-1]
            i <- i + 1
        }
        j <- j + 1
    }
    
    j <- 1
    while(j <= 50)
    {
        # Try to store top trigram results then
        score1 <- 0
        score2 <- 0
        if (!is.na(ngram3$score[1]))
        {
            if (is.element(TRUE, (ngram2$W2 == ngram3$W3[1]))) {score2 <- ngram2[ngram2$W2 == ngram3$W3[1]]$score}
            if (is.element(TRUE, (ngram1$W1 == ngram3$W3[1]))) {score1 <- ngram1[ngram1$W1 == ngram3$W3[1]]$score}
            scoresSLI[i] <- ngram3$score[1] + score1 + score2
            names(scoresSLI)[i] <- ngram3$W3[1]
            
            ngram1 <- ngram1[! ngram1$W1 == ngram3$W3[1]]
            ngram2 <- ngram2[! ngram2$W2 == ngram3$W3[1]]
            ngram3 <- ngram3[-1]
            i <- i + 1
        }
        j <- j + 1
    }
    
    j <- 1
    while(j <= 50)
    {
        # Try to store top bigram results then
        score1 <- 0
        if (!is.na(ngram2$score[1]))
        {
            if (is.element(TRUE, (ngram1$W1 == ngram3$W3[1]))) {score1 <- ngram1[ngram1$W1 == ngram3$W3[1]]$score}
            scoresSLI[i] <- ngram2$score[1] + score1
            names(scoresSLI)[i] <- ngram2$W2[1]
            
            ngram1 <- ngram1[! ngram1$W1 == ngram2$W2[1]]
            ngram2 <- ngram2[-1]
            i <- i + 1
        }
        j <- j + 1
    }
    
    j <- 1
    while(j <= 10)
    {
        # Try to store top unigram results then
        if (!is.na(ngram1$score[1]))
        {
            scoresSLI[i] <- ngram1$score[1]
            names(scoresSLI)[i] <- ngram1$W1[1]
            
            ngram1 <- ngram1[-1]
            i <- i + 1
        }
        j <- j + 1
    }
    
    scoresSLI <- sort(scoresSLI, decreasing = TRUE)    
    
    # Check for profanity filter
    if (profFilter) {names(scoresSLI) <- profanityFilter(names(scoresSLI))}
    
    return(head(scoresSLI, 5))
}

########################################
########################################

getMatchingNgrams <- function(code1, code2, code3, level, gram4Model, gram3Model, gram2Model, gram1Model, encoder)
{
    if ((level >= 4))
    {
        # Get results for quadrigrams
        ngrams4 <- gram4Model[gram4Model$W1 == code3]
        ngrams4 <- ngrams4[ngrams4$W2 == code2]
        ngrams4 <- ngrams4[ngrams4$W3 == code1]
        ngrams4 <- ngrams4[with(ngrams4, order(-occurences)), ]
        ngrams4$proba <- ngrams4$occurences / sum(ngrams4$occurences)
        words4 <- data.table(W1 = getWord(encoder, ngrams4$W1), 
                             W2 = getWord(encoder, ngrams4$W2), 
                             W3 = getWord(encoder, ngrams4$W3), 
                             W4 = getWord(encoder, ngrams4$W4), 
                             occurences = ngrams4$occurences, 
                             proba = ngrams4$proba)
        return(words4)
    }
    
    if (level == 3)
    {
        # Get results for trigrams
        ngrams3 <- gram3Model[gram3Model$W1 == code2]
        ngrams3 <- ngrams3[ngrams3$W2 == code1]
        ngrams3 <- ngrams3[with(ngrams3, order(-occurences)), ]
        ngrams3$proba <- ngrams3$occurences / sum(ngrams3$occurences)
        words3 <- data.table(W1 = getWord(encoder, ngrams3$W1), 
                             W2 = getWord(encoder, ngrams3$W2), 
                             W3 = getWord(encoder, ngrams3$W3), 
                             occurences = ngrams3$occurences, 
                             proba = ngrams3$proba)
        return(words3)
    }
    
    if (level == 2) 
    {
        # Get results for bigrams
        ngrams2 <- gram2Model[gram2Model$W1 == code1]
        ngrams2 <- ngrams2[with(ngrams2, order(-occurences)), ]
        ngrams2$proba <- ngrams2$occurences / sum(ngrams2$occurences)
        words2 <- data.table(W1 = getWord(encoder, ngrams2$W1), 
                             W2 = getWord(encoder, ngrams2$W2), 
                             occurences = ngrams2$occurences, 
                             proba = ngrams2$proba)
        return(words2)
    }
    
    if (level == 1) 
    {
        # Get results for words
        ngrams1 <- gram1Model[order(-occurences), ]
        ngrams1 <- ngrams1[1:5]
        ngrams1$proba <- ngrams1$occurences / sum(ngrams1$occurences)
        words1 <- data.table(W1 = getWord(encoder, ngrams1$W1), 
                             occurences = ngrams1$occurences, 
                             proba = ngrams1$proba)
        return(words1)
    }
}
