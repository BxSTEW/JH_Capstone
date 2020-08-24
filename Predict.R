quadgram <- readRDS(file = "./ngrams/quadgram.RData")
trigram <- readRDS(file = "./ngrams/trigram.RData")
bigram <- readRDS(file = "./ngrams/bigram.RData")

#helper function - cleanse input text
cleanse<-function(t){
        tt <- tolower(t)
        tt <- removePunctuation(tt)
        tt <- removeNumbers(tt)
        tt <- str_replace_all(tt, "[^[:alnum:]]", " ") # use regex here
        tt <- stripWhitespace(tt)
        return(tt)
}

# helper function - freshen input by calling cleanse
reFreshen <- function(t){
        tt <- cleanse(t)
        tt <- txt.to.words.ext(tt,corpus.lang ="English.all",preserve.case = TRUE)
        return(tt)
}

# prediction function
getNextWord <- function(wCount,userInput){
        # if the user enters more than three words, use only the last three words entered
        if (wCount >= 3) {
                userInput <- userInput[(wCount-2):wCount] 
                # user enters two words, make first word in vector NA 
        } else if (wCount == 2) {
                userInput <- c(NA,userInput)   
                # user enters one word, make first two in vector NA
        } else if (wCount == 1) {
                userInput <- c(NA,NA,userInput)
        }
        
        # select next word based on user input from quad-, tri-, and bigram dataframes
        nextWords <- as.character(quadgram[quadgram$unigram==userInput[1] & 
                                                   quadgram$bigram==userInput[2] & 
                                                   quadgram$trigram==userInput[3],][1,]$quadgram)
        
        if(is.na(nextWords)) {
                nextWords <- as.character(trigram[trigram$unigram==userInput[2] & 
                                                          trigram$bigram==userInput[3],][1,]$trigram)
                
                if(is.na(nextWords)) {
                        nextWords <- as.character(bigram[bigram$unigram==userInput[3],][1,]$bigram)
                }
        }
        
        cat(nextWords)
        
}
