if (!require(tm)) {install.packages("tm")}
if (!require(quanteda)) {install.packages("quanteda")}
library(tm)
library(quanteda)


########################################
########################################

cleanUpData <- function(subSet)
{
    # Remove non UTF-8 characters
    subSet <- iconv(subSet, to = "ASCII", sub = " ")
    
    # Remove URLs and emails
    subSet <- removeInternetTerms(subSet)
    
    # Remove numbers
    subSet <- removeDigits(subSet)
    
    # Convert everything to lower case
    subSet <- toLower(subSet)
    
    # Harmonize end of lines
    subSet <- gsub("(\\?)|(\\!)|(\\:)|(\n)|(\r)", ".", subSet, perl = TRUE)
    subSet <- tstrsplit(x = subSet, split = "\\.")
    subSet <- sapply(subSet, function(x){x[!(x ==" ") && !(x =="")]})
    subSet <- sapply(subSet, function(x){x[!is.na(x)]})
    subSet <- subSet[lapply(subSet, length) > 0]
    
    # Remove rest of punctuation (but keep apostrophes)
    subSet <- removePunct(subSet)
    
    # Correct common misspellings or word contractions
    subSet <- stripWhitespace(subSet)
    subSet <- spellChecker(subSet)
    subSet <- stripWhitespace(subSet)
    
    return(subSet)
}

########################################
########################################

removeInternetTerms <- function(subSet)
{
    subSet <- gsub("(\\S*http\\S*)|(\\S*www\\S*)", ".", subSet, perl = TRUE)
    subSet <- gsub("(\\S*@\\S*)", " ", subSet, perl = TRUE)
    subSet <- gsub("RT ", ". ", subSet, perl = TRUE)
    
    return(subSet)
}

########################################
########################################

removeDigits <- function(subSet)
{
    subSet <- gsub(" 1st ", " first ", subSet, perl = TRUE)
    subSet <- gsub(" 2nd ", " second ", subSet, perl = TRUE)
    subSet <- gsub(" 3rd ", " third ", subSet, perl = TRUE)
    subSet <- gsub(" 4th ", " fourth ", subSet, perl = TRUE)
    subSet <- gsub(" 5th ", " fifth ", subSet, perl = TRUE)
    subSet <- gsub(" 6th ", " sixth ", subSet, perl = TRUE)
    subSet <- gsub(" 7th ", " seventh ", subSet, perl = TRUE)
    subSet <- gsub(" 8th ", " eighth ", subSet, perl = TRUE)
    subSet <- gsub(" 9th ", " ninth ", subSet, perl = TRUE)
    subSet <- removeNumbers(subSet)
    
    return(subSet)
}

########################################
########################################

removePunct <- function(subSet)
{
    subSet <- gsub("(^)|($)", ". ", subSet, perl = TRUE)
    subSet <- gsub("(/)|([\\])", " ", subSet)
    subSet <- gsub("(\\.)|(\\,)|(\\;)|(\\{)|(\\})|(\\_)|(\\#)|(\\$)|(\\%)|(\\^)|(\\-)|(\\+)|(\\*)|(\\()|(\\))|(\\[)|(\\])|(\\|)|(\\=)|(\\>)|(\\<)|(\\~)|(\\\")", " ", subSet, perl = TRUE)
    subSet <- gsub("(\\&)", "and", subSet, perl = TRUE)
    subSet <- gsub("(’)|(‘)|(`)|(´)", "'", subSet, perl = TRUE)
}

########################################
########################################

spellChecker <- function(subSet)
{
    #subSet <- gsub("he's ", "he is ", subSet, perl = TRUE) can't differentiate between "he has" and "he is"
    #subSet <- gsub("she's ", "she is ", subSet, perl = TRUE) can't differentiate between "she has" and "she is"
    #subSet <- gsub("he s ", "he is ", subSet, perl = TRUE) can't differentiate between "he has" and "he is"
    #subSet <- gsub("she s ", "she is ", subSet, perl = TRUE) can't differentiate between "she has" and "she is"
    
    subSet <- gsub("( i'm )|( im )|( i m )", " i am ", subSet, perl = TRUE)
    subSet <- gsub("( you're )|( you re )", " you are ", subSet, perl = TRUE)
    subSet <- gsub("( we're )|( we re )", " we are ", subSet, perl = TRUE)
    subSet <- gsub("( they're )|( theyre )|( they re )", " they are ", subSet, perl = TRUE)
    
    subSet <- gsub("( i'll )|( i ll )", " i will ", subSet, perl = TRUE)
    subSet <- gsub("( you'll )|( youll )|( you ll )", " you will ", subSet, perl = TRUE)
    subSet <- gsub("( he'll )|( he ll )", " he will ", subSet, perl = TRUE)
    subSet <- gsub("( she'll )|( she ll )", " she will ", subSet, perl = TRUE)
    subSet <- gsub("( we'll )|( we ll )", " we will ", subSet, perl = TRUE)
    subSet <- gsub("( they'll )|( theyll )|( they ll )", " they will ", subSet, perl = TRUE)
    
    subSet <- gsub("( i've )|( ive )|( i ve )", " i have ", subSet, perl = TRUE)
    subSet <- gsub("( you've )|( youve )|( you ve )", " you have ", subSet, perl = TRUE)
    subSet <- gsub("( we've )|( weve )|( we ve )", " we have ", subSet, perl = TRUE)
    subSet <- gsub("( they've )|( theyve )|( they ve )", " they have ", subSet, perl = TRUE)
    
    subSet <- gsub(" i'd ", " i would ", subSet, perl = TRUE)
    subSet <- gsub("( you'd )|( you d )", " you would ", subSet, perl = TRUE)
    subSet <- gsub("( he'd )|( he d )", " he would ", subSet, perl = TRUE)
    subSet <- gsub("( she'd )|( she d )", " she would ", subSet, perl = TRUE)
    subSet <- gsub("( we'd )|( we d )", " we would ", subSet, perl = TRUE)
    subSet <- gsub("( they'd )|( they d )", " they would ", subSet, perl = TRUE)
    
    subSet <- gsub("( it's )|( it s )", " it is ", subSet, perl = TRUE)
    subSet <- gsub(" it is been ", " it has been ", subSet, perl = TRUE)
    subSet <- gsub("( that's )|( that s )|( thats )", " that is ", subSet, perl = TRUE)
    subSet <- gsub("( there's )|( there s )|( theres )", " there is ", subSet, perl = TRUE)
    
    subSet <- gsub("( what's )|( what s )|( whats )", " what is ", subSet, perl = TRUE)
    
    subSet <- gsub("( let s )|( lets )", " let's ", subSet, perl = TRUE)
    
    subSet <- gsub("( could not )|( couldn t )|( couldnt )",  " couldn't ", subSet, perl = TRUE)
    subSet <- gsub("( would not )|( wouldn t )|( wouldnt )", " wouldn't ", subSet, perl = TRUE)
    
    subSet <- gsub("( have not )|( haven t )|( havent )", " haven't ", subSet, perl = TRUE)
    subSet <- gsub("( is not )|( isn t )|( isnt )", " isn't ", subSet, perl = TRUE)
    subSet <- gsub("( are not )|( aren t )|( arent )", " aren't ", subSet, perl = TRUE)
    subSet <- gsub("( was not )|( wasn t )|( wasnt )", " wasn't ", subSet, perl = TRUE)
    subSet <- gsub("( were not )|( weren t )|( werent )", " weren't ", subSet, perl = TRUE)
    subSet <- gsub("( has not )|( hasn t )|( hasnt )", " hasn't ", subSet, perl = TRUE)
    subSet <- gsub("( had not )|( hadn t )|( hadnt )", " hadn't ", subSet, perl = TRUE)
    subSet <- gsub("( aint )|( ain t )", " ain't ", subSet, perl = TRUE)
    
    subSet <- gsub("( do not )|( don t )|( dont )|( dnt )", " don't ", subSet, perl = TRUE)
    subSet <- gsub("( does not )|( doesn t )|( doesnt )", " doesn't ", subSet, perl = TRUE)
    subSet <- gsub("( did not )|( didn t )|( didnt )", " didn't ", subSet, perl = TRUE)
    subSet <- gsub("( will not )|( won t )|( wont )", " won't ", subSet, perl = TRUE)
    
    subSet <- gsub("( cannot )|( can t )|( cant )", " can't ", subSet, perl = TRUE)
    
    subSet <- gsub(" coulda ", " could have ", subSet, perl = TRUE)
    subSet <- gsub(" woulda ", " would have ", subSet, perl = TRUE)
    subSet <- gsub(" ( gonna )|( gunna ) ", " going to ", subSet, perl = TRUE)
    
    subSet <- gsub(" ur ", " your ", subSet, perl = TRUE)
    subSet <- gsub(" ya ", " you ", subSet, perl = TRUE)
    subSet <- gsub(" yea ", " yeah ", subSet, perl = TRUE)
    subSet <- gsub(" cuz ", " because ", subSet, perl = TRUE)
    subSet <- gsub(" tho ", " though ", subSet, perl = TRUE)
    subSet <- gsub(" idk ", " i don't know ", subSet, perl = TRUE)
    subSet <- gsub(" smh ", " shake my head ", subSet, perl = TRUE)
    subSet <- gsub(" yall ", " you all ", subSet, perl = TRUE)
    subSet <- gsub(" bday ", " birthday ", subSet, perl = TRUE)
    subSet <- gsub(" ( tks )|( thx ) ", " thanks ", subSet, perl = TRUE)
    subSet <- gsub(" imma ", " i'm going to ", subSet, perl = TRUE)
    subSet <- gsub(" kno ", " know ", subSet, perl = TRUE)
    subSet <- gsub(" tonite ", " tonight ", subSet, perl = TRUE)
    subSet <- gsub(" tht ", " that ", subSet, perl = TRUE)
    subSet <- gsub(" prob ", " probably ", subSet, perl = TRUE)
    subSet <- gsub(" xmas ", " christmas ", subSet, perl = TRUE)
    subSet <- gsub(" realise ", " realize ", subSet, perl = TRUE)
    subSet <- gsub(" realised ", " realized ", subSet, perl = TRUE)
    subSet <- gsub(" nothin ", " nothing ", subSet, perl = TRUE)
    subSet <- gsub(" convo ", " conversation ", subSet, perl = TRUE)
    subSet <- gsub(" w o ", " without ", subSet, perl = TRUE)
    subSet <- gsub("( pls )|( plz )|( pleaze )", " please ", subSet, perl = TRUE)
    subSet <- gsub(" fb ", " facebook ", subSet, perl = TRUE)
    subSet <- gsub("'em ", " them ", subSet, perl = TRUE)
    subSet <- gsub(" thru ", " through ", subSet, perl = TRUE)
    subSet <- gsub(" r u ", " are you ", subSet, perl = TRUE)
    subSet <- gsub(" u r ", " you are ", subSet, perl = TRUE)
    
    subSet <- gsub(" a m ", " am ", subSet, perl = TRUE)
    subSet <- gsub(" p m ", " pm ", subSet, perl = TRUE)
    
    subSet <- gsub(" tryin ", " trying ", subSet, perl = TRUE)
    subSet <- gsub(" playin ", " playing ", subSet, perl = TRUE)
    subSet <- gsub(" cryin ", " crying ", subSet, perl = TRUE)
    subSet <- gsub(" comin ", " coming ", subSet, perl = TRUE)
    
    subSet <- gsub(" rite ", " right ", subSet, perl = TRUE)
    subSet <- gsub(" nite ", " night ", subSet, perl = TRUE)
    
    subSet <- gsub(" color ", " colour ", subSet, perl = TRUE)
    subSet <- gsub(" colored ", " coloured ", subSet, perl = TRUE)
    subSet <- gsub(" favorite ", " favourite ", subSet, perl = TRUE)
    
    
    return(subSet)
}

########################################
########################################

profanityFilter <- function(subSet)
{
    deletedString <- "XXXX"
    
    subSet <- gsub("\\S*fuck\\S*", deletedString, subSet, perl = TRUE)
    subSet <- gsub("\\S*fuk\\S*", deletedString, subSet, perl = TRUE)
    subSet <- gsub("\\S*shit\\S*", deletedString, subSet, perl = TRUE)
    subSet <- gsub("\\S*poop\\S*", deletedString, subSet, perl = TRUE)
    subSet <- gsub("\\S*bitch\\S*", deletedString, subSet, perl = TRUE)
    subSet <- gsub("\\S*cunt\\S*", deletedString, subSet, perl = TRUE)
    subSet <- gsub("\\S*whore\\S*", deletedString, subSet, perl = TRUE)
    subSet <- gsub("\\S*slut\\S*", deletedString, subSet, perl = TRUE)
    subSet <- gsub("\\S*skank\\S*", deletedString, subSet, perl = TRUE)
    subSet <- gsub("\\S*nigg\\S*", deletedString, subSet, perl = TRUE)
    subSet <- gsub("\\S*fag\\S*", deletedString, subSet, perl = TRUE)
    subSet <- gsub("\\S*blowjob\\S*", deletedString, subSet, perl = TRUE)
    subSet <- gsub("\\S*handjob\\S*", deletedString, subSet, perl = TRUE)
    subSet <- gsub("\\S*puss\\S*", deletedString, subSet, perl = TRUE)
    subSet <- gsub("\\S*vagina\\S*", deletedString, subSet, perl = TRUE)
    subSet <- gsub("\\S*tits\\S*", deletedString, subSet, perl = TRUE)
    subSet <- gsub("\\Sass\\S", deletedString, subSet, perl = TRUE)
    subSet <- gsub("\\S*dick\\S*", deletedString, subSet, perl = TRUE)
    subSet <- gsub("\\S*penis\\S*", deletedString, subSet, perl = TRUE)
    subSet <- gsub("\\S*prick\\S*", deletedString, subSet, perl = TRUE)
    subSet <- gsub("\\S*erection\\S*", deletedString, subSet, perl = TRUE)
    subSet <- gsub("\\S*fellat\\S*", deletedString, subSet, perl = TRUE)
    subSet <- gsub("\\S*dammit\\S*", deletedString, subSet, perl = TRUE)
    subSet <- gsub("\\S*damnit\\S*", deletedString, subSet, perl = TRUE)
    subSet <- gsub("\\S*bastard\\S*", deletedString, subSet, perl = TRUE)
    subSet <- gsub("\\S*retard\\S*", deletedString, subSet, perl = TRUE)
    subSet <- gsub("\\S*moron\\S*", deletedString, subSet, perl = TRUE)
    subSet <- gsub("\\S*scum\\S*", deletedString, subSet, perl = TRUE)
    subSet <- gsub("\\S*wank\\S*", deletedString, subSet, perl = TRUE)
    subSet <- gsub("ass", deletedString, subSet, perl = TRUE)
    
    return(subSet)
}

########################################
########################################

getCode <- function(encoder, iWord)
{
    return(which.max(iWord == encoder$word))
}

########################################
########################################

getWord <- function(encoder, iCode)
{
    return(encoder[iCode, ]$word)
}
