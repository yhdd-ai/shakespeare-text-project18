# Member1：Chunxi Su（UNN：s2814164）；Member2：Huaidong Yue（UNN：s2815318）；Yifei Peng：王五（UNN：s2792136）
# Contribution Instruction：

## Project Instruction：Member A (33%): Text Preparation and Preprocessing Module；Member B (34%): Word Frequency Statistics and Sequence Matrix Construction Module；Member C (33%): Core function development and sentence generation module

# setwd("D:/scx Work/programming/R/Rprogramming/shakespeare-text-project18")  ## The setwd line should be commented out of the code you finally submit for marking！！！
# reading text：skip first 83 rows（invalid instruction）,reading 196043-83 row（valid text）,coding with UTF-8
a <- scan("shakespeare.txt",
          what = "character",  # Read as character type (split by word)
          skip = 83,
          nlines = 196043 - 83,
          fileEncoding = "UTF-8")

# Verify the read result
# head(a)  # View the first 6 words
# length(a)  # View total words（about 195960）



## pre-processing

# # test case
# a_test <- c("CHUNXI", "[", "Aaa", "Bbb", "]", "Su", "[Aa", "Bb]", "[To", "be,", "or", "not", "to", "be.", "I", "A", "123", "Aaa_bbb", "ccc-ddd!")


##
# function:split_punct()
# purpose:
#   Separate punctuation marks from words
#   1.Remove the punctuation mark from the word
#   2.Insert the punctuation mark as a new element after the word
# 
# input:
#   a-a vector of character
#   punct-a vector of punctuation marks(default includes ",", ".", ";", ":", "!", "?", "(", ")", "\"", "'")
# 
# output: sp_a
##

split_punct <- function(a, punct=c(",", "\\.", ";", ":", "!", "\\?", "\\(", "\\)", "\"", "'")){
  
  sp_a <- c()
  
  for(w in a){
    
    punct_true <- grep(paste(punct, collapse="|"), w)
    
    if(length(punct_true) == 0){
      sp_a <- c(sp_a, w) #No punctuation mark
    }
    else{
      
      w_2 <-w
      
      for(p in punct){
        if(length(grep(p, w_2)) != 0){
          w_2 <- gsub(p, "", w_2)
          sp_a <- c(sp_a, w_2, gsub("\\\\", "", p))
          w_2 <- ""
          break 
        }
      }
      
      if(w_2 != ""){
        sp_a <- c(sp_a, w_2) # Prevent the occurrence of unrecognized punctuation marks
      }
    }
  }
  
  return(sp_a)
  
}

##
# function:clean_text()
# purpose:
#   Clean Shakespeare text vector 'a'
#   Remove stage directions (in [])
#   Remove character names/headings/numbers
#   Remove “_” and "-"
#   Use split_punct function to separate the punctuation marks
#   Convert to lowercase
# input:a-a vector of character
# 
# output:a- the cleaned vector a
##

clean_text <- function(a){
  
  #Remove stage directions (in [])
  a_bracket_le <- grep("\\[", a)
  
  a_remove <- rep(TRUE, length(a))# mark the deleted parts
  
  for(i in a_bracket_le){
    
    a_bracket_ri <- grep("\\]", a[i+1:min(length(a),i+100)])
    
    if(length(a_bracket_ri) == 0){
      j <- i
    }
    else{
      j <- a_bracket_ri[1]+i
    }
    
    a_remove[i:j] <- FALSE
  }
  
  a <- a[a_remove]
  
  #Remove character names/headings/numbers
  
  a_remove <- a == toupper(a)
  a_remove[grep("^[0-9]+$", a)] <- TRUE
  a_remove[grep("^(I|A)$", a)] <- FALSE
  
  a <- a[!a_remove]
  
  #Remove “_” and "-"
  a <- unlist(strsplit(a, "_"))
  a <- gsub("_", "", a)
  
  a <- unlist(strsplit(a, "-"))
  a <- gsub("-", "", a)
  
  #Use split_punct function to separate the punctuation marks
  a <- split_punct(a)
  
  #Convert to lowercase
  a <- tolower(a)
  
  return(a)
}

a <- clean_text(a)

# #test
# a_test <-clean_text(a_test)
# a_test