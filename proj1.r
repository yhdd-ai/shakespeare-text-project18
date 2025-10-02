# Member1：Chunxi Su（UNN：s2814164）；Member2：Huaidong Yue（UNN：s2815318）；Member3：Yifei Peng（UNN：s2792136）
# Contribution Instruction：

## Project Instruction：Member 1 (33%): Text Preparation and Preprocessing Module；Member 2 (34%): Word Frequency Statistics and Sequence Matrix Construction Module；Member 3 (33%): Core function development and sentence generation module

#setwd("/Users/clggmac/Rprogramming/Shakespeare-project/shakespeare-text-project18")  ## The setwd line should be commented out of the code you finally submit for marking！！！
# setwd("D:/scx Work/programming/R/Rprogramming/shakespeare-text-project18")  ## The setwd line should be commented out of the code you finally submit for marking！！！
# reading text：skip first 83 rows（invalid instruction）,reading 196043-83 row（valid text）,coding with UTF-8
a <- scan("shakespeare.txt",
          what = "character",  # Read as character type (split by word)
          skip = 83,
          nlines = 196043 - 83,
          fileEncoding = "UTF-8")

## ------------------------------
## Member1: Text Preprocessing Module (Chunxi Su)
## ------------------------------

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
  
  sp_a <- vector("list", length(a))
  
  punct_pat <- paste(punct, collapse="|")
  
  for(i in 1:length(a)){
    w <- a[i]
    
    if(length(grep(punct_pat, w)) == 0){
      sp_a[[i]] <- w # No punctuation mark
    }
    else{
      
      w_2 <- w
      
      for(p in punct){
        if(length(grep(p, w_2)) > 0){
          w_2 <- gsub(p, "", w_2)
          sp_a[[i]] <- c(w_2, gsub("\\\\", "", p))
          w_2 <- ""
          break
        }
      }
      
      if(w_2 != ""){
        sp_a[[i]] <- w_2 # Prevent unrecognized punctuation marks
      }
    }
  }
  
  return(unlist(sp_a))
  
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
  
  # Remove stage directions (in [])
  a_bracket_le <- grep("\\[", a)
  a_bracket_ri_all <- grep("\\]", a)
  
  a_remove <- rep(TRUE, length(a)) # Mark parts to keep (TRUE = keep, FALSE = remove)
  
  for(i in a_bracket_le){
    # Find the first ")" after "[" (within 100 steps to avoid over-removal)
    a_bracket_ri <- a_bracket_ri_all[a_bracket_ri_all > i & a_bracket_ri_all <= i + 100]
    
    if(length(a_bracket_ri) == 0){
      j <- i # Single "[" case: only remove the "["
    }
    else{
      j <- a_bracket_ri[1]
    }
    
    a_remove[i:j] <- FALSE # Mark content in [] as to remove
  }
  
  a <- a[a_remove]
  
  # Remove character names/headings/numbers
  # Rule: Remove words that are all uppercase (except "I" and "A") + pure numbers
  a_remove <- a == toupper(a)
  a_remove[grep("^[0-9]+$", a)] <- TRUE
  a_remove[grep("^(I|A)$", a)] <- FALSE
  a <- a[!a_remove]
  
  # Remove “_” and "-" (split words if needed, then clean)
  a <- unlist(strsplit(a, "_"))
  a <- gsub("_", "", a)
  a <- unlist(strsplit(a, "-"))
  a <- gsub("-", "", a)
  
  # Separate punctuation marks
  a <- split_punct(a)
  
  # Convert to lowercase
  a <- tolower(a)
  
  return(a)
}

# Execute text preprocessing (output cleaned text vector 'a')
a <- clean_text(a)


## ------------------------------
## Member2: Word Frequency & Sequence Matrix Module (Huaidong Yue)
## ------------------------------

##
# function:vocabulary_constr_and_freq()
# author:Huaidong Yue
# date:25/09/2025
# purpose:
#   1.Vocabulary Construction
#   2.Word Frequency Statistics
# 
# input:
#   pre_words-vector of split, cleaned character (output of clean_text)
# 
# output: 
#   word_counts-counts of each unique word (named numeric vector)
##
vocabulary_constr_and_freq <- function(pre_words){
  # Convert to lowercase (redundant here, but kept for robustness)
  pre_words <- tolower(pre_words)
  
  # Retrieve all unique words in the cleaned text
  unique_words <- unique(pre_words)
  
  # Map words to their indices in the unique vocabulary
  word_indices <- match(pre_words, unique_words)
  
  # Count occurrences of each unique word
  word_counts <- tabulate(word_indices)
  names(word_counts) <- unique_words
  
  return(word_counts)
}

# Execute word frequency statistics (input: cleaned 'a'; output: word_counts)
word_counts <- vocabulary_constr_and_freq(a)

# Set lag order for sequence matrix (hyperparameter)
mlag <- 4
token_vec <- 

##
# function:matrix_construction()
# author:Huaidong Yue
# date:25/09/2025
# purpose:
#   Common Word Marker Sequence Matrix Construction
# 
# input:
#   word_counts-counts of each unique word (output of vocabulary_constr_and_freq)
#   mlag-lag order (global variable, or can add as function parameter)
# 
# output: 
#   M-sequence matrix (rows: windows; columns: lagged tokens)
##
matrix_construction <- function(word_counts, mlag){
  # Filter top 1000 most common words
  k <- 1000
  sorted_words <- names(sort(word_counts, decreasing = TRUE))
  common_words <- sorted_words[1:min(k, length(sorted_words))]
  
  # Generate token vector: map cleaned words to indices of common words (NA if not in common_words)
  token_vec <- match(a, common_words)

  # Check if text length is sufficient for matrix construction
  n <- length(token_vec)
  if (n <= mlag) stop("The text length is insufficient to construct matrix M.")
  
  # Initialize sequence matrix M
  # Rows: n - mlag (number of windows); Columns: mlag + 1 (lag 0 to lag mlag)
  M <- matrix(NA, nrow = n - mlag, ncol = mlag + 1)
  
  # Fill each column (righst-shifted token_vec)
  for (col in 1:(mlag + 1)) {
    # Column col = token_vec starting at position col, length = n - mlag
    M[, col] <- token_vec[col : (col + n - mlag - 1)]
  }
  
  return(M)
}

# Execute sequence matrix construction (input: word_counts + mlag; output: M)
M <- matrix_construction(word_counts, mlag)

#predict next word
next.word <- function(key, M, M1 = token_vec, w=rep(1, ncol(M)-1)) {

          
  # If the key is longer than mlag, take the last mlag characters.
  if (length(key) > mlag) key <- tail(key, mlag)
  
  candidates <- c()
  probs <- c()  
  
  key_len <- length(key)
  # Start with the longest key and gradually decrease the order.
  for (i in seq_len(key_len)) {
    subkey <- tail(key, i)
    mc <- mlag - i + 1
    
    ii <- colSums(!(t(M[, mc:mlag, drop=FALSE]) == subkey))
    match_rows <- which(ii == 0 & is.finite(ii)) #The matching line is ii == 0
    
    if (length(match_rows) > 0) {
      u <- M[match_rows, mlag+1]
      u <- u[!is.na(u)]  
      if (length(u) > 0) {
        probs <- c(probs, rep(w[i] / length(u), length(u)))
        candidates <- c(candidates, u)
      }
    }
  }
  
  #If there are no candidates, randomly select a common word from the entire text.
  if (length(candidates) == 0) {
    return(sample(M1[!is.na(M1)], 1))
  }
  
  # Merge the probability of identical tokens
  prob_table <- tapply(probs, candidates, sum)
  
  prob_table <- prob_table[!is.na(prob_table)]
  if (length(prob_table) == 0) {
    valid_tokens <- M1[!is.na(M1)]
    return(sample(valid_tokens, 1))
  }
  
  #Ensure the probability is 1
  prob_table <- prob_table / sum(prob_table)
  
  next_token <- sample(names(prob_table), 1, prob = prob_table)
  return(as.numeric(next_token))
  
}

#Sentence Generation
simulate_sentence <- function(M, M1=token_vec, b, start_word=NULL, mlag=ncol(M) - 1) {

  
 
  # Select the starting token
  if (is.null(start_word)) {
    valid_tokens <- M1[!is.na(M1)]
    if (length(valid_tokens) == 0) {
      stop("No valid token is available as a starting word")
    }
    start_token <- sample(valid_tokens, 1)
  } else {
    start_token <- match(start_word, b)
    if (is.na(start_token)) {
      stop("The starting word is not in the vocabulary.")
    }
  }
  
  # Initialization
  sentence_tokens <- c(start_token)
  
  # Continuously predict the next token until '.' is encountered.
  repeat {
    # Extract the current key (up to mlag tokens)
    key <- tail(sentence_tokens, mlag)
    
    # Predict the next token
    next_token <- next.word(key, M, M1)
    sentence_tokens <- c(sentence_tokens, next_token)
    

    if (b[next_token] == ".")  break
    
    if (length(sentence_tokens) > 50)  break 
  }
  
  # Convert tokens back to word
  words <- b[sentence_tokens]
  
  sentence <- paste(words, collapse=" ")
  sentence <- gsub(" ([,.;:!?])", "\\1", sentence)  
  sentence <- trimws(sentence)   
  return(sentence)
}
