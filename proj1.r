# Member1：Chunxi Su（UNN：s2814164）；Member2：Huaidong Yue（UNN：s2815318）；Yifei Peng：（UNN：s2792136）
# Contribution Instruction：

## Project Instruction：Member 1 (33%): Text Preparation and Preprocessing Module；Member 2 (34%): Word Frequency Statistics and Sequence Matrix Construction Module；Member 3 (33%): Core function development and sentence generation module

#setwd("/Users/clggmac/Rprogramming/Shakespeare-project/shakespeare-text-project18")  ## The setwd line should be commented out of the code you finally submit for marking！！！
# reading text：skip first 83 rows（invalid instruction）,reading 196043-83 row（valid text）,coding with UTF-8
a <- scan("shakespeare.txt", 
          what = "character",  # Read as character type (split by word)
          skip = 83, 
          nlines = 196043 - 83, 
          fileEncoding = "UTF-8")

# Verify the read result
# head(a)  # View the first 6 words
# length(a)  # View total words（about 195960）

## Vocabulary Construction and Word Frequency Statistics
vocabulary_constr_and_freq <- function(pre_words){
  # Convert to lowercase
  pre_words <- tolower(pre_words)
  
  # Retrieve all unique words in the text
  unique_words <- unique(pre_words)
  
  # Construct pre_words mapping vector from “word → unique word index”
  word_indices <- match(pre_words, unique_words)
  
  # Count the number of occurrences for each unique word.
  word_counts <- tabulate(word_indices)
  names(word_counts) <- unique_words

  return(word_counts)
}

word_counts <- vocabulary_constr_and_freq(a)

mlag <- 4
token_vec <- c()
## Common Word Marker Sequence Matrix Construction
matrix_construction <- function(word_counts){
  # Filter the 1000 most common words）
  k <- 1000
  sorted_words <- names(sort(word_counts, decreasing = TRUE))
  b <- sorted_words[1:min(k, length(sorted_words))]
  
  # Generate token vectors for the entire text (mapped to indices of common words in b; words not in b are NA).
  token_vec <- match(a, b)
  
  n <- length(token_vec)
  if (n <= mlag) stop("The text length is insufficient to construct matrix M.")
  
  # Initialize M: Each column is a right shift of token_vec by 0, 1, ..., mlg bits, removing elements beyond the range.
  M <- matrix(NA, nrow = n - mlag, ncol = mlag + 1)
  for (col in 1:(mlag + 1)) {
    # The colth column = token_vec offset(col-1) bits, taking the first (n-mlag) elements
    M[, col] <- token_vec[(col):(col + n - mlag - 1)]
  }
  
  return(M)
}

M <- matrix_construction(word_counts)