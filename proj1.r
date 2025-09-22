# Member1：Chunxi Su（UNN：s2814164）；Member2：Huaidong Yue（UNN：s2815318）；Yifei Peng：王五（UNN：s2792136）
# Contribution Instruction：

## Project Instruction：Member A (33%): Text Preparation and Preprocessing Module；Member B (34%): Word Frequency Statistics and Sequence Matrix Construction Module；Member C (33%): Core function development and sentence generation module

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
123
 