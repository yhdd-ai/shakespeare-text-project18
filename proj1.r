next.word <- function(key, M, M1 , w=rep(1, ncol(M)-1)) {
  # key: 当前词序列 (tokens, 可能比 mlag 短)
  # M: (n-mlag) × (mlag+1) 矩阵
  # M1: 整个文本的 token 向量
  # w: mixture 权重
  # output: 下一个词的 token
  
  # 若 key 长于 mlag，取最后的 mlag 个
  if (length(key) > mlag) key <- tail(key, mlag)
  
  candidates <- c() #存所有的候选token
  probs <- c()  #与 candidates 一一对应的概率块（还未合并重复）
  
  key_len <- length(key)
  # 从最长的 key 开始，逐步降低阶数
  for (i in seq_len(key_len)) {
    subkey <- tail(key, i)
    mc <- mlag - i + 1
    
    ii <- colSums(!(t(M[, mc:mlag, drop=FALSE]) == subkey))
    match_rows <- which(ii == 0 & is.finite(ii)) #匹配的行是ii == 0
    
    if (length(match_rows) > 0) {
      u <- M[match_rows, mlag+1]
      u <- u[!is.na(u)]  #丢弃 NA
      if (length(u) > 0) {
        probs <- c(probs, rep(w[i] / length(u), length(u)))
        candidates <- c(candidates, u)
      }
    }
  }
  
  #如果没有任何候选，从全文随机选一个常见词
  if (length(candidates) == 0) {
    return(sample(M1[!is.na(M1)], 1))
  }
  
  # 合并相同 token 的概率
  prob_table <- tapply(probs, candidates, sum)
  
  # 删除NA值
  prob_table <- prob_table[!is.na(prob_table)]
  if (length(prob_table) == 0) {
    valid_tokens <- M1[!is.na(M1)]
    return(sample(valid_tokens, 1))
  }
  
  #确保概率为1
  prob_table <- prob_table / sum(prob_table)
  
  #抽样
  next_token <- sample(names(prob_table), 1, prob = prob_table)
  return(as.numeric(next_token))
}

# Step 6: Sentence Generation
simulate_sentence <- function(M, M1, b, start_word=NULL, mlag=ncol(M) - 1) {
  # M: Markov 矩阵
  # M1: 整个文本的 token 序列
  # b: 常用词表（token -> word 的映射）
  # start_word: 可选的起始词（string）；若 NULL 则随机挑选一个
  # 返回: 生成的一句话（string）
  
  # 选择起始token
  if (is.null(start_word)) {
    valid_tokens <- M1[!is.na(M1)]
    if (length(valid_tokens) == 0) {
      stop("没有有效的token可作为起始词")
    }
    start_token <- sample(valid_tokens, 1)
  } else {
    start_token <- match(start_word, b)
    if (is.na(start_token)) {
      stop("起始词不在词汇表中")
    }
  }
  
  # 初始化序列
  sentence_tokens <- c(start_token)
  
  # 不断预测下一个 token，直到遇到句号 "."
  repeat {
    # 提取当前 key（最多 mlag 个 token）
    key <- tail(sentence_tokens, mlag)
    
    # 预测下一个 token
    next_token <- next.word(key, M, M1)
    sentence_tokens <- c(sentence_tokens, next_token)
    
    # 如果对应词是句号，则结束
    if (b[next_token] == ".")  break
    
    # 防止死循环（设定句子最大长度）
    if (length(sentence_tokens) > 50)  break 
  }
  
  # 将token转换回单词
  words <- b[sentence_tokens]
  
  # 拼接成句子
  sentence <- paste(words, collapse=" ")
  sentence <- gsub(" ([,.;:!?])", "\\1", sentence)  # 去掉标点前多余的空格
  sentence <- trimws(sentence)   #去除字符串开头和结尾的空格
  return(sentence)
}