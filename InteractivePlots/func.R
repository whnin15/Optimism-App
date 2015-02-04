# load data from cleaned_pew
load("data/cleaned_pew.rdata")


# calculating the optimism score
calc_score <- function(row) {
  
  questions <- weights[,1]
  rel_weight <- numeric(length(questions)-1)
  scores <- numeric(length(questions)-1)
  sum_weight <- 0

  # getting the answer scores by the respondent
  for (i in 1:(length(questions)-1)) {
    scores[i] <- as.numeric(original_table[row, questions[i]])
    
    # if the person answered the question, get the weight of the question
    # and add to the sum of the weight of the questions
    if (!is.na(scores[i])) {
      rel_weight[i] <- weights[i,2]
      sum_weight <- sum_weight + weights[i, 2]
    } else {
      rel_weight[i] <- NA
    }
  }
  
  # getting the relative weight values by dividing the weight of 
  # each question with the sum of the weights
  rel_weight <- rel_weight / sum_weight
  
  # multiplying the scores with the corresponding relative weights
  scores <- scores * rel_weight
  
  # summing up all the scores
  optimism_score <- sum(scores, na.rm=TRUE) * 5
  
  return (optimism_score)
}

# calculating the proportions of the people who answered q7 (a, b, c, e)
views_prop <- function(x, col) {
  output <- numeric(6)
  bin <- c(-3, -1.5, 0, 1.5, 3)
  score <- original_table$optimism_score
  
  for (i in 1:6) {
    if (i == 1) {
      output[i] <- sum(ifelse((original_table[, col] == x
                  & score < bin[i]), 1, 0), na.rm=TRUE) / sum(score < bin[i])
      
    } else if (i == 6) {
      output[i] <- sum(ifelse((original_table[, col] == x
                  & score >= bin[i-1]), 1, 0), na.rm=TRUE) / sum(score >= bin[i-1])
      
    } else {
      output[i] <- sum(ifelse((original_table[, col] == x
                  & score >= bin[i-1] & score < bin[i]), 1, 0), na.rm=TRUE) /
                  sum(score >= bin[i-1] & score < bin[i])  
    }
    
  }
  return (output)
}
