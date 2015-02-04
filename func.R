  # calculating the optimism score
  # used in clean.R
  # parameter: row - the row number from the original table
  # return: the optimism score for that column

calc_score <- function(row) {
  
  # getting the weights of the questions
  questions <- weights[,1]
  
  # initializing the variables
  rel_weight <- numeric(length(questions)-1)
  scores <- numeric(length(questions)-1)
  sum_weight <- 0

  # getting the answer scores by the respondent
  for (i in 1:(length(questions)-1)) {
    # getting the answer value from the original table
    scores[i] <- as.numeric(original_table[row, questions[i]])
    
    # if the person answered the question, get the weight of the question
    # and add to the sum of the weight of the questions
    # if the person did not answer, assign the relative weight as NA
    if (!is.na(scores[i])) {
      rel_weight[i] <- weights[i, 2]
      sum_weight <- sum_weight + weights[i, 2]
    } else {
      rel_weight[i] <- NA
    }
  }
  
  # getting the relative weight values by dividing the weight of 
  # each question with the sum of the weights
  rel_weight <- rel_weight / sum_weight
  
  # multiplying the answer scores with the corresponding relative weights
  scores <- scores * rel_weight
  
  # summing up all the scores and multiplying the result with 5
  optimism_score <- sum(scores, na.rm=TRUE) * 5
  
  return (optimism_score)
}


  # calculating the proportions of the people who answered x 
  # for optimism score groups, (< -3, -3 => -1.5, -1.5 => 0, 0 => 1.5, 1.5 => 3, >= 3)
  # used in do_effects.R
  # parameters: x - the answer for which the proportion wanted to be calculated
  #             col - the column number or column name in which the answer existed
  # return: return a vector that includes proportions for the six optimism score groups 

views_prop <- function(x, col) {
  
  output <- numeric(6)
  bin <- c(-3, -1.5, 0, 1.5, 3)
  score <- original_table$optimism_score
  
  # calculating the proportion for each optimism score group
  for (i in 1:6) {
    
    # for the first group, get the total number of people who answered x and scored
    # less than -3, and divide by the number of people who scored less than -3
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
