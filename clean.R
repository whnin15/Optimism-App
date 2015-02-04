### to clean up the data for both datasets

# sourcing func.R
source("func.R")

#--------------------------------------------------------------------------------

  # loading the weight score values
weights <- read.csv("data/Weights for Question.csv", header = FALSE)

  # changing the class of the first column as character
weights[,1] <- as.character(weights[,1])

  # removing the age weight score (since we decided not to use the desired age column)
weights <- weights[-15,]

  # changing column names
names(weights) <- c("q_number", "weight")

#--------------------------------------------------------------------------------

# load the data frame with required columns from Pew Research center dataset
load("data/fixed_pew.rdata")

# summary:
# answer range : -1 -> 1
# Neutral answers are given 0 and no responses are set as NA
# calculate optimism score
# save the ready-to-use file as cleaned_pew.rdata


# changing the column name of the region column
# so it has the same column name as the gallup dataset
colnames(original_table)[colnames(original_table)=="opt.cregion"] <- "region"

# changing  no responses/"Don''t know" answers to NA
for (x in 1:length(original_table)) {
  original_table[grep("Don't know", original_table[, x]), x] <- NA
}

# changing answer 998 in opt.q14 column (answer = don't know/refused) to NA
  original_table[grep(998, original_table[, "opt.q14"]), "opt.q14"] <- NA

# changing "Doesn't apply" answers to NA 
for (x in 1:length(original_table)){
  original_table[grep("Doesn't apply", original_table[, x]), x] <- NA
}


  ## changing the column values into numbers
  # changing columns with two different values
  # q1 - change to (-1, 1)
original_table$opt.q1 <- ifelse(original_table$opt.q1 == "Satisfied", 1, -1)
  # q5 - change to (-1, 1)
original_table$opt.q5 <- ifelse(original_table$opt.q5 == "Satisfied", 1, -1)
  # q43 - change to (-1, 1)
original_table$opt.q43 <- ifelse(original_table$opt.q43 == "A good thing", 1, -1)
  # q45 - change to (-1, 1)
original_table$opt.q45 <- ifelse(original_table$opt.q45 == "Yes, would want", 1, -1)


 # changing columns with three different values
  #q2 - change to (-1, 0, 1)
  # changing the class of the column from factor to character
original_table$opt.q2 <- as.character(original_table$opt.q2)

original_table$opt.q2[which(original_table$opt.q2 == "Will be better")] <- 1
original_table$opt.q2[which(original_table$opt.q2 == "About the same")] <- 0
original_table$opt.q2[which(original_table$opt.q2 == "Will be worse")] <- -1

  #q3 - change to (-1, 0, 1)
  # changing the class of the column from factor to character
original_table$opt.q3 <- as.character(original_table$opt.q3)

original_table$opt.q3[which(original_table$opt.q3 == "Better today")] <- 1
original_table$opt.q3[which(original_table$opt.q3 == "About the same")] <- 0
original_table$opt.q3[which(original_table$opt.q3 == "Worse today")] <- -1

# changes (-1, 0, 1) to q22d
for (i in 50) {
  #changing the class of the column from factor to character
  original_table[,i] <- as.character(original_table[,i])
  
  original_table[which(original_table[,i] == "(One) Medical advances that prolong life are generally GOOD because they allow people to live longer [OR]"), i] <- 1
  
  original_table[which(original_table[,i] == "(Two) These advances are BAD because they interfere with the natural cycle of life."), i] <- -1
  
  original_table[which(original_table[,i] == "[VOL. DO NOT READ] Neither/Both equally"), i] <- -0
}

# changing question 22c into (-1, 0, 1)
for (i in 49) {
  #changing the class of the column into 
  original_table[,i] <- as.character(original_table[,i])
  
  original_table[which(original_table[,i] == "(One) The growing world population will NOT be a major problem because we will find a way to stretch our natural..."), i] <- 1
  
  original_table[which(original_table[,i] == "(Two) The growing population WILL be a major problem because there won't be enough food and resources to go around"), i] <- -1
  
  original_table[which(original_table[,i] == "[VOL. DO NOT READ]  Neither/Both equally"), i] <- 0
}

# changing question 21 (a,b,c,e) to (-1, -0.5, 0.5, 1)
for (i in 45:48) {
  #changing the class of the column into 
  original_table[,i] <- as.character(original_table[,i])
  
  original_table[which(original_table[,i] == "Will probably NOT happen"), i] <- -0.5
  
  original_table[which(original_table[,i] == "Will probably happen"), i] <- 0.5
  
  original_table[which(original_table[,i] == "Will definitely not happen"), i] <- -1
  
  original_table[which(original_table[,i] == "Will definitely happen"), i] <- 1
}

# changing question q18h to (-0.5, 0, 0.5, 1)
for (i in 44) {
  original_table[,i] <- as.character(original_table[,i])
  
  original_table[which(original_table[,i] == "Extremely important"), i] <- 1
  
  original_table[which(original_table[,i] == "Somewhat important"), i] <- -0.5
  
  original_table[which(original_table[,i] == "Very important"), i] <- 0.5
  
  original_table[which(original_table[,i] == "Not important"), i] <- 0
}

# changing the question q8 to (-1, 0, 1)
for (i in 40) {
  original_table[,i] <- as.character(original_table[,i])
  
  original_table[which(original_table[,i] == "(One) Medical treatments these days are worth the costs because they allow people to live longer and better quality live"), i] <- 1
  
  original_table[which(original_table[,i] == "(Two) Medical treatments these days often create as many problems as they solve."), i] <- -1
  
  original_table[which(original_table[,i] == "[VOL. DO NOT READ] Neither/Both equally"), i] <- 0
}


# changing columns with four different values
  # q4a-g to (-1, -.5, 0.5, 1, NA)
for (i in c(19, 22, 29:33)) {
  # changing the class of the column from factor to character
  original_table[,i] <- as.character(original_table[,i])
  
  original_table[which(original_table[,i] == "Excellent"), i] <- 1
  original_table[which(original_table[,i] == "Good"), i] <- 0.5
  original_table[which(original_table[,i] == "Only fair"), i] <- -0.5  
  original_table[which(original_table[,i] == "Poor"), i] <- -1  
  original_table[which(original_table[,i] == "[VOL. DO NOT READ]  Doesn't apply"), i] <- NA
}

 # q4h to (-1, -.5, 0.5, 1)
  # changing the class of the column from factor to character
original_table$opt.q4h <- as.character(original_table$opt.q4h)

original_table$opt.q4h[which(original_table$opt.q4h == "A lot")] <- -1
original_table$opt.q4h[which(original_table$opt.q4h == "A little")] <- -0.5
original_table$opt.q4h[which(original_table$opt.q4h == "Not too much")] <- 0.5
original_table$opt.q4h[which(original_table$opt.q4h == "Not at all")] <- 1

 # q9 to (-1, -.5, 0.5, 1)
  # changing the class of the column from factor to character
original_table$opt.q9 <- as.character(original_table$opt.q9)

original_table$opt.q9[which(original_table$opt.q9 == "A lot")] <- 1
original_table$opt.q9[which(original_table$opt.q9 == "Some")] <- 0.5
original_table$opt.q9[which(original_table$opt.q9 == "Not too much")] <- -0.5
original_table$opt.q9[which(original_table$opt.q9 == "None at all")] <- -1

 # q10 to (-1, -.5, 0.5, 1)
  # changing the class of the column from factor to character
original_table$opt.q10 <- as.character(original_table$opt.q10)

original_table$opt.q10[which(original_table$opt.q10 == "A lot")] <- 1
original_table$opt.q10[which(original_table$opt.q10 == "Some")] <- 0.5
original_table$opt.q10[which(original_table$opt.q10 == "Not too much")] <- -0.5
original_table$opt.q10[which(original_table$opt.q10 == "Not at all")] <- -1

 # opt.relig - shorten the religion's names
  #change the column from class factor to character
original_table$opt.relig <- as.character(original_table$opt.relig)

  # remove the religion explanation in parantheses
original_table$opt.relig[grep("Agnostic", original_table[, "opt.relig"])] <- "Agnostic"
original_table$opt.relig[grep("Christian", original_table[, "opt.relig"])] <- "Christian"
original_table$opt.relig[grep("Protestant", original_table[, "opt.relig"])] <- "Protestant"
original_table$opt.relig[grep("Roman Catholic", original_table[, "opt.relig"])] <- "Catholic"
original_table$opt.relig[grep("Atheist", original_table[, "opt.relig"])] <- "Atheist"
original_table$opt.relig[grep("Mormon", original_table[, "opt.relig"])] <- "Mormon"
original_table$opt.relig[grep("Jewish", original_table[, "opt.relig"])] <- "Jewish"
original_table$opt.relig[grep("Unitarian", original_table[, "opt.relig"])] <- "Unitarian"
original_table$opt.relig[grep("Orthodox", original_table[, "opt.relig"])] <- "Orthodox"
original_table$opt.relig[grep("Muslim", original_table[, "opt.relig"])] <- "Muslim"
original_table$opt.relig[grep("Something", original_table[, "opt.relig"])] <- "Something Else"
original_table$opt.relig[grep("Don't Know", original_table[, "opt.relig"])] <- NA

# calculating optimism score for every participant in the survey 
# saving in a new column called optimism_score
original_table$optimism_score <- sapply(1:dim(original_table)[1], calc_score)

save(original_table, file = "data/cleaned_pew.rdata")
#--------------------------------------------------------------------------


# load the data frame for gallup dataset
load("data/fixed_gallup.rdata")

# summary:
# adding a new column, region to be consistent with the Pew dataset
# saving the ready-for-analysis file as cleaned_gallup.rdata


## adding a column that represents the region of the states

# assigning the states to each region variable as from Wikipedia
Northeast <- c("Connecticut", "Maine", "Massachusetts", "New Hampshire", 
          "Rhode Island", "Vermont", "New Jersey", "New York", "Pennsylvania")

Midwest <- c("Illinois", "Indiana", "Michigan", "Ohio", "Wisconsin", "Iowa",
          "Kansas", "Minnesota", "Missouri", "Nebraska", "North Dakota", 
          "South Dakota")

South <- c("Delaware", "Florida", "Georgia", "Maryland", "North Carolina",
         "South Carolina", "Virginia", "District of Columbia", "West Virginia",
         "Alabama", "Kentucky", "Mississippi", "Tennessee", "Arkansas", 
         "Louisiana", "Oklahoma", "Texas")

West <- c("Arizona", "Colorado", "Idaho", "Montana", "Nevada", "New Mexico",
       "Utah", "Wyoming", "Alaska", "California", "Hawaii", "Oregon", "Washington")

# creating a new column called region
gallup$region <- character(dim(gallup)[1])

# adding the region Northeast for the states in Northeast
for (i in Northeast) {
  gallup$region[which(gallup$State == i)] <- "Northeast"
}

# adding the region Midwest for the states in Northeast
for (i in Midwest) {
  gallup$region[which(gallup$State == i)] <- "Midwest"
}

# adding the region South for the states in Northeast
for (i in South) {
  gallup$region[which(gallup$State == i)] <- "South"
}

# adding the region West for the states in Northeast
for (i in West) {
  gallup$region[which(gallup$State == i)] <- "West"
}

save(gallup, file = "data/cleaned_gallup.rdata")

