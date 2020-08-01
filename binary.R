library(caret)

generate <- function(data, size) {
  counter <- nrow(data)
  result <- data
  while (counter < size) {
    if ((size - counter) <  nrow(data)) {
      rows <- sample(nrow(data), size - counter)
      result <- rbind(result, data[rows,])
      counter <- size
    } else {
      result <- rbind(result, data)
      counter <- counter + nrow(data)
    }
  }
  return (result)
}

balance <- function(data, balance, fico) {
  set.seed(42)
  
  # Get records for each category
  Cat_1 <- subset(data, Default_45 == TRUE)
  Cat_2 <- subset(data, Default_45 == FALSE)
  if (fico == TRUE) {
    Cat_1 <- subset(data, Default_45 == TRUE & Has_FICO == TRUE)
    Cat_2 <- subset(data, Default_45 == FALSE & Has_FICO == TRUE)
  }
  
  # Adjust data sets with over or under
  data_set <- data.frame()
  if (balance == FALSE) {
    rows <- min(nrow(Cat_1), nrow(Cat_2))
    data_set <- Cat_1[sample(rows),]
    data_set <- rbind(data_set, Cat_2[sample(rows),])
  } else {
    size <- max(nrow(Cat_1), nrow(Cat_2))
    data_set <- generate(Cat_1, size)
    data_set <- rbind(data_set, generate(Cat_2, size))
  }
  
  # Randomize data sets
  data_set <- data_set[sample(nrow(data_set)),]
  
  # Return final data set
  return(data_set)
}

split <- function(data, ratio) {
  Cat_1 <- subset(data, Default_45 == TRUE)
  Cat_2 <- subset(data, Default_45 == FALSE)
  
  separation <- sort(sample(nrow(Cat_1), nrow(Cat_1)*ratio))
  
  train <- Cat_1[separation,]
  test <- Cat_1[-separation,]
  
  train <- rbind(train, Cat_2[separation,])
  test <- rbind(test, Cat_2[-separation,])
  
  train <- train[sample(nrow(train)),]
  test <- test[sample(nrow(test)),]
  
  return(list(train = train, test = test))  
}

logistic_regression <- function(data, form) {
  train <- data$train
  test <- data$test
  
  m <- glm(formula = form, data = train, family = "binomial")
  result <- predict(m, newdata = test, type = "response")
  
  preds <- rep(FALSE,length(test$Default_45))
  preds[result >= .5] = TRUE
  
  print(m)
  print(caret::confusionMatrix(data = as.factor(preds), reference = as.factor(test$Default_45)))
}


binary <- function(data, sampling) {
  # Original data set
  m0 <- Default_45 ~ V1 + V5 + V6 + Business_channel + Age
  m1 <- Default_45 ~ V1 + V5 + V6 + FICO_Score + Business_channel + Age
  m2 <- Default_45 ~ V1 + V5 + V6 + FICO_Score + Business_channel + Age + V2
  m3 <- Default_45 ~ V1 + V5 + V6 + FICO_Score + Business_channel + Age + V4
  m4 <- Default_45 ~ V1 + V5 + V6 + FICO_Score + Business_channel + Age + V7
  m5 <- Default_45 ~ V1 + V5 + V6 + FICO_Score + Business_channel + Age + V8
  m6 <- Default_45 ~ V1 + V5 + V6 + FICO_Score + Business_channel + Age + V2 + V4
  m7 <- Default_45 ~ V1 + V5 + V6 + FICO_Score + Business_channel + Age + V2 + V7
  m8 <- Default_45 ~ V1 + V5 + V6 + FICO_Score + Business_channel + Age + V2 + V8
  m9 <- Default_45 ~ V1 + V5 + V6 + FICO_Score + Business_channel + Age + V4 + V7
  m10 <- Default_45 ~ V1 + V5 + V6 + FICO_Score + Business_channel + Age + V4 + V8
  m11 <- Default_45 ~ V1 + V5 + V6 + FICO_Score + Business_channel + Age + V7 + V8
  m12 <- Default_45 ~ V1 + V5 + V6 + FICO_Score + Business_channel + Age + V2 + V4 + V7
  m13 <- Default_45 ~ V1 + V5 + V6 + FICO_Score + Business_channel + Age + V2 + V4 + V8
  m14 <- Default_45 ~ V1 + V5 + V6 + FICO_Score + Business_channel + Age + V4 + V7 + V8
  m15 <- Default_45 ~ V1 + V5 + V6 + FICO_Score + Business_channel + Age + V2 + V4 + V7 + V8
  
  
  # Save data set for re-use
  save_data <- data
  
  # Data set balance
  if (sampling == 1) { data <- balance(data, FALSE, TRUE) } 
  else { data <- balance(data, TRUE, TRUE) }
  
  # Split data set
  data <- split(data, .8)
  
  logistic_regression(data = data, form = m1)
  logistic_regression(data = data, form = m2)
  logistic_regression(data = data, form = m3)
  logistic_regression(data = data, form = m4)
  logistic_regression(data = data, form = m5)
  logistic_regression(data = data, form = m6)
  logistic_regression(data = data, form = m7)
  logistic_regression(data = data, form = m8)
  logistic_regression(data = data, form = m9)
  logistic_regression(data = data, form = m10)
  logistic_regression(data = data, form = m11)
  logistic_regression(data = data, form = m12)
  logistic_regression(data = data, form = m13)
  logistic_regression(data = data, form = m14)
  logistic_regression(data = data, form = m15)
  
  # Data set balance
  if (sampling == 1) { data <- balance(save_data, FALSE, FALSE) } 
  else { data <- balance(save_data, TRUE, FALSE) }
  
  # Split data set
  data <- split(data, .8)
  
  logistic_regression(data = data, form = m0)
  
}