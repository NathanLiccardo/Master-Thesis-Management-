library(nnet)
library(party)
library(caret)
library(e1071)

multinomial_generate <- function(data, size) {
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
multinomial_balance <- function(data, balance, fico) {
  set.seed(42)
  
  # Separate categories for each status
  Cat_1 <- subset(data, Status == "Bad")
  Cat_2 <- subset(data, Status == "Middle")
  Cat_3 <- subset(data, Status == "Good")
  if (fico == TRUE) {
    Cat_1 <- subset(data, Status == "Bad" & Has_FICO == TRUE)
    Cat_2 <- subset(data, Status == "Middle" & Has_FICO == TRUE)
    Cat_3 <- subset(data, Status == "Good" & Has_FICO == TRUE)
  }
  
  # Under or over sampling (with random over)
  data_set <- data.frame()
  if (balance == FALSE) {
    rows <- min(nrow(Cat_1), nrow(Cat_2), nrow(Cat_3))
    data_set <- rbind(data_set, Cat_1[sample(rows),])
    data_set <- rbind(data_set, Cat_2[sample(rows),])
    data_set <- rbind(data_set, Cat_3[sample(rows),])
  } else {
    rows <- max(nrow(Cat_1), nrow(Cat_2), nrow(Cat_3))
    data_set <- rbind(data_set, multinomial_generate(Cat_1, rows))
    data_set <- rbind(data_set, multinomial_generate(Cat_2, rows))
    data_set <- rbind(data_set, multinomial_generate(Cat_3, rows))
  }
  
  # Randomize data set
  data_set <- data_set[sample(nrow(data_set)),]
  
  # Return final data set
  return(data_set)
}

multinomial_split <- function(data, ratio) {
  Cat_1 <- subset(data, Status == "Bad")
  Cat_2 <- subset(data, Status == "Middle")
  Cat_3 <- subset(data, Status == "Good")
  
  separation <- sort(sample(nrow(Cat_1), nrow(Cat_1)*ratio))
  
  train <- Cat_1[separation,]
  test <- Cat_1[-separation,]
  train <- rbind(train, Cat_2[separation,])
  test <- rbind(test, Cat_2[-separation,])
  train <- rbind(train, Cat_3[separation,])
  test <- rbind(test, Cat_3[-separation,])
  
  train <- train[sample(nrow(train)),]
  test <- test[sample(nrow(test)),]
  
  return(list(train = train, test = test))
}

multinomial_modelise <- function(data, form) {
  train <- data$train
  test <- data$test
  
  m <- nnet::multinom(formula = form, data = train)
  result <- predict(m, newdata=test, type="class")
  
  print(form)
  return(confusionMatrix(data = as.factor(result), reference = as.factor(test$Status)))
}

multinomial <- function(data, balance) {
  m0 <- Status ~ V2 + V5 + V6 + V7 + V8
  m1 <- Status ~ V2 + V5 + V6 + V7 + V8 + V3
  m2 <- Status ~ V2 + V5 + V6 + V7 + V8 + FICO_Score
  m3 <- Status ~ V2 + V5 + V6 + V7 + V8 + FICO_Score + Business_channel
  m4 <- Status ~ V2 + V5 + V6 + V7 + V8 + FICO_Score + Business_channel + Age
  m5 <- Status ~ FICO_Score + Business_channel
  m6 <- Status ~ FICO_Score + Business_channel + Age
  
  save_data <- data
  
  if (balance == 1) { 
    data <- multinomial_balance(data = data, balance = FALSE, fico = FALSE) 
  } else { 
    data <- multinomial_balance(data = data, balance = TRUE, fico = FALSE)
  }
  data <- multinomial_split(data = data, ratio = .8)
  
  print(multinomial_modelise(data = data, form = m0))
  print(multinomial_modelise(data = data, form = m1))
  
  if (balance == 1) { 
    data <- multinomial_balance(data = save_data, balance = FALSE, fico = TRUE) 
  }
  else {
    data <- multinomial_balance(data = save_data, balance = TRUE, fico = TRUE)
  }
  data <- multinomial_split(data = data, ratio = .8)
  
  print(multinomial_modelise(data = data, form = m2))
  print(multinomial_modelise(data = data, form = m3))
  print(multinomial_modelise(data = data, form = m4))
  print(multinomial_modelise(data = data, form = m5))
  print(multinomial_modelise(data = data, form = m6))
}