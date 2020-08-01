library(nnet)
library(party)
library(caret)
library(e1071)

multi_generate <- function(data, size) {
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

multi_balance <- function(data, balance, fico) {
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
    data_set <- rbind(data_set, multi_generate(Cat_1, rows))
    data_set <- rbind(data_set, multi_generate(Cat_2, rows))
    data_set <- rbind(data_set, multi_generate(Cat_3, rows))
  }
  
  # Randomize data set
  data_set <- data_set[sample(nrow(data_set)),]
  
  # Return final data set
  return(data_set)
}

multi_split <- function(data, ratio) {
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

multi_modelise <- function(data, fico) {
  train <- data$train
  test <- data$test
  
  form <- Status ~ V1 + V2 + V5 + FICO_Score + Business_channel + Age
  if (fico == FALSE) { form <- Status ~ V1 + V2 + V5 + Business_channel + Age}
  
  model <- multinom(formula = form, data = train)
  
  result <- predict(model, newdata = test)
  
  return(confusionMatrix(result, test$Status))
}

multi_regression <- function(data, balance, fico, ratio) {
  data <- multi_balance(data = data, balance = balance, fico = fico)
  data <- multi_split(data = data, ratio = ratio)
  print(result <- multi_modelise(data = data, fico = fico))
}

multinomial <- function(data) {
  # Under = 0, Over = 1, Without_Fico = 0, With_FICO = 1
  multi_regression(data = data, balance = FALSE, fico = FALSE, ratio = .8)
  multi_regression(data = data, balance = FALSE, fico = TRUE, ratio = .8)
  multi_regression(data = data, balance = TRUE, fico = FALSE, ratio = .8)
  multi_regression(data = data, balance = TRUE, fico = TRUE, ratio = .8)
}