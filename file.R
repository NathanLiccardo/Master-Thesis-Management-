openFile <- function(filename) {
  data <- read.csv(filename, header = TRUE, sep = ";", dec=",")
  
  # Set types of data -> Inputs
  data$ID <- as.integer(data$ID)
  data$V1 <- as.factor(data$V1)
  data$V2 <- as.factor(data$V2)
  data$V3 <- as.factor(data$V3)
  data$V4 <- as.factor(data$V4)
  data$V5 <- as.factor(data$V5)
  data$V6 <- as.factor(data$V6)
  data$V7 <- as.factor(data$V7)
  data$V8 <- as.factor(data$V8)
  data$Has_FICO <- as.logical(data$Has_FICO)
  data$FICO_Score <- as.numeric(data$FICO_Score)
  data$Business_channel <- as.factor(data$Business_channel)
  data$Expected_loss <- as.numeric(data$Expected_loss)
  data$Expected_profit <- as.numeric(data$Expected_profit)
  data$Loan_amount <- as.numeric(data$Loan_amount)
  data$Monthly_income <- as.numeric(data$Monthly_income)
  data$Age <- as.numeric(data$Age)
  data$Gearing_coefficient <- as.numeric(data$Gearing_coefficient)
  data$Max_gearing_ratio <- as.numeric(data$Max_gearing_ratio)
  
  # Set types of data -> Outputs
  data$Days_late <- as.integer(data$Days_late)
  data$Status <- factor(data$Status, ordered=TRUE, levels=c("Bad", "Middle", "Good"))
  data$Default_45 <- as.logical(data$Default_45)
  
  data
}