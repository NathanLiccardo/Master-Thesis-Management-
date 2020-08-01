library(ggplot2)
library(xtable)
library(dplyr)

plot_table <- function(table, type, name) {
  print(xtable(table, type = type), file = name)
}

output_distribution <- function(data) {
  table1 <- prop.table(table(data$Default_45))
  table2 <- prop.table(table(data$Status))
  plot_table(table = table1, type = "latex", name = "output/Default_45_table.tex")
  plot_table(table = table2, type = "latex", name = "output/Status_table.tex")
  print(ggplot(data = data, aes_string(x = "Default_45")) + geom_bar())
  print(ggplot(data = data, aes_string(x = "Status")) + geom_bar())
}

anonymized_distribution <- function(data) {
  input <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8")
  output <- c("Default_45", "Status")
  for (input_i in input) {
    for (output_i in output) {
      table <- prop.table(table(data[[output_i]], data[[input_i]]), 2)
      names <- paste("output/", input_i, "_", output_i, "_table.tex", sep="")
      plot_table(table = table, type = "latex", name = names)
    }
  }
  table <- table(data$Default_45, data$V3)
  plot_table(table = table, type = "latex", name = "output/V3_Default_45_table.tex")
  table <- table(data$Status, data$V3)
  plot_table(table = table, type = "latex", name = "output/V3_Status_table.tex")
}

anonymized_barplot <- function(data) {
  table <- prop.table(table(data$Default_45, data$V3), 2)
  print(table(data$Default_45, data$V3))
  barplot(table ,legend = c(TRUE, FALSE), args.legend = list(title = "Default", x = "topright", cex = .7))
  table <- prop.table(table(data$Status, data$V3), 2)
  print(table(data$Status, data$V3))
  barplot(table ,legend = c("Bad", "Middle", "Good"), args.legend = list(title = "Status", x = "topright", cex = .7))
}

anonymized_records <- function(data) {
  input <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8")
  output <- c("Default_45", "Status")
  for (input_i in input) {
    for (output_i in output) {
      print(table(data[[output_i]], data[[input_i]]))
    }
  }
}

fico_distribution <- function(data) {
  input <- c("Has_FICO", "FICO_Score")
  output <- c("Default_45", "Status")
  for (input_i in input) {
    for (output_i in output) {
      table <- prop.table(table(data[[output_i]], data[[input_i]]), 2)
      names <- paste("output/", input_i, "_", output_i, "_table.tex", sep="")
      print(xtable(table, type = "latex"), file = names)
      print(table(data[[output_i]], data[[input_i]]))
    }
  }
}

fico_plot <- function(data) {
  print(ggplot(data = data, aes_string(x = "Has_FICO")) + geom_histogram(aes(fill=Default_45), stat="count"))
  print(ggplot(data = data, aes_string(x = "Has_FICO")) + geom_histogram(aes(fill=Status), stat="count"))
  print(ggplot(data = data, aes_string(x = "FICO_Score")) + geom_density() + facet_grid(rows = vars(Default_45)))
  print(ggplot(data = data, aes_string(x = "FICO_Score")) + geom_density() + facet_grid(rows = vars(Status)))
}

channel_distribution <- function(data) {
  table1 <- prop.table(table(data[["Default_45"]], data[["Business_channel"]]), 2)
  table2 <- prop.table(table(data[["Status"]], data[["Business_channel"]]), 2)
  name1 <- paste("output/Business_channel_Default_45_table.tex")
  name2 <- paste("output/Business_channel_Status_table.tex")
  plot_table(table = table1, type = "latex", name = name1)
  plot_table(table = table2, type = "latex", name = name2)
}

expected_plot <- function(data) {
  print(ggplot(data = data, aes_string(x = "Expected_loss")) + geom_density() + facet_grid(rows = vars(Default_45)))
  print(ggplot(data = data, aes_string(x = "Expected_loss")) + geom_density() + facet_grid(rows = vars(Status)))
  print(ggplot(data = data, aes_string(x = "Expected_profit")) + geom_density() + facet_grid(rows = vars(Default_45)))
  print(ggplot(data = data, aes_string(x = "Expected_profit")) + geom_density() + facet_grid(rows = vars(Status)))
}

amount_distribution <- function(data) {
  print(ggplot(data = data, aes(Loan_amount)) + geom_density() + facet_grid(rows=vars(Default_45)))
  print(ggplot(data = data, aes(Loan_amount)) + geom_histogram(aes(fill=Default_45), bins = 400) + xlim(-2,10))
  print(ggplot(data = data, aes(Loan_amount)) + geom_density() + facet_grid(rows=vars(Status)))
  print(ggplot(data = data, aes(Loan_amount)) + geom_histogram(aes(fill=Status), bins = 400) + xlim(-2,10))
}

income_distribution <- function(data) {
  print(ggplot(data = data, aes(Monthly_income)) + geom_density() + facet_grid(rows=vars(Default_45)) + xlim(-2,5))
  print(ggplot(data = data, aes(Monthly_income)) + geom_density() + facet_grid(rows=vars(Status)) + xlim(-2,5))
}

age_distribution <- function(data) {
  # Distribution of variable Age
  print(ggplot(data = data, aes(Age)) + geom_density() + facet_grid(rows=vars(Default_45)))
  print(ggplot(data = data, aes(Age)) + geom_density() + facet_grid(rows=vars(Status)))
}

gearing_distribution <- function(data) {
  # Distribution of variable Gearing_coef
  print(ggplot(data = data, aes(Gearing_coefficient)) + geom_density()+ facet_grid(rows=vars(Default_45)) + xlim(-2,5))
  print(ggplot(data = data, aes(Gearing_coefficient)) + geom_density()+ facet_grid(rows=vars(Status)) + xlim(-2,5))
  # Distribution of variable Gearing_coef, Max_gearing
  print(ggplot(data = data, aes(Max_gearing_ratio)) + geom_density()+ facet_grid(rows=vars(Default_45)) + xlim(-2,5))
  print(ggplot(data = data, aes(Max_gearing_ratio)) + geom_density()+ facet_grid(rows=vars(Status)) + xlim(-2,5))
}

inputs_distribution <- function(data) {
  anonymized_distribution(data)
  anonymized_barplot(data)
  anonymized_records(data)
  fico_distribution(data)
  fico_plot(data)
  channel_distribution(data)
  expected_plot(data)
  amount_distribution(data)
  income_distribution(data)
  age_distribution(data)
  gearing_distribution(data)
}

distribution <- function(data) {
  output_distribution(data)
  inputs_distribution(data)
}