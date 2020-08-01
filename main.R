source("file.R")
source("plot.R")
source("multinomial.R")
source("ordinal.R")

main <- function(action, fileName) {
  # Load the data set contained in "data" folder
  data_set <- openFile(fileName)
  if (action == "data analysis") {
    # Plot all the necessary elements for data analysis
    distribution(data_set)
  } else if (action == "multinomial regression") {
    # Apply the regression analysis for the classification
    multinomial(data_set, balance = 1)
  } else if (action == "ordinal regression") {
    # ADD Ordinal regression
    ordinal1(data_set, balance = 1)
  } else if (action == "binary regression") {
    # ADD Binary regression
    binary(data_set, sampling = 1)
  }
}

main(action = "multinomial regression", fileName = "data/dataset_1.csv")