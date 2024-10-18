library(dplyr)
library(h2o)
library(readr)
library(reactable)

# Preparing objects
ml <- list()
colors <- c("#e8e9ed", "#e89978", "#4a57a6", "#4192b5")

# Start h2o cluster
h2o::h2o.init()

# Reading the data
ml$data$raw <- readr::read_csv(
  "https://raw.githubusercontent.com/wrprates/open-data/master/telco_customer_churn.csv"
) |>
  dplyr::mutate(across(where(is.character), as.factor))

# Defining variables
ml$vars$y <- "Churn"
ml$vars$discard <- "customerID"
ml$vars$x <- setdiff(names(ml$data$raw), c(ml$vars$y, ml$vars$discard))

# Setup h2o
ml$data$h2o <- h2o::as.h2o(ml$data$raw)
ml$data$splits <- h2o::h2o.splitFrame(ml$data$h2o, ratios = 0.7)
names(ml$data$splits) <- c("train", "test")

# Running the model
ml$model <- h2o::h2o.gbm(x = ml$vars$x, y = ml$vars$y, training_frame = ml$data$splits$train)
ml$predictions <- h2o::h2o.predict(ml$model, ml$data$splits$test)
h2o::h2o.performance(ml$model, ml$data$splits$test)