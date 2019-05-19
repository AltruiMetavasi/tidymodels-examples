# prepare environment -----------------------------------------------------

# clean environment
rm(list = ls())

# import libs
library(lubridate)
library(tidyverse)
library(tidymodels)

# import dataset ----------------------------------------------------------

# import
data_clean <- readRDS("data/sample/data-clean.rds")

# quick check
glimpse(data_clean)

# split train and new sample ----------------------------------------------

# split train
set.seed(100)
splitted <- initial_split(data_clean, prop = 0.8, strata = "y")

# readjust new sample
data_new <- testing(splitted)

data_new_response <- data_new %>%
  select(cust_id, call_date, y)

data_new <- data_new %>%
  select(-y)

# export ------------------------------------------------------------------

# write to csv
write_csv(training(splitted), "data/data-train.csv")
write_csv(data_new, "data/data-new.csv")
write_csv(data_new_response, "data/data-new-response.csv")
