# prepare environment -----------------------------------------------------

# clean environment
rm(list = ls())

# import libs
library(snakecase)
library(stringi)
library(tidyverse)

# import dataset ----------------------------------------------------------

# import
data_raw <- read_csv("data-raw/Airline-Sentiment-2-w-AA.csv")

# quick check
glimpse(data_raw)

# cleaning ----------------------------------------------------------------

# get problematic rows
problem_rows <- problems(data_raw) %>%
  pull(row) %>%
  unique()

# remove problematic rows
data_raw <- data_raw %>%
  slice(-problem_rows)

# readjust non-ascii
data_raw <- data_raw %>%
  mutate(text = stri_trans_general(text, id = "ASCII-Latin"))

# finalize ----------------------------------------------------------------

# readjust all column names
data_raw <- data_raw %>%
  rename_all(~ to_snake_case(.))

# readjust columns
data_clean <- data_raw %>%
  select(sentiment = airline_sentiment, tweet = text, everything())

# export ------------------------------------------------------------------

# write to csv
write_csv(data_clean, "data/data-clean.csv")
