# prepare environment -----------------------------------------------------

# clean environment
rm(list = ls())

# import libs
library(lubridate)
library(snakecase)
library(tidyverse)

# import dataset ----------------------------------------------------------

# import
data_raw <- read_delim("data-raw/bank-full.csv", delim = ";")

# quick check
# glimpse(data_raw)

# filter-out "unknown" ----------------------------------------------------

# check "unknown" data
# data_raw %>%
#   select_if(is.character) %>%
#   mutate_all(as.factor) %>%
#   summary()

# filter-out abnormal "unknown"
data_raw <- data_raw %>%
  filter(
    job != "unknown",
    education != "unknown",
    contact != "unknown",
  )

# check "unknown" data
# data_raw %>%
#   filter(previous != 0) %>%
#   pull(poutcome) %>%
#   table()

# data_raw %>%
#   filter(pdays != -1) %>%
#   pull(poutcome) %>%
#   table()

# filter-out abnormal "unknown"
data_raw <- data_raw %>%
  filter(
    !(previous != 0 & poutcome == "unknown"),
    !(pdays != -1 & poutcome == "unknown")
  )

# quick check
# glimpse(data_raw)

# filter-out outliers -----------------------------------------------------

# select eligible numeric columns
num_cols <- data_raw %>%
  select_if(is.numeric) %>%
  select(-day) %>%
  colnames()

# filter-out outliers
data_raw <- data_raw %>%
  mutate_at(num_cols, ~ ifelse(abs(. - mean(.)) > 3 * sd(.), NA, .)) %>%
  drop_na()

# quick check
# glimpse(data_raw)

# readjust factors --------------------------------------------------------

# readjust all nominal columns
data_raw <- data_raw %>%
  mutate_if(is.character, ~ to_snake_case(.))

# quick check
# glimpse(data_raw)

# add more realistic column -----------------------------------------------

# add date
sample_date <- seq.Date(Sys.Date() - years(1), Sys.Date(), by = "day")

data_raw <- data_raw %>%
  mutate(call_date = sample(sample_date, nrow(.), replace = TRUE))

# add customer id
data_raw <- data_raw %>%
  mutate(cust_id = paste0("cust_", str_pad(row_number(), width = 6, side = "left", pad = "0")))

# finalize ----------------------------------------------------------------

# select used columns
data_clean <- data_raw %>%
  select(cust_id, call_date, everything(), -duration)

# export ------------------------------------------------------------------

# save to rds
saveRDS(data_clean, "data/sample/data-clean.rds")
