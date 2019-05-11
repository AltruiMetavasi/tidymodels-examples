# prepare environment -----------------------------------------------------

# clean environment
rm(list = ls())

# import libs
library(ranger)
library(tidymodels)
library(tidyverse)

# import dataset ----------------------------------------------------------

# import
data_train <- read_csv("data/data-train.csv")

# quick check
glimpse(data_train)

# split train-test --------------------------------------------------------

# set seed
set.seed(100)

# create initial split scheme
splitted <- initial_split(data_train, prop = 0.8, strata = "y")

# set-up recipe -----------------------------------------------------------

# create a recipe based on training data
rec <- recipe(y ~ ., data = training(splitted)) %>%
  step_rm(cust_id, call_date) %>%
  step_string2factor(all_nominal(), -all_outcomes()) %>%
  step_string2factor(y, levels = c("yes", "no")) %>%
  step_downsample(y, seed = 100) %>%
  step_center(all_numeric()) %>%
  step_scale(all_numeric()) %>%
  # step_pca(all_numeric(), threshold = 0.75) %>%
  # step_dummy(all_nominal(), -all_outcomes()) %>%
  prep(strings_as_factors = FALSE)

# set-up parsnip models ---------------------------------------------------

# set-up a model specification
model_spec <- rand_forest(
  mode = "classification",
  mtry = ncol(juice(rec)) - 1,
  trees = 500,
  min_n = 1
)

# set-up model engine
model_spec <- model_spec %>%
  set_engine(engine = "ranger", seed = 100, num.threads = parallel::detectCores())

# model fitting -----------------------------------------------------------

# fit on train
model <- fit_xy(model_spec, x = juice(rec, -y), y = juice(rec, y))

# model evaluation --------------------------------------------------------

# predict on test
pred_test <- bake(rec, testing(splitted)) %>%
  bind_cols(predict(model, new_data = .)) %>%
  bind_cols(predict(model, new_data = ., type = "prob"))

# confusion matrix
pred_test %>%
  conf_mat(y, .pred_class) %>%
  autoplot(type = "heatmap")

# metrics summary
pred_test %>%
  summarise(
    accuracy = accuracy_vec(y, .pred_class),
    recall = recall_vec(y, .pred_class),
    precision = precision_vec(y, .pred_class)
  )

# export ------------------------------------------------------------------

# save model to rds
saveRDS(model, "model/model.rds")

# save recipe to rds
saveRDS(rec, "model/rec.rds")
