# prepare environment -----------------------------------------------------

# clean environment
rm(list = ls())

# import libs
library(dials)
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

# set-up cv scenario ------------------------------------------------------

# set seed
set.seed(100)

# create cv split
cv_split <- vfold_cv(juice(rec), v = 3, repeats = 2, strata = "y")

# set-up parsnip models ---------------------------------------------------

# set-up a model engine
model_engine <- rand_forest(mode = "classification") %>%
  set_engine(
    engine = "ranger",
    seed = 100,
    num.threads = parallel::detectCores(),
    importance = "impurity"
  )

# set-up model grid
model_grid <- grid_regular(
  range_set(mtry, range = c(2, ncol(juice(rec)) - 1)),
  range_set(trees, range = c(500, 1000)),
  range_set(min_n, range = c(1, 10)),
  levels = 2
)

# merge model engine and grid
model_specs <- tibble(spec = merge(model_engine, model_grid)) %>%
  mutate(spec_id = str_pad(row_number(), width = 2, side = "left", pad = "0"))

# combine cv splits and models --------------------------------------------

# cross cv splits and model specs
crossed <- crossing(cv_split, model_specs)

# model fitting -----------------------------------------------------------

# fit on every folds
crossed <- crossed %>%
  mutate(model = map2(spec, splits, ~
    fit_xy(.x, x = select(analysis(.y), -y), y = select(analysis(.y), y))
  ))

# get cv result -----------------------------------------------------------

# get hold-out prediction in every folds
cv_result <- crossed %>%
  mutate(prediction = map2(model, splits, ~
    assessment(.y) %>%
    bind_cols(predict(.x, new_data = .)) %>%
    bind_cols(predict(.x, new_data = ., type = "prob")) %>%
    select(y, starts_with(".pred"))
  ))

# unnest the cv result
cv_result <- cv_result %>%
  select(id, id2, spec_id, prediction) %>%
  unnest(prediction)

# give every spec in model grid an id
model_grid <- model_grid %>%
  mutate(spec_id = str_pad(row_number(), width = 2, side = "left", pad = "0"))

# combine with cv result
cv_result <- cv_result %>%
  left_join(model_grid)

# explore model performance -----------------------------------------------

# each specification performances across all folds
cv_result %>%
  group_by(spec_id, id, id2) %>%
  summarise(
    accuracy = accuracy_vec(y, .pred_class),
    recall = recall_vec(y, .pred_class),
    precision = precision_vec(y, .pred_class)
  ) %>%
  ungroup() %>%
  gather(metrics, value, accuracy:precision) %>%
  ggplot(aes(x = spec_id, y = value)) +
    geom_boxplot() +
    facet_wrap(~ metrics, scales = "free", ncol = 1, labeller = "label_both") +
    labs(x = "Model ID", y = "Metrics Value") +
    theme_minimal()

# each parameter combination performances across all folds
cv_result %>%
  group_by(mtry, trees, min_n) %>%
  summarise(
    accuracy = accuracy_vec(y, .pred_class),
    recall = recall_vec(y, .pred_class),
    precision = precision_vec(y, .pred_class)
  ) %>%
  ungroup() %>%
  gather(metrics, value, accuracy:precision) %>%
  ggplot(aes(x = mtry, y = value)) +
    geom_point(aes(colour = factor(trees))) +
    geom_line(aes(colour = factor(trees))) +
    facet_wrap(~ metrics + min_n, scales = "free", ncol = 2, labeller = "label_both") +
    labs(y = "Metrics Value", colour = "trees") +
    theme_minimal()

# get best model ----------------------------------------------------------

# filter to model with best performance across all folds
best_model <- cv_result %>%
  group_by(spec_id, id, id2) %>%
  summarise(recall = recall_vec(y, .pred_class)) %>%
  group_by(spec_id) %>%
  summarise(recall = mean(recall)) %>%
  ungroup() %>%
  arrange(desc(recall)) %>%
  head(1)

# check best model specification
best_model %>%
  left_join(model_grid)

# get the best model spec
model_spec <- crossed %>%
  right_join(best_model) %>%
  distinct(spec_id, .keep_all = TRUE) %>%
  pull(spec) %>%
  pluck(1)

# final model evaluation --------------------------------------------------

# fit on train
model <- fit_xy(model_spec, x = juice(rec, -y), y = juice(rec, y))

# get variable importance
var_imp <- tidy(model$fit$variable.importance) %>%
  mutate(names = reorder(names, x)) %>%
  rename(variable = names, importance = x)

# variable importance plot
ggplot(var_imp, aes(x = variable, y = importance)) +
  geom_col(aes(fill = importance)) +
  coord_flip() +
  labs(title = "Variables Importance", x = NULL, y = NULL, fill = NULL) +
  scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
  theme_minimal()

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
    precision = precision_vec(y, .pred_class),
    roc_auc = roc_auc_vec(y, .pred_yes)
  )

# roc curve
pred_test %>%
  roc_curve(y, .pred_yes) %>%
  autoplot()

# export ------------------------------------------------------------------

# save model to rds
saveRDS(model, "model/model.rds")

# save recipe to rds
saveRDS(rec, "model/rec.rds")
