# load packages
library(tidyverse)
library(tidymodels)
library(textrecipes)
library(stacks)
tidymodels_prefer()

# load data
load("data/wrangle/text_classification.rda")
set.seed(123)

# split data + v-fold
data_split <- initial_split(data, prop = 0.8, strata = category)
data_train <- training(data_split)
data_test <- testing(data_split)
data_fold <- vfold_cv(data_train, v = 3, repeats = 1, strata = category)

# pre-processing ----
stop_words <- c("event", "chicago", "please", "program", "camp", "tbd", "tba",
                "internship")
  
charcter_2 <- function(x) {
  return(nchar(x) > 2)
}
starts_zoom <- function(x) {
  return(!(startsWith(x, "zoom")))
}

recipe <- recipe(category ~ description, data = data_train) %>%
  # tokenization
  step_tokenize(description) %>%
  # stopwords removal
  step_stopwords(description, custom_stopword_source = stop_words) %>%
  # stem words
  step_stem(description) %>%
  # filter out words with <= 2 characters
  step_tokenfilter(description, filter_fun = charcter_2) %>%
  # filter out words starting with "zoom"
  step_tokenfilter(description, filter_fun = starts_zoom) %>%
  # select tokens
  step_tokenfilter(description, max_tokens = 500) %>%
  # convert to tf-idf
  step_tfidf(description) 

View(recipe %>%
  prep() %>%
  bake(new_data = data_train))

# model specs ----
lasso_spec <- multinom_reg(mode = "classification",
                           penalty = tune(), 
                           mixture = tune()) %>%
  set_engine("glmnet")

rf_spec <- rand_forest(mode = "classification",
                       min_n = tune(),
                       mtry = tune()) %>% 
  set_engine("ranger")

kn_spec <- nearest_neighbor(mode = "classification",
                            neighbors = tune()) %>%
  set_engine("kknn")

svm_spec <- svm_linear(mode = "classification",
                       cost = tune(),
                       margin = tune()) %>%
  set_engine("kernlab")

# model workflows ----
lasso_wkfl <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(lasso_spec)

rf_wkfl <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(rf_spec)

kn_wkfl <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(kn_spec)

svm_wkfl <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(svm_spec)

# parameter tuning grid ----
## lasso grid
lasso_params <- parameters(lasso_spec) %>%
  update(mixture = mixture(c(0, 1)))
lasso_grid <- grid_regular(lasso_params, levels = 3)

## random forest grid
rf_params <- parameters(rf_spec) %>% 
  update(mtry = mtry(c(1, 500)))
rf_grid <- grid_regular(rf_params, levels = 3)

## boosted tree grid
bt_params <- parameters(bt_spec) %>%
  update(mtry = mtry(c(1, 1000)))
bt_grid <- grid_regular(bt_params, levels = 3)

## k-nearest neighbor grid
kn_grid <- grid_regular(parameters(kn_spec), levels = 3)

## svm grid
svm_grid <- grid_regular(parameters(svm_spec), levels = 3)

# tune parameters ----
metrics <- metric_set(accuracy, f_meas, precision, recall)
ctrl_grid <- control_stack_grid()

lasso_tuned <- lasso_wkfl %>%
  tune_grid(data_fold, grid = lasso_grid, metrics = metrics, control = ctrl_grid)
save(lasso_tuned, file = "model/tuned/lasso_tuned.rda")

rf_tuned <- rf_wkfl %>% 
  tune_grid(data_fold, grid = rf_grid, metrics = metrics, control = ctrl_grid)
save(rf_tuned, file = "model/tuned/rf_tuned.rda")

kn_tuned <- kn_wkfl %>%
  tune_grid(data_fold, grid = kn_grid, metrics = metrics, control = ctrl_grid)
save(kn_tuned, file = "model/tuned/kn_tuned.rda")

svm_tuned <- svm_wkfl %>% 
  tune_grid(data_fold, grid = rf_grid, metrics = metrics, control = ctrl_grid)
save(svm_tuned, file = "model/tuned/svm_tuned.rda")

## best model
load("model/tuned/lasso_tuned.rda")
load("model/tuned/rf_tuned.rda")
load("model/tuned/kn_tuned.rda")
load("model/tuned/svm_tuned.rda")

show_best(lasso_tuned, metric = metrics, n = 1)
show_best(rf_tuned, metric = metrics, n = 1)
show_best(kn_tuned, metric = metrics, n = 1)
show_best(svm_tuned, metric = metrics, n = 1)

# stack models ----
blend_penalty <- c(10^(-6:-1), 0.5, 1, 1.5, 2)

model_stack <- stacks() %>%
  add_candidates(lasso_tuned) %>%
  add_candidates(rf_tuned) %>%
  add_candidates(kn_tuned) %>%
  add_candidates(svm_tuned) %>%
  blend_predictions(penalty = blend_penalty)

# fit best tuned model ----
model_stack_fitted <- model_stack %>%
  fit_members()
save(model_stack_fitted, file = "model_info/model_stack_fitted.rda")

wkfl_tuned <- bt_wkfl %>% 
  finalize_workflow(select_best(bt_tuned, metric = "f_meas"))
bt_fit <- fit(wkfl_tuned, data_train)
save()

# test model----
predict(fit, new_data = data_test) %>% 
  bind_cols(data_test %>% select(category)) %>% 
  mutate(category = as.factor(category)) %>%
  metrics(truth = category, estimate = .pred_class)
