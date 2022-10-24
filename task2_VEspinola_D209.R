library(tidyr)
library(tidymodels)
library(class)
library(janitor)
library(caret)
library(pROC)
source("cleaner_copy.R")

churn_data <- read.csv("churn_clean.csv", header = TRUE) %>% 
  janitor::clean_names() %>%
  churn_cleaning() 
#recipe goes here
# Create training and testing splits ----
set.seed(123)
splits <- initial_split(churn_data, prop = 0.80)

train_churn <- training(splits)
test_churn  <- testing(splits)


# Create feature engineering recipe for initial model ----
recipe_spec <- recipe(churn ~., data = train_churn ) %>%
  #impute the missing variables with the mean
  step_impute_mean(all_numeric_predictors()) %>%  
  #removed any identification variables that do not add insight into the classification model.
  step_rm(county, state, time_zone, lat, lng, zip) %>% 
  #removed all no variance variable from all predictors 
  step_nzv(all_predictors())%>%
  #group together all nominal predictors into an other category when they are retained from low variance, 
  #but account for 0.5% of the data
  step_other(all_nominal_predictors(),threshold = 0.005) %>%
  #normalize the numeric data with a log transform; this step is important to reduce any outliers in the data set. 
  step_log(all_numeric_predictors(),offset = 1)
  #dummy the nominal predictors with one-hot encoding method 
  #step_dummy(all_nominal_predictors(), one_hot = TRUE, keep_original_cols = FALSE) 

#Full_prepped_data
prepped_data <- recipe_spec %>% prep() %>% bake(new_data = churn_data) %>% janitor::clean_names()
skimr::skim(prepped_data)

#Modeling
rand_forest_spec <- rand_forest(
  mtry  = tune(), 
  trees = tune(), 
  min_n = tune()
) %>%
  set_mode("classification") %>%
  set_engine("ranger")

set.seed(123)

tree_folds <- vfold_cv(data = train_churn, v = 2)
#set up tuning grid


test_grid <- tidyr::crossing(
  mtry  = 1:10, 
  trees = seq(100,1000,100),
  min_n = seq(1,5,1)
)

set.seed(123)

tune_results <- tune_grid(rand_forest_spec, 
                          recipe_spec,
                          resamples = tree_folds,
                          grid      = test_grid,
                          metrics   = metric_set(accuracy)
)

autoplot(tune_results)


best_tunes <- select_best(tune_results)
best_spec <- finalize_model(rand_forest_spec, best_tunes)


#Finalizing the model
final_wflw <- workflow() %>%
  add_recipe(recipe_spec) %>%
  add_model(best_spec) %>%
  fit(train_churn)

#add fit resample
fit_resample <- fit_resamples(
                            best_spec, 
                            recipe_spec, 
                            resamples = tree_folds,
                            metrics   = metric_set(auc, accuracy)
                    )

pred_test_prob <- predict(final_wflw, new_data = test_churn,type ="prob")

test_with_preds <- test_churn %>% bind_cols(pred_test_prob) 
eval_roc_auc <- roc_curve(test_with_preds, truth = churn, estimate = .pred_Yes)
