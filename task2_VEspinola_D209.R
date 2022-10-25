library(tidyverse)
library(tidymodels)
library(janitor)
library(vip)

churn_data <- read.csv("churn_clean.csv", header = TRUE) %>% 
  mutate_if(is.character,as.factor) %>%
  janitor::clean_names()
 
# Create training and testing splits ----
set.seed(123)
splits <- initial_split(churn_data, prop = 0.80, strata = churn)

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
  #normalize the numeric data; this step is important to reduce any outliers in the data set. 
  step_normalize(all_numeric_predictors())

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
  set_engine("ranger", importance = "permutation")

set.seed(123)

tree_folds <- vfold_cv(data = train_churn, v = 8, strata = churn)
#set up tuning grid


test_grid <- tidyr::crossing(
  mtry  = 5:7,
  trees = seq(10,30,10),
  min_n = 1:3
)

set.seed(123)
doParallel::registerDoParallel()
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
  add_model(best_spec)  

#add fit resample
fit_resample <- fit_resamples(
                            final_wflw,
                            resamples = tree_folds,
                            metrics   = metric_set(roc_auc, accuracy)
                    )


collect_metrics(fit_resample, summarize = TRUE)

full_model <- final_wflw %>%
  last_fit(splits)

full_model %>% collect_predictions() %>%
  roc_curve(truth = churn, estimate = .pred_No) %>%
  autoplot()

full_model %>% collect_metrics()

extract_workflow(full_model)%>%
  extract_fit_parsnip()%>%
  vip(geom="point",num_features=10)
