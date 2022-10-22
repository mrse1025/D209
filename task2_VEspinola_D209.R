library(tidyr)
library(tidymodels)
library(class)
library(janitor)
library(caret)
library(pROC)
source("cleaner_copy.R")

churn_data <- read.csv("churn_clean.csv", header = TRUE) %>% 
  janitor::clean_names() %>%
  churn_cleaning() %>%
  select(-uid, -case_order, -lat, -lng, -state, -county,-time_zone)

set.seed(123)

splits <- initial_split(
                        churn_data,
                        prop   = .80,
                        strata = churn
                        )

train_tbl <- training(splits)
test_tbl <- testing(splits)

#recipe goes here



#tuning 
#dummy specs 

tune_spec <- decision_tree(
  tree_depth = tune (), 
  cost_complexity = tune()
) %>%
  set_mode("classification") %>%
  set_engine("rpart")

#set up tuning grid

model_grid <- grid_regular(
  parameters(tune_spec), 
  levels = 5
)

folds <- vfold_cv(train_tbl, v = 10)

tune_results <- tune_grid(tune_spec, 
                          churn ~. , 
                          resamples = folds,
                          grid      = model_grid,
                          metrics   = metric_set(accuracy)
                          )

autoplot(tune_results)


best_tunes <- select_best(tune_results)
best_spec <- finalize_model(tune_spec, best_tunes)

#Fitting the model 
final_model <- fit(best_spec, 
                   churn~., 
                   train_tbl
                   )

pred_test <- predict(final_model, new_data = test_tbl,type ="class")
# #complete one-hot here
# one_hot_dummy <- dummyVars(
#   " ~.",
#   data = (churn_data %>%
#             select(where(is.factor) & !contains("churn"))
#   )
# )
# 
# 
# dummy_df <- data.frame(
#   predict(
#     one_hot_dummy,
#     newdata = (churn_data %>%
#                  select(where(is.factor) & !contains("churn"))
#     )
#   )
# )

# final_data_set <- dummy_df %>% bind_cols(churn_data) %>% #put together numeric with dummied variables
#   select(!where(is.factor), churn)%>% #remove all non-dummied variables 
#   
#   janitor::clean_names() 
# 
# 
# #normalizing numeric data
# 
# scaled_ds <- final_data_set %>% 
#   mutate(
#     across(where(is.numeric), ~(.x-min(.x))/(max(.x)-min(.x))
#     )
#   )
# 
# skimr::skim(scaled_ds)

