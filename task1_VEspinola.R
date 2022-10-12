library(tidyr)
library(class)
library(janitor)
library(caret)
library(tidymodels)
source("cleaner_copy.R")

churn_data <- read.csv("churn_clean.csv", header = TRUE) %>% 
  janitor::clean_names() %>%
  churn_cleaning() %>%
  select(-uid, -case_order, -lat, -lng, -state, -county,-time_zone)


#complete one-hot here
one_hot_dummy <- dummyVars(
  " ~.",
  data = (churn_data %>%
            select(where(is.factor) & !contains("churn"))
          )
  )
dummy_df <- data.frame(
  predict(
    one_hot_dummy,
    newdata = (churn_data %>%
                 select(where(is.factor) & !contains("churn"))
               )
    )
)

final_data_set <- dummy_df %>% bind_cols(churn_data) %>% #put together numeric with dummied variables
  select(!where(is.factor), churn)%>%  #remove all non-dummied variables 
  janitor::clean_names() 


#normalizing numeric data

scaled_ds <- final_data_set %>% 
    mutate(
    across(where(is.numeric), ~(.x-min(.x))/(max(.x)-min(.x))
      )
    )


#setting seed and splitting into train/test. 
set.seed(123)
splits <- initial_split(scaled_ds)
train_data <- training(splits) %>% select(-churn)
churn_train <- training(splits)$churn
test_data <- testing(splits) %>% select(-churn)
churn_test <- testing(splits)$churn


knn(train = train_data,test = test_data,k = 5, cl = churn_train)
