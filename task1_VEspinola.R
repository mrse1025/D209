library(tidyr)
library(class)
library(janitor)
library(tidymodels)
source("cleaner_copy.R")

churn_data <- read.csv("churn_clean.csv", header = TRUE) %>% 
  janitor::clean_names() %>%
  churn_cleaning() %>%
  select(-uid, -case_order, -lat, -lng) 

#complete one-hot here

#normalizing numeric data

scaled_ds <- churn_data %>% 
    mutate(
    across(where(is.numeric), ~(.x-min(.x))/(max(.x)-min(.x))
      )
    )

#setting seed and splitting into train/test. 
set.seed(123)
splits <- initial_split(scaled_ds)
train_data <- training(splits)
test_data <- testing(splits)

knn(train = train_data %>% select(-churn),test = test_data %>%select(-churn),k = 5, cl = train_data$churn)
