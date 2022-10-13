library(tidyr)
library(class)
library(janitor)
library(caret)
library(tidymodels)
library(pROC)
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

skimr::skim(scaled_ds)
#setting seed and splitting into train/test. 
set.seed(123)
splits <- initial_split(scaled_ds)
train_data <- training(splits) %>% select(-churn)
churn_train <- training(splits)$churn
test_data <- testing(splits) %>% select(-churn)
churn_test <- testing(splits)$churn

#Analysis, finding k, and determining accuracy

#finding k
k = 1:sqrt(nrow(train_data))
accu_k = rep(x = 0, times= length(k))

calc_class_acc  = function(actual, prediction) {
  mean(actual == prediction)
}

#defining k = 8 and determining how accuracy 
calc_class_acc(actual = churn_test, 
               prediction = knn(train = train_data,
                       test = test_data,
                       k = 8, 
                       cl = churn_train)
)

for (t in seq_along(k)) {
  pred = knn(train = train_data,
             test = test_data,
             cl = churn_train,
             k = k[t])
  accu_k[t] = calc_class_acc(churn_train, pred)
}

plot(accu_k, type = "b", col = "red", 
     xlab = "k neighbors", ylab = "Accuracy Rate", 
     main = "(Test) Accuracy Rate for k = 1 to 81")

abline(h = max(accu_k), col = "blue")

best_k <- max(accu_k)

final_knn_prob <- knn(train = train_data,
                 test = test_data,
                 k = 68, 
                 cl = churn_train,
                 prob = TRUE)

conf_matrix <- table(final_knn_prob, churn_test)
conf_matrix
summary(confusionMatrix(final_knn_prob))


roc(final_knn_prob, attributes(final_knn_prob)$prob, plot = TRUE, legacy.axes = TRUE, percent = TRUE, 
    xlab = "False Positive Percent", ylab = "True Positive Percent", print.auc = TRUE,print.auc.y = 95)
