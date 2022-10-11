library(dplyr)

churn_cleaning <- function(data){
  data %>% 
    mutate_if(is.character,as.factor) %>%
    #Simple imputation applied to all missing data using the mean for each column.
    replace_na(
      list(
        tenure = mean(data$tenure, na.rm =TRUE),
        yearly_equip_failure = mean(data$yearly_equip_failure, na.rm = TRUE) %>% as.integer(),
        contacts = mean(data$contacts, na.rm = TRUE) %>% as.integer(),
        bandwidth_gb_year = mean(data$bandwidth_gb_year, na.rm =TRUE),
        age = mean(data$age, na.rm =TRUE)%>% as.integer(),
        income = mean(data$income, na.rm = TRUE) ,
        children = median(data$children, na.rm = TRUE)%>% as.integer()
      )
    )%>%
    #Removing outliers from data; the outliers were found by looking for numeric columns with values 3 sigma mutate(
    mutate(
      across(where(is.numeric),
             ~ifelse(
               (.x > mean(.x, na.rm=TRUE)+abs(3*sd(.x,na.rm =TRUE)) ) |
                 (.x < mean(.x, na.rm=TRUE)-abs(3*sd(.x,na.rm =TRUE))),
               "outlier",
               "not outlier"
             ),
             .names = "outlier_{.col}")
    ) %>%
    #filter out all outlier entries
    filter(if_all(starts_with("outlier"), ~ . == "not outlier")) %>%
    #Looks for variables that have inputs that occur less than .5% and lumps them together in "other" category.
    mutate(
      across(
        where(is.factor),
        ~fct_lump(.x,prop = 0.005)
      )
    ) %>%
    select(everything(), -starts_with("outlier"), -population, -job,-customer_id,-interaction,-city)
  }


