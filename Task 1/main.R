library(readr)
library(dplyr)
library(ggplot2)

sample_test <- function(dataframe, percent){
  sample_size = floor((percent/100)*nrow(dataframe))
  set.seed(123)
  train_index <- sample(seq_len(nrow(dataframe)), size = sample_size)
  
  train <- dataframe[train_index, ]
  test <- dataframe[-train_index, ]
  
  return(list(train, test))
}

combine_train_test <- function(train, test){
  train$type = 'train'
  test$type = 'test'
  View(rbind(test_data, train_data))
  return (rbind(test, train))
}


data <- read_csv("~/TSF-GRIP Data Science/Task 1/data.csv")

ggplot(data, aes(Hours, Scores)) + scale_color_manual(values = "darkslategray3") + geom_point(shape = 1) + labs(fill = "Type" ,title = "Hours studied VS Score received", x = "Hours", y = "Scores") 

train_test <- sample_test(data, 80)
train_data <- as.data.frame(train_test[1])
test_data <- as.data.frame(train_test[2])

testtrain_data <- combine_train_test(train_data, test_data)

model <- lm(Scores~Hours, train_data)

redicted_data <- test_data %>% mutate(predicted_scores = predict(model, test_data)) 
head(predicted_data)

ggplot(testtrain_data, aes(Hours, Scores, col = type)) + scale_color_manual(values = c("brown1", "cyan4")) + geom_point(shape = 1) + geom_smooth(method= "lm", color= "darkslategray", se=F) + labs(fill = "Type" ,title = "Hours studied VS Score received Prediction Line", x = "Hours", y = "Scores")

