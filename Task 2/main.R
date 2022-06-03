library(readr)
library(dplyr)
library(ggplot2)
library(gridExtra)s
library(clue)

data <- read_csv("~/TSF-GRIP-Data-Science-June-2022/Task 2/Iris.csv")

#4 plots
extract_legend <- function(my_ggp) {
  step1 <- ggplot_gtable(ggplot_build(my_ggp))
  step2 <- which(sapply(step1$grobs, function(x) x$name) == "guide-box")
  step3 <- step1$grobs[[step2]]
  return(step3)
}
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
  return (rbind(test, train))
}

plot1 <- ggplot(data, aes(PetalLengthCm, SepalLengthCm)) + geom_point(shape = 1) 
plot2 <- ggplot(data, aes(PetalWidthCm, SepalWidthCm)) + geom_point(shape = 1) 
plot3 <- ggplot(data, aes(PetalLengthCm, SepalWidthCm)) + geom_point(shape = 1)
plot4 <- ggplot(data, aes(PetalWidthCm, SepalLengthCm)) + geom_point(shape = 1)

grid.arrange(arrangeGrob(plot1, plot2, plot3, plot4, ncol = 2),
             legend, nrow = 2, heights = c(10, 2))

#Area plot
area_df <- data.frame(matrix(nrow = 150, ncol = 2))
colnames(area_df) <- c("sepal", "petal")

area_df$sepal <- (data[2] * data[3])$SepalLengthCm
area_df$petal <- (data[4] * data[5])$PetalLengthCm

area_df$species = data$Species

ggplot(area_df, aes(sepal, petal, col = species)) + geom_point(shape = 1)

#Train Test Model
train_test <- sample_test(data, 80)

train_data <- as.data.frame(train_test[1])
test_data <- as.data.frame(train_test[2])

testtrain_data <- combine_train_test(train_data, test_data)

# K-means Modeling
df <- data.frame(matrix(nrow = 0, ncol = 2))
colnames(df) <- c("n", "mean")

for (i in 1:10){
  tm <- kmeans(train_data[2:5], i)
  mean <- tm[4]$withinss
  df[i,] = c(i,mean)
}
ggplot(df, aes(n, mean)) + geom_line() + geom_point()

model <- kmeans(train_data[2:5], 3)

sol <- cl_predict(model, test_data[2:5])
test_data$predicted_species <- sol

