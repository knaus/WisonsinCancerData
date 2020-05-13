library(tidyverse)
library(cluster)
library(factoextra)
library(caret)

# First pass data preparation
# names <- c('id_number', 'clump_thickness', 'uniform_cell_size', 'uniform_cell_shape', 'marginal_adhesion', 'single_epithelial_cell_size', 'bare_nuclei', 'bland_chromatin', 'normal_nucleoli', 'mitoses','diagnosis')
# data <- read.csv('breast-cancer-wisconsin.data',header=FALSE,col.names=names)
# # Data cleaning
# #code an outcome column, remove the id_number and duplicate diagnosis column
# data$outcome[data$diagnosis==4] = 1 # malignant
# data$outcome[data$diagnosis==2] = 0 # benign
# data$outcome = as.integer(data$outcome)
# length(data)
# data %<>% select(-c(id_number,diagnosis,bare_nuclei))
# #check data size, types
# length(data)
# glimpse(data)
# 
# # Write clean data for app
# write.csv(data, file = 'breast-cancer-wisconsin-cleaned.csv', row.names=FALSE)

# Get data 
data <- read.csv('breast-cancer-wisconsin-cleaned.csv', header=TRUE)

# Model fitting

#split into train/test
set.seed(42)
trainIndex <- createDataPartition(data$outcome, p = .8, list = FALSE, times = 1)
traind <- data[ trainIndex, ]
testd <- data[ -trainIndex, ]

#fit a Random Forest model
fit_rf <- train(as.factor(outcome) ~ ., data = traind, metric = "Accuracy")
fit_rf$finalModel
#fit metrics
train_predict<- predict(fit_rf,traind)
test_predict<- predict(fit_rf,testd)
results_train <- confusionMatrix(train_predict, as.factor(traind$outcome))
results_test <- confusionMatrix(test_predict, as.factor(testd$outcome))
acc_train <- round(as.numeric(results_train$overall[1])*100,2)
acc_test <- round(as.numeric(results_test$overall[1])*100,2)

#use variable importance to simplify the model to reduce overfitting
varImportance <- varImp(fit_rf, scale = FALSE)
varImportanceScores <- data.frame(varImportance$importance)
varImportanceScores <- data.frame(names = row.names(varImportanceScores), var_imp_scores = varImportanceScores$Overall)
varImportanceScores <- arrange(varImportanceScores,desc(var_imp_scores))

#compute kmeans
train_features <- traind %>% select(- outcome)
k2 <- kmeans(train_features, centers = 3, nstart = 25)
fviz_cluster(k2, data = traind)
#visualize optimal nr of clusters mimizing within cluster sum of squares metric
set.seed(123)
fviz_nbclust(train_features, kmeans, method = "wss")

#rename columns from 1 to Cluster 1
rename_cluster_col = function(df){
  for (i in 2:ncol(df)){
    colnames(df)[i] = paste0('cluster',(i-1))}
  df}

#summarize features per cluster
summary_cluster <- traind %>%
  mutate(Cluster = k2$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean") %>%
  gather(variable,value, -Cluster ) %>%
  spread (Cluster,value) %>%
  rename_cluster_col()



