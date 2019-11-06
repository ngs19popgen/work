library("e1071")

# Read in data with k-means and hdbscan clusters
class_data <- read.table("classification_data.txt", header=T, sep="\t")[,-(1:2)]

# SVM
# Create training and testing sets
library(caTools) 
set.seed(123) 
split = sample.split(class_data$POP, SplitRatio = 0.75) 
training_set = subset(class_data, split == TRUE) 
test_set = subset(class_data, split == FALSE) 

# Run SVM on training set
svm_model <- svm(POP ~ ., data=training_set)
summary(svm_model)
plot(svm_model, class_data)

# Run SVM on testing set
pred <- predict(svm_model, test_set[,-21])
pred_table <- table(test_set[, 21], pred)
sum(diag(pred_table))/sum(pred_table)



# Random forest classification
library(caret)
library(randomForest)

# Create training and testing sets
set.seed(123)
set_index <- createDataPartition(class_data$hdb_cluster, p=.8, list=F)
training_set <- class_data[set_index,]
test_set <- class_data[-set_index,]
summary(training_set)
summary(test_set)

# Run Random Forest on training set
rf_model <- randomForest(formula = hdb_cluster ~ PC1 + PC2 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22,
                         data = training_set, ntree=500)
print(rf_model)
plot(rf_model)

# Run Random Forest on training and testing set 
rf_oob_comp <- randomForest(formula = hdb_cluster ~ PC1 + PC2 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22,
                            data= training_set, xtest = test_set[,3:22], ytest = test_set[,25])
print(rf_oob_comp)

# extract OOB & validation errors
oob <- sqrt(rf_oob_comp$err.rate)
validation <- sqrt(rf_oob_comp$test$err.rate)

# compare error rates
tibble::tibble(
  `Out of Bag Error` = oob,
  `Test error` = validation,
  ntrees = 1:rf_oob_comp$ntree
) %>%
  gather(Metric, err.rate, -ntrees) %>%
  ggplot(aes(ntrees, err.rate, color = Metric)) +
  geom_line() +
  scale_y_continuous() +
  xlab("Number of trees")

