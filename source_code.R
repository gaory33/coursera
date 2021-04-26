#install.packages("lattice")
library(lattice)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("caret")
library(caret)
#install.packages("corrplot")
library(corrplot)

#data load

train_data <- read.csv("./pml-training.csv", na.strings = c("", " ", NA))
test_data <- read.csv("./pml-testing.csv", na.strings = c("", " ", NA))
set.seed(0105)

head(train_data)
summary(train_data)

#data cleansing & preprocessing

train_data <- train_data[train_data$new_window=='no',]
colSums(is.na(train_data))

train_data <- train_data[!colSums(is.na(train_data)|train_data ==0) == nrow(train_data)]
colSums(train_data)
head(train_data)
dim(train_data)

test_data <- test_data[test_data$new_window=='no',]
test_data <- test_data[!colSums(is.na(test_data)|test_data ==0) == nrow(test_data)]
test_data
## dataset split : train(75%) / val(25%)

train_idx <- sample(1:nrow(train_data), size=0.75*nrow(train_data), replace=F)
val_idx <- (-train_idx)

train_data <- train_data[train_idx,]
train_data <- subset(train_data, select=-c(X))
val_data <- train_data[val_idx,]
test_data <- subset(test_data, select=-c(X,problem_id))


Y_train <- train_data$classe
X_train <- subset(train_data, select=-c(classe))
Y_val <- val_data$classe
X_val <- subset(val_data, select=-c(classe))

## model 1) Decision_Tree

library(rpart)
#install.packages("rattle")
library(rattle)

decision_tree <- rpart(classe ~ ., data=train_data, method="class")
fancyRpartPlot(decision_tree)

val_decision_tree <- predict(decision_tree, val_data, type = "class")
confusion_matrix_tree <- confusionMatrix(val_decision_tree, val_data$classe)
confusion_matrix_tree

## model 2) RandomForest

controlRF <- trainControl(method="cv", number=3, verboseIter=FALSE)
randomforest <- train(classe~., data=train_data, method="rf", trControl=controlRF)
randomforest$finalModel

val_rf <- predict(randomforest, val_data)
confusion_matrix_rf <- confusionMatrix(val_rf, val_data$classe)
confusion_matrix_rf

## model 3) Gradient Boosting Tree

controlGB <- trainControl(method="cv", number=3, verboseIter=F)
GB <- train(classe~., data=train_data, method="gbm", trControl = controlGB, tuneLength = 5, verbose = F)
GB$finalModel

val_GB <- predict(GB, val_data)
confusion_matrix_GB <- confusionMatrix(val_GB, val_data$classe)
confusion_matrix_GB

## model summary

plot(decision_tree)
plot(randomforest)
plot(GB)

## test-set predict

test_predict <- predict(randomforest, test_data)
test_predict
