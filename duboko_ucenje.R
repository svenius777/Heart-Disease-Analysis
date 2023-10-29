#install.packages("keras")
#install.packages("caret")
library(keras)
library(caret)
require(caret)


#install.packages("tensorflow")

library(tensorflow)

dataset=read.csv("C:/FAKS/IS2/HeartFailureSeminar/heart_failure.csv")

DEATH_EVENT_faktor=factor(dataset$DEATH_EVENT)

library(dplyr)
dataset=dataset %>% mutate(DEATH_EVENT=DEATH_EVENT_faktor)


validation_index<-createDataPartition(dataset$DEATH_EVENT, p =0.80, list = FALSE)
# Uzimamo 20% podataka za skup za testiranje
dataset_test<-dataset[-validation_index, ]

# Uzimamo 80% podataka za skup za treniranje
dataset_train<-dataset[validation_index, ]

# dimenzije dataseta
dim(dataset)

# Run algorithms using 10-fold cross validation
control<-trainControl(method = "cv", number = 10)
metric<-"Accuracy"

#install.packages("kernlab")
#install.packages("randomForest")

# Linear Discriminant Analysis (LDA)  
#install.packages('e1071')
library(e1071)
set.seed(7)
fit.lda <- caret::train(DEATH_EVENT~., data=dataset_train, method="lda", metric=metric, trControl=control)

# Classification and Regression Trees (CART)
set.seed(7)
fit.cart <- caret::train(DEATH_EVENT~., data=dataset_train, method="rpart", metric=metric, trControl=control)

# k-Nearest Neighbors (kNN)
set.seed(7)
fit.knn <- caret::train(DEATH_EVENT~., data=dataset_train, method="knn", metric=metric, trControl=control)

library(kernlab)
# Support Vector Machines (SVM)
set.seed(7)
fit.svm <- caret::train(DEATH_EVENT~., data=dataset_train, method="svmRadial", metric=metric, trControl=control)

library(randomForest)
# Random Forest (RF)
set.seed(7)
fit.rf <- caret::train(DEATH_EVENT~., data=dataset_train, method="rf", metric=metric, trControl=control)

#ODABIR NAJBOLJEG MODELA
results <- resamples(list(lda=fit.lda,cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)

# Graf usporedbe točnosti modela
dotplot(results)

# Sažetak našeg najboljeg modela - RF
print(fit.rf)


predictions<-predict(fit.rf, dataset_test)
confusionMatrix(predictions, dataset_test$DEATH_EVENT)

predictions2<-predict(fit.lda, dataset_test)
confusionMatrix(predictions2, dataset_test$DEATH_EVENT)
