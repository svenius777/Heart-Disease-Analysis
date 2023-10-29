dataset=read.csv("C:/FAKS/IS2/HeartFailureSeminar/heart_failure.csv")
head(dataset)
#install.packages("rpart")
#install.packages("rpart.plot")

#install.packages('dplyr')
library(dplyr)
DEATH_EVENT_2=ifelse(dataset$DEATH_EVENT==1, "Yes", "No")
dataset=dataset %>% mutate(DEATH_EVENT=DEATH_EVENT_2)

n = nrow(dataset)
n_train=round(0.80*n)
set.seed(123)
train_indeksi <- sample(1:n, n_train)
dataset_train <- dataset[train_indeksi, ]  
dataset_test <- dataset[-train_indeksi, ]

library("rpart")
library("rpart.plot")


#anaemia_2=ifelse(dataset$anaemia==1, "Yes", "No")
#high_blood_pressure_2=ifelse(dataset$high_blood_pressure==1, "Yes", "No")
#sex_2=ifelse(dataset$sex==1, "Male", "Female")
#smoking_2=ifelse(dataset$smoking==1, "Yes", "No")

  
dataset_stablo_model = rpart(formula = DEATH_EVENT ~ .,
                             data=dataset_train,
                             method="class")
rpart.plot(x = dataset_stablo_model, yesno = 2, type = 0, extra = 0)


