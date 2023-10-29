#RIDGE
#install.packages("ISLR")
library (ISLR)
dataset=read.csv("C:/FAKS/IS2/HeartFailureSeminar/heart_failure.csv")
x<-model.matrix(DEATH_EVENT ~.,dataset )[,-1]
y<-dataset$DEATH_EVENT

#install.packages("glmnet")
library (glmnet)
grid =10^seq(10,-2, length=100)
ridge_model=glmnet(x, y, alpha=0, lambda=grid) # Treniranje Ridge modela
dim(coef(ridge_model))

cat("lambda:", ridge_model$lambda[50])
# Koeficijenti za 50. lambda, tj. lambda=11497
coef(ridge_model)[,50]
cat("L2 norma:")
sqrt(sum(coef(ridge_model)[-1,50]^2))

cat("lambda:")
ridge_model$lambda[60]
cat("koeficijenti:")
coef(ridge_model)[,60]
cat("L2 norma:", sqrt(sum(coef(ridge_model)[-1,60]^2)))

predict(ridge_model,s=50,type="coefficients")[1:13,]

set.seed(1)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]

ridge_model=glmnet(x[train,],y[train],alpha=0,lambda=grid, thresh=1e-12)
ridge.pred=predict(ridge_model,s=4,newx=x[test,])
mse_pogreska<-mean((ridge.pred-y.test)^2)
cat("MSE pogreška:", mse_pogreska)

cat("MSE pogreška:", mean((mean(y[train])-y.test)^2))

ridge.pred=predict(ridge_model,s=1e10,newx=x[test,])
cat("MSE pogreška:", mean((ridge.pred-y.test)^2))

ridge.pred=predict.glmnet (ridge_model ,s=0, newx=x[test ,])
mean((ridge.pred -y.test)^2)
lm(y~x, subset =train)
predict(ridge_model,s=0,type="coefficients") [1:13 ,]

set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
bestlambda=cv.out$lambda.min
cat("najbolji lambda je:", bestlambda)

ridge.pred=predict(ridge_model,s=bestlambda,newx=x[test,])
cat("MSE pogreška kada koristimo bestlambda:", mean((ridge.pred-y.test)^2))

out=glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlambda)[1:13,]