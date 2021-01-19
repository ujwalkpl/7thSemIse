library("MASS")
library("survival")
library("ipred")

data("BreastCancer", package = "mlbench")
mod <- bagging(Class ~., coob=TRUE)
print(mod)

print(head(BreastCancer))
p <- predict(mod,BreastCancer)
tab <- table(p,BreastCancer$Class)

library(ROCR)
pred <- predict(mod,BreastCancer,type = 'prob')
head(pred[,1])
pred <- prediction(pred[,2],BreastCancer$Class)
eval <- performance(pred,"acc")
plot(eval)
abline(a=0,b=1)

summary(BreastCancer)
BreastCancer$Bare.nuclei = ifelse(is.null(BreastCancer$Bare.nuclei),mean(BreastCancer$Bare.nuclei,na.rm = TRUE),BreastCancer$Bare.nuclei)
# Fit the model on the training set
summary(BreastCancer)
set.seed(123)
model <- train(
  Class ~., data = BreastCancer, method = "xgbTree",
  trControl = trainControl("cv", number = 10)
)
# Best tuning parameter
model$bestTune
pred2 <- predict(model,BreastCancer,type = 'prob')
pred2 <- prediction(pred2[,2],BreastCancer$Class)
eval2 <- performance(pred2,"acc")
plot(eval2)

abline(a=0,b=1)

