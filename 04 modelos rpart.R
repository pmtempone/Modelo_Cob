#modelo base de prueba

library(rpart)
library(rpart.plot)
library(party)
library(caret)
library(rattle)
library(pROC)
library(rpart.utils)
library(randomForest)
library(funModeling)
library(dplyr)
library(foreach)
library(arules)

#rpart modelo base # Tue May 23 13:09:34 2017 ------------------------------

train_barrido$target <- as.numeric(as.character(train_barrido$target))
train_barrido$quincena <- factor(ifelse(train_barrido$dia_mes<15,"1Q","2Q"))
train <- train_barrido %>% filter(id_mes =='2017-01')
validation <- train_barrido %>% filter(id_mes=='2017-02')
test_barrido$quincena <- factor(ifelse(test_barrido$dia_mes<15,"1Q","2Q"))

fit.rpart.m1 <- rpart(target ~ .,data = train[,c(2,3,5:16)],control = rpart.control(cp=0.01,maxdepth = 20),parms = list(split='gini'))

fancyRpartPlot(fit.rpart.m1)



pred.rpart <- predict(fit.rpart.m1,validation)

pred.rpart <- ifelse(pred.rpart >= .10, 1,0)

table("predicho"=pred.rpart, "observado"=validation$target)



g <- roc(target ~ pred.rpart, data = validation)
plot(g, col="red")
g

probpred.training <- predict(fit.rpart.m1)

pred.train.rpart <- ifelse(probpred.training >= .10, 1,0)

g.training <- roc(target ~ pred.train.rpart, data = train)
g.training


lines(g.training, col="blue") 

pred.rpart.test <- predict(fit.rpart.m1,test_barrido)

pred.rpart.test <- ifelse(pred.rpart.test >= .10, 1,0)

table("predicho"=pred.rpart.test, "observado"=test_barrido$target)


g.testing <- roc(target ~ pred.rpart.test, data = test_barrido)
g.testing
lines(g.testing, col="green")

legend("bottomright", c(paste("prueba",round(g$auc,2),sep = " "),paste("entrenamiento",round(g.training$auc,2),sep = " "),paste("testing",round(g.testing$auc,2),sep = " ")), col = c("blue", "red"), lty = 1,cex = 0.7 )

#AUC 75% validation
#testing 78%

#rpart modelo con mas parametros # Tue May 23 15:05:15 2017 ------------------------------


fit.rpart.m2 <- rpart(target ~ .,data = train[,c(2,3,5:16)],control = rpart.control(cp=0.001,maxdepth = 20),parms = list(split='gini'))

fancyRpartPlot(fit.rpart.m2)



pred.rpart <- predict(fit.rpart.m2,validation)

pred.rpart <- ifelse(pred.rpart >= .10, 1,0)

table("predicho"=pred.rpart, "observado"=validation$target)



g <- roc(target ~ pred.rpart, data = validation)
plot(g, col="red")
g

probpred.training <- predict(fit.rpart.m2)

pred.train.rpart <- ifelse(probpred.training >= .10, 1,0)

g.training <- roc(target ~ pred.train.rpart, data = train)
g.training


lines(g.training, col="blue") 

pred.rpart.test <- predict(fit.rpart.m2,test_barrido)

pred.rpart.test <- ifelse(pred.rpart.test >= .10, 1,0)

table("predicho"=pred.rpart.test, "observado"=test_barrido$target)
prop.table(table("predicho"=pred.rpart.test, "observado"=test_barrido$target),2)


g.testing <- roc(target ~ pred.rpart.test, data = test_barrido)
g.testing
lines(g.testing, col="green")

legend("bottomright", c(paste("prueba",round(g$auc,2),sep = " "),paste("entrenamiento",round(g.training$auc,2),sep = " "),paste("testing",round(g.testing$auc,2),sep = " ")), col = c("blue", "red"), lty = 1,cex = 0.7 )

