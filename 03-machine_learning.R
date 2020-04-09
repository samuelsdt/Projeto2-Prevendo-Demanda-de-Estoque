#Esse script tem como objetivo gerar os modelos de machine learning, serão testados alguns algoritmos 
#bem como alguns parâmetros dentro desses algoritmos

library(data.table)

df <- fread("data_work.csv", dec=",")

#normalizando as colunas
colNorm <- c("qt_sem","rank_prod_loja","rank_loja","rank_prod","total_agencia","Demanda_anterior","total_loja","total_cli","total_prod","total_rota","media_agencia","nreg","qt_sem_loja","media_cli","media_prod","media_rota")

normaliza <- function (col)
{
  mx <- max(df[, col,with=F])
  mn <- min(df[, col,with=F])
  
  df[,(col) := (unlist(lapply(df[,col,with=F],function(x){(x - mn) / (mx - mn)})))]
}

for (i in colNorm)
{
  normaliza(i)
}

#convertendo Canal_ID em fator
df$Canal_ID = as.factor(df$Canal_ID)

#separando em treino e teste
linhas <- sample(nrow(df),nrow(df)*0.70)
train <- df[linhas,]
test <- df[-linhas, ]
rm(df)
gc()


#aplicando os algoritmos de machine learning, a métrica de validação será RMSLE
#pois é a mesma métrica da competição do Kaggle

library(MLmetrics)

#começando com regressão linear
lr <- lm(Demanda_uni_equil ~ .,data=train)
p <- predict(lr,test)
p[which(p<0)] <- 0
RMSLE(p,test$Demanda_uni_equil)
#RMSLE = 0.5442383

#testando com variáveis mais relevantes
library(caret)
varImp(lr, scale = FALSE)

lr <- lm(Demanda_uni_equil ~ total_loja + rank_prod_loja + media_agencia + media_cli + Canal_ID + media_rota + Demanda_anterior + qt_sem_loja + qt_sem + total_cli,data=train)
p <- predict(lr,test)
p[which(p<0)] <- 0
RMSLE(p,test$Demanda_uni_equil)
#RMSLE = 0.535709
#uma pequena melhora no RMSLE usando apenas variáveis selecionadas

#Random Forest
#o random forest, por ser um algoritmo que exige muita capacidade computacional,
#será usado um subsample com 1.500.000 linhas da base de treino para treinameento do modelo
require(randomForest)

set.seed(123)
train1 <- train[sample(nrow(train),500000),]
rf <- randomForest(Demanda_uni_equil ~ .,data=train1, ntree=10)
p <- predict(rf,test)
p[which(p<0)] <- 0
RMSLE(p,test$Demanda_uni_equil)
#RMSLE = 0.4961283

#devido ao demora no treinamento da base de 1.000.000 de registros, será testado 
#se o algoritmo mantém a mesma perfomance treinando uma base de 100.000 registros, caso sim, os próximos experimentos
#usando o random forest podem ser feitos em uma base menor
train1 <- train[sample(nrow(train),200000),]
rf1 <- randomForest(Demanda_uni_equil ~ .,data=train1, ntree=10, importance=T)
p <- predict(rf1,test)
p[which(p<0)] <- 0
RMSLE(p,test$Demanda_uni_equil)
#RMSLE = 0.5003127

#a base com 500.000 registros gerou um modelo com menor erro, porém o tempo de processamento foi
#muito maior, enquanto a diferença do RMSLE foi apenas 0.0041, portanto, será usado a base de 200.000 para os próximos experimentos

#verificando a importância das váriaveis 
varImp(rf1)
#ordem de importâcia das variaveis para o modelo: total_loja,total_agencia,rank_prod_loja,media_rota,Demanda_anterior,total_prod,qt_sem_loja,
#qt_sem,media_cli,total_rota,rank_loja,media_agencia,media_prod,total_cli,nreg,Canal_ID,rank_prod,Semana

#treinando o modelo sem alguns atributos para identificar a tendência do RMSLE
rf2 <- randomForest(Demanda_uni_equil ~ total_loja + total_agencia + rank_prod_loja + media_rota + Demanda_anterior + total_prod + qt_sem_loja + qt_sem + media_cli + total_rota + rank_loja + media_agencia, data=train1, ntree=10)
p <- predict(rf2,test)
p[which(p<0)] <- 0
RMSLE(p,test$Demanda_uni_equil)
#RMSLE = 0.5022999
#o RMSLE piorou ao tirar atributos, o modelo com todos os atributos apresenta um erro menor

#aumentando o número de árvores
rf3 <- randomForest(Demanda_uni_equil ~ .,data=train1, ntree=50)
p <- predict(rf3,test)
p[which(p<0)] <- 0
RMSLE(p,test$Demanda_uni_equil)
#RMSLE = 0.4856252

rf4 <- randomForest(Demanda_uni_equil ~ .,data=train1, ntree=100)
p <- predict(rf4,test)
p[which(p<0)] <- 0
RMSLE(p,test$Demanda_uni_equil)
#RMSLE = 0.4837306

#xgboost
library(xgboost)
#criando as matrizes de treino e testes, inicialmente será treinado apenas 5 rounds para avaliar os parâmetros com
#melhor performance
dtrain <- xgb.DMatrix(data.matrix(train[,-c("Demanda_uni_equil")]), label = train$Demanda_uni_equil)
dtest <- xgb.DMatrix(data.matrix(test[,-c("Demanda_uni_equil")]), label = test$Demanda_uni_equil)

#criando uma metrca de avaliação customizada
evalerror <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  preds[which(preds<0)] <- 0
  err <- RMSLE(preds,labels)
  return(list(metric = "RMSLE", value = err))
}

#colocando em uma watchlist
watchlist <- list(train = dtrain, eval = dtest)
#colocando a metrica  nos parâmetros de avaliação
param <- list(eval_metric = evalerror)

xgb_model <- xgb.train(param, dtrain, nrounds = 5, watchlist)
#RMSLE = 0.501818

#verificando as variáveis mais relevantes
xgb.importance(model=xgb_model)
#ordem de importância das variáveis: total_loja,Demanda_anterior,rank_prod_loja,media_cli,total_cli,Canal_ID,nreg,media_prod,media_rota,qt_sem_loja,total_rota,total_agencia,rank_loja,media_agencia,qt_sem,total_prod

#testando o modelo com as variáveis mais relevantes
dtrain <- xgb.DMatrix(data.matrix(train[,c("total_loja","Demanda_anterior","rank_prod_loja","media_cli","total_cli","Canal_ID","nreg","media_prod","media_rota","qt_sem_loja","total_rota","total_agencia")]), label = train$Demanda_uni_equil)
dtest <- xgb.DMatrix(data.matrix(test[,c("total_loja","Demanda_anterior","rank_prod_loja","media_cli","total_cli","Canal_ID","nreg","media_prod","media_rota","qt_sem_loja","total_rota","total_agencia")]), label = test$Demanda_uni_equil)

watchlist <- list(train = dtrain, eval = dtest)
xgb_model1 <- xgb.train(param, dtrain, nrounds = 5, watchlist)
#RMSLE = 0.503109 

#com menos atributos, o modelo teve uma taxa de erro maior

#testando outros parâmetros, voltando a utilizar a base com todos os atributos
dtrain <- xgb.DMatrix(data.matrix(train[,-c("Demanda_uni_equil")]), label = train$Demanda_uni_equil)
dtest <- xgb.DMatrix(data.matrix(test[,-c("Demanda_uni_equil")]), label = test$Demanda_uni_equil)
watchlist <- list(train = dtrain, eval = dtest)
param <- list(eval_metric = evalerror, max_depth=15, min_child_weight=5)
xgb_model2 <- xgb.train(param, dtrain, nrounds = 5, watchlist)
#RMSLE = 0.463918  

param <- list(eval_metric = evalerror, max_depth=18, min_child_weight=6)
xgb_model3 <- xgb.train(param, dtrain, nrounds = 5, watchlist)
#RMSLE = 0.462125 

param <- list(eval_metric = evalerror, max_depth=21, min_child_weight=7)
xgb_model4 <- xgb.train(param, dtrain, nrounds = 5, watchlist)
#RMSLE = 0.462127 

#entre todos os testes efetuados, o melhor RMSLE foi o modelo 3, porém o RMSLE do modelo 4 ficou muito próximo
#serão testados esses 2 modelos com uma taxa de aprendizado mais baixa (0.1) e 20 rounds de treino
param <- list(eval_metric = evalerror, eta=0.1, max_depth=18, min_child_weight=6)
xgb_model3 <- xgb.train(param, dtrain, nrounds = 20, watchlist)
#RMSLE = 0.458443 (depois de 19 rounds), provavelmente a partir do 19º round o modelo estava começando um processo de overfitting

param <- list(eval_metric = evalerror, eta=0.1, max_depth=21, min_child_weight=7)
xgb_model4 <- xgb.train(param, dtrain, nrounds = 20, watchlist)
#RMSLE = 0.457431 (depois de 19 rounds), assim como no modelo 3, o 19º round teve o menor erro na base de testes
#entre os 2 modelos, o modelo 4 apresentou a melhor perfomance

#o modelo será novamente treinado em 19 rounds (melhor performance na base de testes)
xgb_model4 <- xgb.train(param, dtrain, nrounds = 19, watchlist)

#salvando o modelo
xgb.save(xgb_model4,"model")