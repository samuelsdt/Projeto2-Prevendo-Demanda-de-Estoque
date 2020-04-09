#Esse script tem como objetivo a aplicação do algoritmo de machine learning na base de testes do Kaggle
#os atributos total_loja, qt_sem_loja, rank_prod_loja, total_cli, media_cli, rank_loja, total_prod, media_prod, 
#rank_prod, total_rota, media_rota, total_agencia e media_agencia foram calculados na base de treino com os dados
#das 5 semanas anteriores (semana 3 a 7), portanto na base de teste também serão usados os dados das 5 semanas
#anteriores (semana 5 a 9), já o atributo demanda anterior, para a semana 10 será usado os valores da Semana 9,
#já para a semana 11, será usado os valores previtos da semana 10, portanto, a predição será aplicada primeiro na
#semana 10 e depois na semana 11, separadamente. O atributo nreg refere-se ao número de registros do produto na 
#semana e qt_sem refere-se as 5 semanas anteriores mais a semana atual

library(data.table)
library(dplyr)

df <- fread("train.csv")
df <- df[df$Semana>4,]

test <- fread("test.csv")

df_ant <- df %>%
  filter(Semana==9) %>%
  select(Cliente_ID,Producto_ID,Demanda_uni_equil) %>% 
  group_by(Cliente_ID,Producto_ID) %>%
  summarise(Demanda_anterior = sum(Demanda_uni_equil))

test <- left_join(test,df_ant,by=c("Cliente_ID","Producto_ID"))

test[(test$Semana==11) | is.na(test$Demanda_anterior),"Demanda_anterior"] <- 0
rm(df_ant)
gc()

nreg_sem <- test %>%
  select(Semana,Producto_ID) %>%
  group_by(Semana,Producto_ID) %>% 
  summarise(nreg = n())

test <- left_join(test,nreg_sem,by=c("Semana","Producto_ID"))
rm(nreg_sem)
gc()

p <- df %>%
  select(Semana, Producto_ID)%>%
  group_by(Semana, Producto_ID) %>%
  summarise(cnt_sem=n())

p2 <- test %>%
  select(Semana, Producto_ID)%>%
  group_by(Semana, Producto_ID) %>%
  summarise(cnt_sem=n())

p <- rbind(as.data.frame(p),as.data.frame(p2))
rm(p2)
gc()

d <- p %>%
  filter(Semana<11) %>%
  select(Producto_ID) %>%
  group_by(Producto_ID) %>%
  summarise(qt_sem=n()) %>%
  mutate(Semana=10)

d2 <- p %>%
  filter(Semana>5) %>%
  select(Producto_ID) %>%
  group_by(Producto_ID) %>%
  summarise(qt_sem=n()) %>%
  mutate(Semana=11)

d <- rbind(d,d2)
rm(p,d2)
gc()

test <- left_join(test,d,by=c("Producto_ID","Semana"))
rm(d)
gc()

prod <- df %>%
  select(Cliente_ID, Producto_ID, Demanda_uni_equil) %>%
  group_by(Cliente_ID, Producto_ID) %>%
  summarise(total_loja = sum(Demanda_uni_equil), qt=n()) %>%
  mutate(total_loja = total_loja/qt)

prod <- prod[order(prod$Cliente_ID,-prod$total_loja),]

prod <- prod %>%
  group_by(Cliente_ID) %>%
  mutate(rank = order(order(total_loja, decreasing=TRUE))) 

prod$chave <- unlist(lapply(c(1:nrow(prod)),function(x) paste(prod$Cliente_ID[x],prod$total_loja[x],sep="#",collapse="")))

rank <- prod %>%
  select(chave, rank) %>%
  group_by(chave) %>%
  summarise(rnk = min(rank))

prod <- left_join(prod,rank,by="chave")

prod <- prod[,-c(5,6)]
names(prod)[4:5] <- c("qt_sem_loja","rank_prod_loja")

test <- left_join(test,prod,by=c("Cliente_ID","Producto_ID"))
rm(prod,rank)
gc()

lojas <- df %>%
  select(Cliente_ID,Demanda_uni_equil)%>%
  group_by(Cliente_ID) %>%
  summarise(total_cli = sum(Demanda_uni_equil), qt=n()) %>%
  mutate(media_cli = total_cli/qt)

lojas <- lojas[order(-lojas$total_cli),]

lojas$rank_loja =rank(-lojas$total_cli,ties.method ="min")
lojas$qt <- NULL
test <- left_join(test,lojas,by="Cliente_ID")
rm(lojas)
gc()

prods <- df %>%
  select(Producto_ID,Demanda_uni_equil)%>%
  group_by(Producto_ID) %>%
  summarise(total_prod = sum(Demanda_uni_equil), qt=n()) %>%
  mutate(media_prod = total_prod/qt)

prods <- prods[order(-prods$total_prod),]

prods$rank_prod <- rank(-prods$total_prod,ties.method ="min")

prods$qt <- NULL
test <- left_join(test,prods,by="Producto_ID")

rm(prods)
gc()

rota <- df %>%
  select(Ruta_SAK, Producto_ID, Demanda_uni_equil) %>%
  group_by(Ruta_SAK,Producto_ID) %>%
  summarise(total_rota = sum(Demanda_uni_equil), qt=n())%>%
  mutate(media_rota= total_rota/qt)

rota$qt <- NULL
test <- left_join(test,rota,by=c("Ruta_SAK","Producto_ID"))
rm(rota)
gc()

agencia <- df %>%
  select(Agencia_ID, Producto_ID, Demanda_uni_equil) %>%
  group_by(Agencia_ID, Producto_ID) %>%
  summarise(total_agencia = sum(Demanda_uni_equil), qt=n())%>%
  mutate(media_agencia= total_agencia/qt)

agencia$qt <- NULL
test <- left_join(test,agencia,by=c("Agencia_ID","Producto_ID"))
rm(agencia, df)
gc()

#removendo os valores não disponiveis (usando os mesmos valores default da base de treino)
def_val_nas <- c(total_loja=0,qt_sem_loja=0,rank_prod_loja=202,total_cli=0,media_cli=0,rank_loja=861297,total_prod=0,media_prod=0,rank_prod=1652,total_rota=0,media_rota=0,total_agencia=0,media_agencia=0)

for (d in c(1:length(def_val_nas)))
{
  
  col <- names(def_val_nas[d])
  i <- unname(def_val_nas[d])
  test[is.na(test[, col]), col] <- i
} 

#no processo de normalização, será utilizado os minimos e maximos da base de treino
df <- fread("data_work.csv", dec=",")
colNorm <- c("Demanda_anterior","nreg","qt_sem","total_loja","qt_sem_loja","rank_prod_loja","total_cli","media_cli","rank_loja","total_prod","media_prod","rank_prod","total_rota","media_rota","total_agencia","media_agencia")

test <- as.data.table(test)

for (col in colNorm)
{
  mx <- max(df[, col,with=F])
  mn <- min(df[, col,with=F])
  
  test[,(col) := (unlist(lapply(test[,col,with=F],function(x){(x - mn) / (mx - mn)})))]
}

mn_dem_ant <- min(df$Demanda_anterior)
mx_dem_ant <- max(df$Demanda_anterior)

rm(df)
gc()

test$Canal_ID <- as.factor(test$Canal_ID)

#lendo o modelo
library(xgboost)
mdl <- xgb.load("model")

#aplicando o modelo na semana 10
ptest  <- predict(mdl, data.matrix(test[test$Semana==10,c("Semana","Canal_ID","Demanda_anterior","nreg","qt_sem","total_loja","qt_sem_loja","rank_prod_loja","total_cli","media_cli","rank_loja","total_prod","media_prod","rank_prod","total_rota","media_rota","total_agencia","media_agencia")]))
ptest[ptest<0] <- 0
test$Predicao <- 0
test[test$Semana==10,"Predicao"] <- ptest

#aplicando o modelo na semana 11
t2 <- test[test$Semana==11,]

df_ant <- test %>%
  filter(Semana==10) %>%
  select(Cliente_ID,Producto_ID,Predicao) %>% 
  group_by(Cliente_ID,Producto_ID) %>%
  summarise(Demanda_anterior = sum(Predicao))

t2$Demanda_anterior <- NULL
t2 <- left_join(t2,df_ant,by=c("Cliente_ID","Producto_ID"))
rm(df_ant)
gc()

t2[is.na(t2$Demanda_anterior),"Demanda_anterior"] <- 0 
t2$Demanda_anterior <- (unlist(lapply(t2$Demanda_anterior,function(x){(x - mn_dem_ant) / (mx_dem_ant - mn_dem_ant)})))

ptest  <- predict(mdl, data.matrix(t2[,c("Semana","Canal_ID","Demanda_anterior","nreg","qt_sem","total_loja","qt_sem_loja","rank_prod_loja","total_cli","media_cli","rank_loja","total_prod","media_prod","rank_prod","total_rota","media_rota","total_agencia","media_agencia")]))
ptest[ptest<0] <- 0
test[test$Semana==11,"Predicao"] <- ptest

#gerando o arquivo para submissão no Kaggle
sub <- test[,c("id","Predicao")]
sub$Predicao <- round(sub$Predicao,0)
names(sub)[2] <- "Demanda_uni_equil"

sub <- sub[order(sub$id),]
write.csv(sub,"submission.csv",row.names=F,sep = ",")

#ao submeter o arquivo para a competição do Kaggle, o Private Score foi de 0.49668, portanto,
#o erro foi menor que objetivo estabelecido de 0.5