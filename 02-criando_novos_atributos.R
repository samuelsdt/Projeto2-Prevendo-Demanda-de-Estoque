#Esse script tem como objetivo criar novos atributos na base de treino para o treinamento do modelo preditivo

#A partir da identificação dos dados no Script 01 e pela descrição dos mesmos, serão criados o seguintes atributos:

#Demanda_anterior: demanda de produtos na semana anterior agrupados por Cliente_ID e Producto_ID
#nreg: número de registros do produto na semana
#qt_sem: quantidade de semanas que o produto foi comercializado
#total_loja: média do produto vendido na loja (Cliente_ID) nas semanas anteriores
#qt_sem_loja: quantidade de semanas que o produto foi comercializado naquela loja específica
#rank_prod_loja: a posição do ranking do produto na loja com relação aos demais (1 = produto mais vendido da loja, 2...)
#total_cli: total geral de produtos vendidos na loja
#media_cli: média geral de produtos vendidos na loja
#rank_loja: ranking da loja com relação as demais em número de produtos vendidos
#total_prod: total geral vendido do produto
#media_prod: média geral do produto 
#rank_prod: ranking do produto com relação aos demais em número de vendas
#total_rota: total geral do produto vendido na rota (RUTA_SAK)
#media_rota: média geral do produto na rota
#total_agencia: total geral do produto na agência (Agencia_ID)
#media_agencia: média geral do produto na agência

#o atributo Demanda_anterior será criado com base na semana anterior
#o atributo nreg será criado com base na semana atual
#o atributo qt_sem será criado com base nas 5 semanas anteriores + semana atual
#os demais atributos serão criados com base nas 5 semanas anteriores

library(data.table)
library(dplyr)


df <- fread("train.csv")
df <- df[df$Semana>=8, ]

df_ant <- fread("train.csv")
df_ant <- df_ant[(df_ant$Semana==7) | (df_ant$Semana==8),]

df_ant$Semana <- df_ant$Semana + 1

df_ant <- df_ant %>%
  select(Semana,Cliente_ID,Producto_ID,Demanda_uni_equil) %>% 
  group_by(Semana,Cliente_ID,Producto_ID) %>%
  summarise(Demanda_anterior = sum(Demanda_uni_equil))

df <- left_join(df,df_ant,by=c("Semana","Cliente_ID","Producto_ID"))
rm(df_ant)
gc()

df[is.na(df$Demanda_anterior),"Demanda_anterior"] <- 0


nreg_sem <- df %>%
        select(Producto_ID) %>%
        group_by(Producto_ID) %>% 
        summarise(nreg = n())

df <- left_join(df,nreg_sem,by=c("Producto_ID"))
rm(nreg_sem)
gc()

df_ant <- fread("train.csv")
df_ant <- df_ant[,c("Semana","Producto_ID")]

qt <- df_ant %>%
  select(Semana,Producto_ID) %>%
  group_by(Semana,Producto_ID) %>%
  summarise(cnt_sem = n()) 

qt1 <- qt %>%
  filter(Semana<=8) %>%
  select(Producto_ID) %>%
  group_by(Producto_ID) %>%
  summarise(qt_sem=n()) %>%
  mutate(Semana=8)

qt2 <- qt %>%
  filter(Semana>3) %>%
  select(Producto_ID) %>%
  group_by(Producto_ID) %>%
  summarise(qt_sem=n()) %>%
  mutate(Semana=9)

qt1 <- rbind(qt1,as.data.frame(qt2))


df <- left_join(df,qt1,by=c("Semana","Producto_ID"))
rm(df_ant, qt, qt1, qt2)
gc()

df_ant <- fread("train.csv")

df_ant <- df_ant[df_ant$Semana<8,]

prod <- df_ant %>%
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

df <- left_join(df,prod,by=c("Cliente_ID","Producto_ID"))

rm(prod,rank)
gc()

lojas <- df_ant %>%
  select(Cliente_ID,Demanda_uni_equil)%>%
  group_by(Cliente_ID) %>%
  summarise(total_cli = sum(Demanda_uni_equil), qt=n()) %>%
  mutate(media_cli = total_cli/qt)

lojas <- lojas[order(-lojas$total_cli),]

lojas$rank_loja <- rank(-lojas$total_cli,ties.method ="min")

lojas$qt <- NULL
df <- left_join(df,lojas,by="Cliente_ID")

rm(lojas)
gc()

prods <- df_ant %>%
  select(Producto_ID,Demanda_uni_equil)%>%
  group_by(Producto_ID) %>%
  summarise(total_prod = sum(Demanda_uni_equil), qt=n()) %>%
  mutate(media_prod = total_prod/qt)

prods <- prods[order(-prods$total_prod),]

prods$rank_prod <- rank(-prods$total_prod,ties.method ="min")

prods$qt <- NULL
df <- left_join(df,prods,by="Producto_ID")

rm(prods)
gc()

rota <- df_ant %>%
  select(Ruta_SAK, Producto_ID, Demanda_uni_equil) %>%
  group_by(Ruta_SAK,Producto_ID) %>%
  summarise(total_rota = sum(Demanda_uni_equil), qt=n())%>%
  mutate(media_rota= total_rota/qt)

rota$qt <- NULL
df <- left_join(df,rota,by=c("Ruta_SAK","Producto_ID"))

rm(rota)
gc()

agencia <- df_ant %>%
  select(Agencia_ID, Producto_ID, Demanda_uni_equil) %>%
  group_by(Agencia_ID, Producto_ID) %>%
  summarise(total_agencia = sum(Demanda_uni_equil), qt=n())%>%
  mutate(media_agencia= total_agencia/qt)

agencia$qt <- NULL
df <- left_join(df,agencia,by=c("Agencia_ID","Producto_ID"))

rm(agencia)
gc()

#as colunas Venta_uni_hoy, Venta_hoy, Dev_uni_proxima e Dev_proxima não estão na base de testes, portanto serão removidas
df$Venta_uni_hoy <- NULL
df$Venta_hoy <- NULL
df$Dev_uni_proxima <- NULL
df$Dev_proxima <- NULL

#completando os valores NA's
#os valores NA's serão completados com zero, exceto para os rankings, pois no caso do 
#ranking, quanto maior o número menor a possibilidade do produto ter vendas,
#para o ranking será utilizado o valor máximo do ranking + 1

#verificando os valores máximos dos rankings
max(df[!is.na(df$rank_prod_loja),"rank_prod_loja"]) #201
max(df[!is.na(df$rank_loja),"rank_loja"]) #861296
max(df[!is.na(df$rank_prod),"rank_prod"]) #1651

#vetor com os valores NA por atributo
def_val_nas <- c(total_loja=0,qt_sem_loja=0,rank_prod_loja=202,total_cli=0,media_cli=0,rank_loja=861297,total_prod=0,media_prod=0,rank_prod=1652,total_rota=0,media_rota=0,total_agencia=0,media_agencia=0)

#preenchendo os NA's na base
for (d in c(1:length(def_val_nas)))
{
  
  col <- names(def_val_nas[d])
  i <- unname(def_val_nas[d])
  df[is.na(df[, col]), col] <- i
}  



write.csv2(df,"data_work.csv",row.names = F)
