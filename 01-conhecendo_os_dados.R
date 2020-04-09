#Para esse projeto, por ser uma competição finalizada da Kaggle, já possuimos um benchmark com relação a o que é um bom score,
#Nessa competição, a métrica usada na Kaggle é o RMSLE, portanto essa será a métrica usada no modelo, 
#O objetivo é um modelo com score de até 0.5 no Private Score da competição na Kaggle, 
#o modelo com melhor score na base de testes durante o processo de treinamento, será utilizado na 
#base de testes da competição e submetido ao Kaggle para calculo do RMSLE.

#Esse script busca conhecer os dados da base de treino e teste para a criação de novos atributos 

#Conforme a descrição dos dados, eles estão divididos em semanas, buscando entender como esses dados 
#estão divididos nas bases de treino e testes

#lendo os dados de treino
library(data.table)

df <- fread("train.csv")

#conhecendo os dados de treino
head(df)
str(df)

#divisão dos dados está em semanas, vamos verificar a semana inicial e final
min(df$Semana)
max(df$Semana)

#os dados estão dispostos entre semana 3 e 9, ou seja são 7 semanas de dados, vamos verificar as semanas na base de teste
#se a mesma está na sequencia ou os dados estão inseridos dentro das mesmas semanas dos dados de treino
teste <- fread("test.csv")

#verificando as semanas dos dados de teste
min(teste$Semana)
max(teste$Semana)
#os dados de teste estão compreendidos entre a semana 10 e 11, ou seja, estão na sequencia dos dados de treino

#como os dados de teste estão dividos entres as semanas 10 e 11, será utilizado como base de trabalho as duas
#últimas semanas da base de treino (semanas 8 e 9), além disso, os novos atributos serão criados com os dados das 5 
#semanas anteriores (semanas 3 a 7).
rm(teste)
gc()
df <- df[df$Semana>=8, ]

#Na base de treinos, além das semanas, temos alguns outros atributos como Agencia_ID, Canal_ID, Ruta_SAK, Cliente_ID, Producto_ID 
#representados como inteiros, porém esses dados são categóricos, sendo apenas uma representação numérica das respectivas categorias
#váriaveis categóricas com muitas categorias dificultam a criação do modelo de machine learning.
#Portanto, variáveis com muitas categorias, serão sintetizadas de outras maneiras (com base em dados históricos das semanas anteriores)

#verificando o número de categorias das variáveis categóricas
length(unique(df$Agencia_ID))
length(unique(df$Canal_ID))
length(unique(df$Ruta_SAK))
length(unique(df$Cliente_ID))
length(unique(df$Producto_ID))
#o unico atributo com poucas categorias é o atributo Canal_ID (9 categorias), todos os demais possuem mais de 500 valores distintos
#Sendo assim, no modelo preditivo, o atributo Canal_ID será usado como categorico e os demais serão sintetizados de outras formas
#a partir dos dados  históricos

#distribuição da variável target
hist(df$Demanda_uni_equil)
#a grande maioria dos registros possuí menos de 1.000 unidades, verificando o boxplot
library(ggplot2)

ggplot(df,aes(x="Geral",y=Demanda_uni_equil)) + geom_boxplot()

#o boxplot evidencia a maior parte dos valores próximo a 0, com muitos outliers, ajustando a escala para melhor visualização
ggplot(df,aes(x="Geral",y=Demanda_uni_equil)) + geom_boxplot() + ylim(c(0,1000))

#mesmo com a escala ajustada, a maioria ainda está próximo de zero, verificando os quartis
quantile(df$Demanda_uni_equil)

#apesar do valor máximo ser 5.000 unidades, em 75% dos registros foram comercializados no máximo 6 unidades de 
#um determinado produto, sendo que o valor da mediana é 3 (dados observados apenas nas semanas 8 e 9).
