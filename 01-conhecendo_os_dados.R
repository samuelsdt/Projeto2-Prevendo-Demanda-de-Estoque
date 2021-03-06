#Para esse projeto, por ser uma competi��o finalizada da Kaggle, j� possuimos um benchmark com rela��o a o que � um bom score,
#Nessa competi��o, a m�trica usada na Kaggle � o RMSLE, portanto essa ser� a m�trica usada no modelo, 
#O objetivo � um modelo com score de at� 0.5 no Private Score da competi��o na Kaggle, 
#o modelo com melhor score na base de testes durante o processo de treinamento, ser� utilizado na 
#base de testes da competi��o e submetido ao Kaggle para calculo do RMSLE.

#Esse script busca conhecer os dados da base de treino e teste para a cria��o de novos atributos 

#Conforme a descri��o dos dados, eles est�o divididos em semanas, buscando entender como esses dados 
#est�o divididos nas bases de treino e testes

#lendo os dados de treino
library(data.table)

df <- fread("train.csv")

#conhecendo os dados de treino
head(df)
str(df)

#divis�o dos dados est� em semanas, vamos verificar a semana inicial e final
min(df$Semana)
max(df$Semana)

#os dados est�o dispostos entre semana 3 e 9, ou seja s�o 7 semanas de dados, vamos verificar as semanas na base de teste
#se a mesma est� na sequencia ou os dados est�o inseridos dentro das mesmas semanas dos dados de treino
teste <- fread("test.csv")

#verificando as semanas dos dados de teste
min(teste$Semana)
max(teste$Semana)
#os dados de teste est�o compreendidos entre a semana 10 e 11, ou seja, est�o na sequencia dos dados de treino

#como os dados de teste est�o dividos entres as semanas 10 e 11, ser� utilizado como base de trabalho as duas
#�ltimas semanas da base de treino (semanas 8 e 9), al�m disso, os novos atributos ser�o criados com os dados das 5 
#semanas anteriores (semanas 3 a 7).
rm(teste)
gc()
df <- df[df$Semana>=8, ]

#Na base de treinos, al�m das semanas, temos alguns outros atributos como Agencia_ID, Canal_ID, Ruta_SAK, Cliente_ID, Producto_ID 
#representados como inteiros, por�m esses dados s�o categ�ricos, sendo apenas uma representa��o num�rica das respectivas categorias
#v�riaveis categ�ricas com muitas categorias dificultam a cria��o do modelo de machine learning.
#Portanto, vari�veis com muitas categorias, ser�o sintetizadas de outras maneiras (com base em dados hist�ricos das semanas anteriores)

#verificando o n�mero de categorias das vari�veis categ�ricas
length(unique(df$Agencia_ID))
length(unique(df$Canal_ID))
length(unique(df$Ruta_SAK))
length(unique(df$Cliente_ID))
length(unique(df$Producto_ID))
#o unico atributo com poucas categorias � o atributo Canal_ID (9 categorias), todos os demais possuem mais de 500 valores distintos
#Sendo assim, no modelo preditivo, o atributo Canal_ID ser� usado como categorico e os demais ser�o sintetizados de outras formas
#a partir dos dados  hist�ricos

#distribui��o da vari�vel target
hist(df$Demanda_uni_equil)
#a grande maioria dos registros possu� menos de 1.000 unidades, verificando o boxplot
library(ggplot2)

ggplot(df,aes(x="Geral",y=Demanda_uni_equil)) + geom_boxplot()

#o boxplot evidencia a maior parte dos valores pr�ximo a 0, com muitos outliers, ajustando a escala para melhor visualiza��o
ggplot(df,aes(x="Geral",y=Demanda_uni_equil)) + geom_boxplot() + ylim(c(0,1000))

#mesmo com a escala ajustada, a maioria ainda est� pr�ximo de zero, verificando os quartis
quantile(df$Demanda_uni_equil)

#apesar do valor m�ximo ser 5.000 unidades, em 75% dos registros foram comercializados no m�ximo 6 unidades de 
#um determinado produto, sendo que o valor da mediana � 3 (dados observados apenas nas semanas 8 e 9).
