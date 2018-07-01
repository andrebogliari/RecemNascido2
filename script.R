library(tidyverse)
library(ggplot2)
library(rpart)
library(rpart.plot)

#Puxando os dados

data <- read.csv("birthwt.csv")

#Transformando variáveis categóricas em fatores

cols <- c('low', 'race', 'smoke', 'ht', 'ui')
data[cols] <- lapply(data[cols], as.factor)

# Examinando estrutura do dataframe

str(data)

# Olhando o histograma

hist(data$bwt)

#Separando dados para treinamento e testes

set.seed(123)

train.index <- sample((nrow(data)),0.7*nrow(data))

train <- data[train.index,]
test <- data[-train.index,]

#Aplicando a regressão logistica 

fit <- glm(low ~ lwt + age + race + ptl + smoke + ht + ui + ftv, data = train, family = binomial(link = "logit"))
           
summary(fit)

anova(fit, test = "Chisq")

# Na tabela entregue pela função anova vemos que a diferença entre o null deviance e as desviances residuais de age = 0,221; race = 0,185 e ftv = 0,383 mostram que 
# estas variaveis não infleunciam nossas variaveis. Logo, excluimos elas.

novo_fit <- glm(low ~ lwt + ptl + smoke + ht + ui, data = train, family = binomial(link = "logit"))

summary(novo_fit)

anova(novo_fit, test = "Chisq")

# Testando modelo

prediction_prob <- predict(novo_fit, newdata = test, type = 'response')
prediction <- ifelse(prediction_prob > 0.5,1,0)
table(prediction, test$low)

# Analisando em porcentagem

prop.table((table(prediction, test$low)))



