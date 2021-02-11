library(dplyr)
covid <- read.csv("C:/Users/guadalupe martinez/Desktop/Machine Learning/Github respaldo/16-Tarea/covidfilter.csv")
head(covid)
covid <- select(covid, -X) %>%
  mutate(SEXO = ifelse(SEXO == "HOMBRE", 1, 0), OBESIDAD = ifelse(OBESIDAD == "SI", 1, 0),
         DIABETES = ifelse(DIABETES == "SI", 1, 0), EPOC = ifelse(EPOC == "SI", 1, 0),
         ASMA = ifelse(ASMA == "SI", 1, 0), CARDIOVASCULAR = ifelse(CARDIOVASCULAR == "SI", 1, 0),
         RENAL.CRONICA = ifelse(RENAL.CRONICA == "SI", 1, 0), INMUNOSUPRESION = ifelse(INMUNOSUPRESION == "SI", 1, 0),
         HIPERTENSION = ifelse(HIPERTENSION == "SI", 1, 0), TIPO.PACIENTE = ifelse(TIPO.PACIENTE == "HOSPITALIZADO", 1, 0),
         TABAQUISMO = ifelse(TABAQUISMO == "SI", 1, 0))
head(covid)
write.csv(covid, file = "Covid_clean.csv")

library(MASS)
library(leaps)
#En este modelo la variable "y" a trabajar sera "Tipo paciente"
#Queremos saber si hay alguna relación entre las covariables restantes y si de ellas depende
#si la persona esta hospitalizada o no
nullmodel = lm(TIPO.PACIENTE~ 1, data = covid)
fullmodel <- lm(TIPO.PACIENTE~ ., data = covid)
print("Selección de variables con AIC 'forward'")
stepAIC(nullmodel, scope = list(lower=nullmodel,upper=fullmodel), data=covid, direction='forward')
print("Selección de variables con AIC 'backward'")
stepAIC(fullmodel, scope = list(lower=nullmodel,upper=fullmodel), data=covid, direction='backward')
print("Selección de variables con AIC 'stepwise'")
stepAIC(fullmodel, scope = list(lower=nullmodel,upper=fullmodel), data=covid, direction='both')
print("Los 3 modelos contemplan todas las covariables")

print("Criterios BIC y Mallow Cp")
models = regsubsets(TIPO.PACIENTE~. ,data=covid, nbest=1 , nvmax=11)
info = summary(models)
cbind(info$which, round(cbind(bic=info$bic, cp=info$cp),3))

print("Vemos que el BIC y Cp mas pequeño en ambos casos son con el modelo que contempla todas las covariables")
