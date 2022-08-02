###################################################
####### ALUMNA: CAMILA CIURLO ARAGÓN ##############
######ALUMNO: PAOLO VALCARCEL PINEDAD #############
###### CURSO: BIG DATA Y MACHINE LEARNING #########
#TRABAJO FINAL: PREDDICCIÓN DE GASTO EN EDUCACIÓN #
###################################################

rm(list = ls())
setwd("C:/Users/Camila Ciurlo/Desktop/MeCA/Big data/Educando")


#######----------Cargar librerías----------####### 

library(pacman)
p_load(rio, tidyverse, skimr)
library(ggplot2)
library(patchwork)
library(caret)
library(caTools)
library(RColorBrewer)
library(stargazer)
library(dplyr)
library("nycflights13")
library(lubridate)
library(tseries)
library(car)
library(foreign)
library(timsac)
library(lmtest)
library(mFilter)
library(nlme)
library(lmtest)
library(broom)
library(kableExtra)
library(knitr)
library(MASS)
library(parallel)
library(mlogit)
library(tidyr)
library(forecast)
library(stats)
library(quantmod)
library(foreach)
library(ISLR)
library(caret)
#install.packages("Matrix")
library(glmnet)
library(readr)
pacman::p_load(gridExtra, scales, ggcorrplot, e1071)
library(haven)
library(rpart)

#######----------Cargar y primers fusiones de  bases de datos----------####### 

C <- read_dta("C:/Users/Camila Ciurlo/Desktop/MeCA/Big data/Educando/Caracteristicas y_composicion_del_hogar.dta")#Características del hogar
E <- read_dta("C:/Users/Camila Ciurlo/Desktop/MeCA/Big data/Educando/Educación.dta")#Educación
FT <- read_dta("C:/Users/Camila Ciurlo/Desktop/MeCA/Big data/Educando/Fuerza de trabajo.dta")#Fuerza de trabjo
V <- read_dta("C:/Users/Camila Ciurlo/Desktop/MeCA/Big data/Educando/Datos de la vivienda.dta")#Datos vivienda
S <- read_dta("C:/Users/Camila Ciurlo/Desktop/MeCA/Big data/Educando/Servicios del hogar.dta")#Servicios del hogar

# Merge 1

M1 <- as.data.frame(left_join(x=E , y=C, by=c("directorio", "secuencia_encuesta", 
                                              "secuencia_p", "orden")))#Educación y características del hogar

# Merge 2

M2 <- as.data.frame(left_join(x=FT , y=M1, by=c("directorio", "secuencia_encuesta", 
                                                "secuencia_p", "orden")))#Educación y fuerza de trabajo



#######----------Creación de variables----------####### 


# Jefe del hogar 

M2$Jefe <- ifelse(M2$p6051==1,1,0)

# Gasto en educación colapsado por jefe del hogar

M2 = M2 %>%
  mutate(Gasto = rowSums(M2[,c("p6180s1", "p3343s1", "p3344s1", "p3345s1", 
                               "p3346s1","p3347s1", "p3348s1", "p8610s1", "p8614s1" )], na.rm=TRUE))

M2 = M2 %>% 
  group_by(directorio) %>% 
  mutate(Gasto_h = sum(Gasto))

M2$Gasto_E <- ifelse(M2$Jefe==1, M2$Gasto_h, NA)

# Logro educactivo jefe del hogar

M2$Logro[M2$p8587== 1 | M2$p8587== 2] <- 1
M2$Logro[M2$p8587== 3 ] <- 2
M2$Logro[M2$p8587== 4 ] <- 3
M2$Logro[M2$p8587== 5 ] <- 4
M2$Logro[M2$p8587== 6 | M2$p8587== 7] <- 5
M2$Logro[M2$p8587== 8 | M2$p8587== 9] <- 6
M2$Logro[M2$p8587== 10 | M2$p8587== 11] <- 7
M2$Logro[M2$p8587== 12 | M2$p8587== 13] <- 8


M2$Logro_Jefe <- ifelse(M2$Jefe==1, M2$Logro, NA)

M2$Logro_Jefe <- factor(M2$Logro_Jefe, 
                   labels = c("Ninguno", "Primaria", "Secundaria",
                              "Media", "Técnica", "Tecnológica", "Universitaria", "Posgrado"))

# Estado civil Jefe del hogar 

M2$Estado <- ifelse(M2$Jefe==1 & M2$p5502==1 | M2$p5502== 2 | M2$p5502== 6,1, NA)
M2$Estado <- ifelse(M2$Jefe==1 & M2$p5502==3 | M2$p5502== 4 | M2$p5502== 5,0, M2$Estado)

M2$Sexo <- factor(M2$Estado, 
                  labels = c("Soltero", "En pareja"))


# Sexo Jefe del hogar 

M2$Sexo <- ifelse(M2$Jefe==1 & M2$p6020==1,1,0)
M2$Sexo <- factor(M2$Sexo, 
                   labels = c("Mujer", "Hombre"))



# Integrantes del hogar en edad escolar imputados al Jefe 

M2$Jovenes <- ifelse(M2$p6040<18 & M2$p6051 == 3 & 4,1,0)
M2 = M2 %>% 
  group_by(directorio) %>% 
  mutate(Jovenes2 = sum(Jovenes))

M2$Escolares <- ifelse(M2$Jefe==1, M2$Jovenes2, NA)

# Ocupados en el hogar imputados al Jefe 

M2$Ocupados <- ifelse(M2$p6240==1,1,0)

M2 = M2 %>% 
  group_by(directorio) %>% 
  mutate(Ocupados_h = sum(Ocupados))

M2$Ocupacion <- ifelse(M2$Jefe==1, M2$Ocupados_h, NA)

#######----------Subset base de datos y fusión con nuevos módulos----------####### 


Subset1 <- M2 [, c("directorio", "secuencia_encuesta", "secuencia_p", "Sexo", "orden","fex_c",
               "Jefe", "Gasto_E", "Logro_Jefe", "Estado", "Escolares", "Ocupacion" )] #Subset ECV 

ECV_1 <- Subset1 %>%
  group_by(directorio)%>%
  filter(Jefe==1)%>%
  summarize(secuencia_encuesta =(secuencia_encuesta),
            secuencia_p=(secuencia_p),orden =(orden), factor = (fex_c),
            Escolares=max(Escolares), Ocupados= max(Ocupacion), Gasto = max(Gasto_E), 
            Logro_Jefe=(Logro_Jefe), Estado=(Estado), Sexo = (Sexo))

# Fusión con mòdulos de hogares 


M3 <- as.data.frame(left_join(x=ECV_1 , y=S, by=c("directorio", "orden")))#Educación y fuerza de trabajo


# Creación variable electricidad

M3$Electricidad <- ifelse(M3$p791==1 | M3$p791==2,1,0)

# Subset 2

names(M3)
Subset2 <- M3 [, c("directorio", "orden", "Gasto", "Logro_Jefe", "factor", "Estado", "Escolares", "Ocupados", "Sexo", "cant_personas_hogar",
                   "i_ugasto", "Electricidad")] #Subset ECV 

# Fusión con mòdulos de hogares 

M4 <- as.data.frame(left_join(x=V , y=Subset2, by=c("directorio", "orden")))#Educación y fuerza de trabajo

# Creación de variables electricidad y zona 

M4$Area <- ifelse(M4$clase==1,1,0)

M4$Area <- factor(M4$Area, 
                   labels = c("Rural", "Urbano"))


# Seubset 3

names(M4)
Subset3 <- M4 [, c("directorio","factor", "orden"
                   , "Gasto", "Logro_Jefe", "Estado", "Escolares", "Ocupados", "Sexo", "cant_personas_hogar",
                   "i_ugasto", "Area", "Electricidad")] #Subset 3



#######----------Base final y modelo----------####### 


ECV <- subset(Subset3, Gasto!=0, i_ugasto!=0)
ECV = ECV %>%
  mutate(Logro_Jefe = ifelse(is.na(ECV$Logro_Jefe)==T,
                             yes = 1,
                             no = ECV$Logro_Jefe))

is.na(ECV$Logro_Jefe)%>% table# Revisando NA

# Creación Logaritmo del gasto

ECV$le <- log(ECV$Gasto) 

# Modelo 

set.seed(10101) 
id_Train <- sample(1:nrow(ECV), size=0.7*nrow(ECV), replace = F)
train <- ECV[id_Train, ]
test <- ECV[-id_Train, ]


# Modelo 1

Modelo_1 = lm(le ~ Escolares + as.factor (Area) + Ocupados + Sexo + cant_personas_hogar +
                Electricidad + as.factor (Logro_Jefe) + as.factor (Estado) + 
                i_ugasto, data = train)

summary(Modelo_1)

# Modelo 2: solo las variables significativas

Modelo_2 = lm(le ~ Escolares + Ocupados + 
                Electricidad + as.factor (Logro_Jefe) + 
                i_ugasto, data = train)

summary(Modelo_2)

#########----Métodos de regularización: predicción dentro de muestra----#####


#Regresión lneal 

Modelo_2 = lm(le ~ Escolares + Ocupados + 
                Electricidad + as.factor (Logro_Jefe) + 
                i_ugasto, data = train)

# Predicción y MSE 

y_hat_in<- predict(Modelo_2, train)
y_real_in<-train$le
MSE_1 <- mean ((y_hat_in-y_real_in)^2)
RMSE_1 <-sqrt(MSE_1)

#Ctrl
ctrl <- trainControl(method = "cv",
                     number = 5,
                     summaryFunction = defaultSummary,
                     classProbs = TRUE,
                     verbose=FALSE,
                     savePredictions = T)

# Árboles
set.seed(10101) 
arbol <- train(le ~ Escolares + Ocupados + 
                 Electricidad + as.factor (Logro_Jefe) + 
                 i_ugasto, data = train, method = "rpart", trControl = ctrl,
  parms=list(split='Gini'), tuneLength=200)

arbol

# Lasso 
set.seed(10101) 
lambda <- 10^seq(-2, 3, length = 10)
lasso <- train(le ~ Escolares + Ocupados + 
                    Electricidad + as.factor (Logro_Jefe) + 
                    i_ugasto, data = train, method = "glmnet",
               trControl = trainControl("cv", number = 10),
               tuneGrid = expand.grid(alpha = 1,
                                      lambda=lambda), preProcess = c("center", "scale"))
lasso

#Ridge
set.seed(10101) 
ridge <- train(le ~ Escolares + Ocupados + 
                 Electricidad + as.factor (Logro_Jefe) + 
                 i_ugasto, data = train, method = "glmnet",
                  trControl = trainControl("cv", number = 10),
                  tuneGrid = expand.grid(alpha = 0,lambda=lambda), preProcess = c("center", "scale"))
ridge

#Elastic Net
set.seed(10101)
Elastic_Net <- train(le ~ Escolares + Ocupados + 
              Electricidad + as.factor (Logro_Jefe) + 
              i_ugasto, data = train, method = "glmnet",
              trControl = trainControl("cv", number = 10), preProcess = c("center", "scale"))
Elastic_Net

# Encontrando el mejor modelo para predecir 

RMSE_1
lasso
ridge
Elastic_Net
arbol 

# Predicción del gasto en educación por hogar 

test$pred_MCO <- predict (Modelo_2, test)
test$MCO_gasto <- exp(test$pred_MCO)

test$pred_arbol <-predict(arbol,test)
test$arbol_gasto <- exp(test$pred_arbol)

test$pred_lasso <-predict(lasso,test)
test$lasso_gasto <- exp(test$pred_lasso)

test$pred_ridge <-predict(ridge,test)
test$ridge_gasto <- exp(test$pred_ridge)

test$pred_Enet <-predict(Elastic_Net,test)
test$ridge_gasto <- exp(test$pred_Enet)

test$Dif <- test$Gasto-test$arbol_gasto

# Criterio de clasificación  

test$clas <- ifelse(test$Dif>=200000 | test$Dif<= -200000, 1, 0)
table(test$clas)

mean (test$Dif)