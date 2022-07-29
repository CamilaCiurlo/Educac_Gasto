###################################################
####### ALUMNA: CAMILA CIURLO ARAGÓN ##############
######ALUMNO: PAOLO VALCARCEL PINEDAD #############
###### CURSO: BIG DATA Y MACHINE LEARNING #########
#TRABAJO FINAL: PREDDICCIÓN DE GASTO EN EDUCACIÓN #
###################################################

setwd("C:/Users/Camila Ciurlo/Desktop/MeCA/Big data/Gasto_Educacion")
rm(list = ls())

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


#######----------Cargar bases de datos----------####### 


C <- read_dta("C:/Users/Camila Ciurlo/Desktop/MeCA/Big data/Gasto_Educacion/Caracteristicas y composicion del hogar.dta")
V <- read_dta("C:/Users/Camila Ciurlo/Desktop/MeCA/Big data/Gasto_Educacion/Datos de la vivienda.dta")
E <- read_dta("C:/Users/Camila Ciurlo/Desktop/MeCA/Big data/Gasto_Educacion/Educación.dta")
T <- read_dta("C:/Users/Camila Ciurlo/Desktop/MeCA/Big data/Gasto_Educacion/Fuerza de trabajo.dta")
S <- read_dta("C:/Users/Camila Ciurlo/Desktop/MeCA/Big data/Gasto_Educacion/Servicios del hogar.dta")

names(V)



#######----------Merge bases de datos prueba 1----------####### 

#put all data frames into list
#BD <- list(C, V, E, T, S)    
#merge all data frames together
#BD %>% as.data.frame(reduce(full_join, by=c("directorio", "secuencia_encuesta", "secuencia_p", "orden")))

#######----------Merge bases de datos----------####### 

Primera <- as.data.frame(left_join(x=E , y=V, by=c("directorio", "secuencia_encuesta", "secuencia_p", "orden")))
Segunda <- as.data.frame(left_join(x=Primera , y=S, by=c("directorio", "secuencia_encuesta", "secuencia_p", "orden")))
Tercera <-  as.data.frame(left_join(x=C , y=T, by=c("directorio", "secuencia_encuesta", "secuencia_p", "orden")))
BD_Final <- as.data.frame(left_join(x=Tercera , y=Segunda, by=c("directorio", "secuencia_encuesta", "secuencia_p", "orden")))



#######----------Subset bases de dato----------####### 

#Se trabaja con las variables relevantes para el modelo 


ECV <- BD_Final [, c("directorio", "secuencia_encuesta", "secuencia_p", "orden", "p6020",  "fex_c", "p6040", "p6051",
              "p5502", "p6071", "p6081", "p6087", "p6083", "p6088", "p8520s1", "p1_departamento", 
              "region", "clase", "i_hogar", "i_ugasto", "percapita", "i_ou",
                   "cant_personas_hogar", "p8587", "p8587s1", "p3341", "p3341s1", "p3342", "p3342s1",
                   "p3343", "p3343s1", "p3344", "p3344s1", "p3345", "p3345s1", "p3346", "p3346s1", "p3347", "p3347s1",
                   "p3348", "p3348s1", "p6240")] #Subset ECV 

rm(BD_Final, C, E, Primera, S, Segunda, T, Tercera, V)


#######----------Creación de variables----------####### 

#Gasto en educación 

#ECV = ECV %>%
#group_by(directorio) %>%
#mutate(Gasto = sum ("p3343s1", "p3344s1", "p3345s1", "p3346s1","p3347s1", "p3348s1", na.rm=T))


ECV = ECV %>%
  mutate(Gasto = rowSums(ECV[,c("p3343s1", "p3344s1", "p3345s1", "p3346s1","p3347s1", "p3348s1" )], na.rm=TRUE))


#Jefe
ECV$Jefe <- ifelse(ECV$p6051==1,1,0)

#Escolaridad Jefe del hogar

#ECV$ESC <- ifelse(ECV$p8587==1 & 2 & 3, 0, 1)

#Estado civil 
ECV$Estado <- ifelse(ECV$p5502==1 & 2 & 6,1,0)

#Sexo
ECV$Sexo <- ifelse(ECV$p6020==1,1,0)

#Energía
ECV$Energia <- ifelse(ECV$p8520s1==1,1,0)

#Nùmero de niños en edad escolar por hogar 

ECV$Jovenes <- ifelse(ECV$p6040<18 & ECV$p6051 == 3 & 4,1,0)

#Área
ECV$Area <- ifelse(ECV$clase==1,1,0)

#Ocupación

ECV$Ocupados <- ifelse(ECV$p6240==1,1,0)
is.na(ECV$p6240)

#Perceptor de ingresos prueba 

ECV$PI <-ifelse (ECV$p6040 <= 16 | ECV$p6040 >= 62 & ECV$Sexo== 1, 0, ECV$Ocupados)
ECV$PI <-ifelse (ECV$p6040 <= 16 | ECV$p6040 >= 57 & ECV$Sexo== 0, 0, ECV$Ocupados)

is.na(ECV)

colSums(is.na(ECV))

which(colSums(is.na(T))>0)

names(which(colSums(is.na(T))>0))



T$PI <- ifelse(ECV$p6040 <= 16 | ECV$p6040 > 62 & Sexo== 1, 0, Ocupados)

rm(ocu_pred)
rm(pred_arbol)

#Subset base de trabajo 

rm(Area, ECV_modelo, ECV_modelo_1, ECV_modelo2, Escolares, Gasto, Ocupados)
rm(Train)
rm(ECV_model)


ECV_model <- ECV %>%
  group_by(directorio) %>%
  summarize(personas = max(cant_personas_hogar, na.rm = TRUE), 
            Escolares=sum(Jovenes), ocupados= sum(Ocupados), Area= max(Area, na.rm = TRUE),
            Ingreso = max(i_ugasto, na.rm = TRUE), Energia = max(Energia, na.rm = TRUE), Gasto = sum(Gasto))

ECV_final <- ECV_model                                    # Duplicate data
ECV_final [is.na(ECV_final) | ECV_final == "Inf"] <- NA  # Replace NaN & Inf with NA


Modelo_1 = lm(Gasto ~ personas + Escolares + Area + Ingreso + Energia, data = ECV_final)
summary(Modelo_1)



arbol <- rpart(Ocupados ~Sexo + p6040 + Estado,
               data = ECV, method = "class")

pred_arbol <- as.data.frame (predict(arbol, type ="class"))

ocu_pred <- cbind(T, pred_arbol)

as.da

#######----------Modelo 1: regresión lineal múltiple----------####### 

set.seed(10101) 
Train <- sample(1:nrow(ECV_model), size=0.7*nrow(ECV_model), replace = F)
x.Train <- Train[id_Train, ]
x.Test <- Train[-id_Train, ]

names(x.Train)
Modelo_1 = lm(Ingpcug ~ Esc + Sexo + Edad + Edad2 + Jefe + Npersug + Vivienda + Habit, data = x.Train)
summary(Modelo_1)

is.na(ECV_model)

colSums(is.na(ECV_model))

which(colSums(is.na(my_df))>0)

names(which(colSums(is.na(my_df))>0))



