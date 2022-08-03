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

##########################################################################
#######----------Cargar bases de datos y primers fusiones----------####### 
##########################################################################

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
                               "p3346s1","p3347s1", "p3348s1", "p8614s1" )], na.rm=TRUE))

M2 = M2 %>% 
  group_by(directorio) %>% 
  mutate(Gasto_h = sum(Gasto))

M2$Gasto_E <- ifelse(M2$Jefe==1, M2$Gasto_h, NA)

# Logro educactivo jefe del hogar recategorizado 

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

# Edad

M2$Edad <- M2$p6040

# Integrantes del hogar en edad escolar colapsado por Jefe del hogar 

M2$Jovenes <- ifelse(M2$p6040<18 & M2$p6051 == 3 & 4,1,0)
M2 = M2 %>% 
  group_by(directorio) %>% 
  mutate(Jovenes2 = sum(Jovenes))

M2$Escolares <- ifelse(M2$Jefe==1, M2$Jovenes2, NA)

# Ocupados en el hogar colapsados por jefe del hogar 

M2$Ocupados <- ifelse(M2$p6240==1,1,0)

M2 = M2 %>% 
  group_by(directorio) %>% 
  mutate(Ocupados_h = sum(Ocupados))

M2$Ocupacion <- ifelse(M2$Jefe==1, M2$Ocupados_h, NA)


##################################################################################
#######----------Subset base de datos y fusión con nuevos módulos----------####### 
##################################################################################


Subset1 <- M2 [, c("directorio", "secuencia_encuesta", "secuencia_p", "Sexo", "Edad", "Escolares", "orden","fex_c",
               "Jefe", "Gasto_E", "Logro_Jefe", "Estado", "Ocupacion" )] #Subset 1 

ECV_1 <- Subset1 %>%
  group_by(directorio)%>%
  filter(Jefe==1)%>%
  summarize(secuencia_encuesta =(secuencia_encuesta),
            secuencia_p=(secuencia_p),orden =(orden), factor = (fex_c),
            Edad = (Edad), Escolares = max (Escolares), Ocupados= max(Ocupacion), Gasto = max(Gasto_E), 
            Logro_Jefe=(Logro_Jefe), Estado=(Estado), Sexo = (Sexo))

# Fusión con mòdulos de hogares 


M3 <- as.data.frame(left_join(x=ECV_1 , y=S, by=c("directorio", "orden")))#Educación y fuerza de trabajo


# Creación variable electricidad

M3$Electricidad <- ifelse(M3$p791==1 | M3$p791==2,1,0)


#######----------Subset 2----------####### 

names(M3)
Subset2 <- M3 [, c("directorio", "orden", "Gasto", "Logro_Jefe", "factor", "Estado", "Ocupados", "Sexo", 
                   "cant_personas_hogar", "Edad", "Escolares",
                   "i_ugasto", "Electricidad")] #Subset 2 

# Fusión con módulos de hogares 

M4 <- as.data.frame(left_join(x=V , y=Subset2, by=c("directorio", "orden")))#Educación y fuerza de trabajo

# Creación de variables área urbano rural 

M4$Area <- ifelse(M4$clase==1,1,0)

M4$Area <- factor(M4$Area, 
                   labels = c("Rural", "Urbano"))


#######----------Subset 3----------####### 

names(M4)
Subset3 <- M4 [, c("directorio","factor", "orden"
                   , "Gasto", "Logro_Jefe", "Estado", "Edad", "Ocupados", "Sexo", "Escolares", "cant_personas_hogar",
                   "i_ugasto", "Area", "Electricidad")] #Subset 3



#####################################################
#######----------Base final y modelo----------####### 
#####################################################


ECV <- subset(Subset3, Gasto!=0, i_ugasto!=0)

# Imputación de los missings por la no respuesta en la variable logro educativo

ECV = ECV %>%
  mutate(Logro_Jefe = ifelse(is.na(ECV$Logro_Jefe)==T,
                             yes = 1,
                             no = ECV$Logro_Jefe))

is.na(ECV$Logro_Jefe)%>% table# Revisión NA



# Creación Logaritmo del gasto y edad al cuadrado

ECV$le <- log(ECV$Gasto) 

ECV$Edad2 <- ECV$Edad^2

#######----------Estadística descriptiva----------####### 

summary(ECV$Gasto)


#Promedio del gasto en educación de acuerdo con el sexo del Jefe del hogar 

gassex = aggregate(Gasto ~ Sexo, data = ECV, FUN = mean)
colnames(gassex) <- c("Sexo jefe hogar","Promedio de gasto en educación mes anterior")
export(gassex, "gassex6.xlsx")


#Promedio del gasto en educación por rangos de edad del Jefe del hogar 

summary(ECV$Edad)

ECV$Edad_jefe <- ifelse(ECV$Edad>= 12 & ECV$Edad<= 19,'12 a 19',0)
ECV$Edad_jefe <- ifelse(ECV$Edad>= 20 & ECV$Edad<= 30,'20 a 30',ECV$Edad_jefe)
ECV$Edad_jefe <- ifelse(ECV$Edad>= 31 & ECV$Edad<= 50,'31 a 50',ECV$Edad_jefe)
ECV$Edad_jefe <- ifelse(ECV$Edad>= 51 & ECV$Edad<= 70,'51 a 70',ECV$Edad_jefe)
ECV$Edad_jefe <- ifelse(ECV$Edad>= 71,'Mayores de 71',ECV$Edad_jefe)


ggplot(data = ECV, mapping = aes(x = Edad_jefe, y = Gasto/1000, options, 
                                fill = Sexo)) + geom_bar(stat = "identity") +
  labs(x = "Edad Jefe", y = "Gasto en educación") + 
  scale_color_manual(values = c("0"="coral1" , "1"="cyan4"))


#Promedio del gasto en educación por área de reidencia del hogar 

summary(ECV$Area)

gasarea = aggregate(Gasto ~ Area, data = ECV, FUN = mean)
colnames(gasarea) <- c("Area","Promedio de gasto en educación mes anterior")
export(gasarea, "gasarea1.xlsx")


#######----------Modelos----------#######  

set.seed(10101) 
id_Train <- sample(1:nrow(ECV), size=0.7*nrow(ECV), replace = F)
train <- ECV[id_Train, ]
test <- ECV[-id_Train, ]


# Modelo 1
Modelo_1 = lm(le ~ Escolares + Edad + Edad2 + as.factor (Area) + Ocupados + Sexo + cant_personas_hogar +
                Electricidad + as.factor (Logro_Jefe) + as.factor (Estado) +  
                i_ugasto, data = train)

summary(Modelo_1)

# Modelo 2: solo las variables significativas

Modelo_2 = lm(le ~ Escolares + Ocupados + 
                Electricidad + as.factor (Logro_Jefe) + 
                i_ugasto, data = train)

summary(Modelo_2)


##########################################################################
#######----------Métodos de regularización y predicciones----------####### 
##########################################################################


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

# Encontrando el mejor modelo para predecir: comparaciones RMSE

RMSE_1
lasso
ridge
Elastic_Net
arbol 

# Predicción del gasto en educación por hogar con el mejor modelo

test$pred_arbol <-predict(arbol,test) #Predicción
test$arbol_gasto <- exp(test$pred_arbol) #Exponencial


#######----------Generando un criterio de clasificación----------####### 

# Diferencia entre el valor predicho y el de la muestra 

test$Dif <- test$Gasto-test$arbol_gasto


# Criterio de clasificación  

summary(test$Gasto)

test$clas_arbol <- ifelse(test$Dif>=150000 | test$Dif<= -150000, 1, 0)
table(test$clas_arbol)


############################################################################


