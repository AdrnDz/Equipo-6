#0.  Entorno de trabajo

rm(list=ls())    
graphics.off()    

library(foreign)
library(factoextra)
library(dplyr)
library(knitr)
library(tidyverse)
library(readxl)
paquetes <- c("tidyverse", "foreign", "factoextra", "ggplot2", "psych", "httr")
for (i in paquetes) {if (!require(i, character.only = TRUE)) {install.packages(i);library(i, character.only = TRUE)} else {library(i, character.only = TRUE)}}

#1. Cargar base
PROYECTO <- read_csv("C:/Users/migue/Downloads/Base.csv")
#2. Limpieza
Filtrado<- c("sexo","nivel_esc","lenguaje","puesto","carrera_cve","p3","experiencia")

Datosfiltrados<-PROYECTO[,Filtrado]
print(Datosfiltrados)

# Quitar respuestas invalidas
Datosfiltrados<-Datosfiltrados%>%drop_na()

# Renombrar p3
colnames(Datosfiltrados)[colnames(Datosfiltrados) == "p3"] <- "Ingresos"

# Define los límites de los intervalos
limites_ingresos <- c(0, 10, 5000, 10000, 15000, 29000, Inf)

# Define las etiquetas para cada rango
#Nada -> 0
#Muy bajo -> 1
#Bajo -> 2
#Medio -> 3
#Alto -> 4 
#Muy alto -> 5
etiquetas_ingresos <- c("1", "2","3", "4", "5","6")

# Utiliza la función cut para categorizar la columna de ingresos
Datosfiltrados$CategoríaIngresos <- cut(Datosfiltrados$Ingresos, breaks = limites_ingresos, labels = etiquetas_ingresos, include.lowest = TRUE)

# Muestra el resultado
print(Datosfiltrados)
#------------------

# Filtrar variables
var<-c("sexo","nivel_esc","lenguaje","puesto","carrera_cve","experiencia","CategoríaIngresos")
Datosfiltrados<-Datosfiltrados[,var]

# Tabulados

table(Datosfiltrados$sexo)
table(Datosfiltrados$nivel_esc)
table(Datosfiltrados$lenguaje)
table(Datosfiltrados$puesto)
table(Datosfiltrados$carrera_cve)
table(Datosfiltrados$experiencia)
table(Datosfiltrados$CategoríaIngresos)

# Proporciones
table(Datosfiltrados$sexo) %>% prop.table()
table(Datosfiltrados$nivel_esc) %>% prop.table()
table(Datosfiltrados$lenguaje) %>% prop.table()
table(Datosfiltrados$puesto) %>% prop.table()
table(Datosfiltrados$carrera_cve) %>% prop.table()
table(Datosfiltrados$experiencia) %>% prop.table()
table(Datosfiltrados$CategoríaIngresos) %>% prop.table()

#LOGITTT

#Recodificación de datos
Datosfiltrados <- Datosfiltrados %>%
  mutate(sexo = recode(sexo, 
                              "1" = "0",
                              "2" = "1"))

Datosfiltrados <- Datosfiltrados %>%
  mutate(lenguaje = recode(lenguaje, 
                       "0" = "1",
                       "1" = "2"))

Datosfiltrados <- Datosfiltrados %>%
  mutate(experiencia = recode(experiencia, 
                       "0" = "1",
                       "1" = "2"))


Datosfiltrados$sexo<-factor(Datosfiltrados$sexo, levels = c(0,1), labels = c("Hombre","Mujer"))
Datosfiltrados$nivel_esc<-factor(Datosfiltrados$nivel_esc, levels = c(1,2), labels = c("Postgrado","Licenciatura"))
Datosfiltrados$lenguaje<-factor(Datosfiltrados$lenguaje, levels = c(1,2), labels = c("Español","Ingles"))
Datosfiltrados$puesto<-factor(Datosfiltrados$puesto, levels = c(1,2,3,4), labels = c("Alta dirección","Responsabilidad intermedia","Nivel inicial","Nivel básico"))
Datosfiltrados$carrera_cve<-factor(Datosfiltrados$carrera_cve, levels = c(1,2,3,4,5,6,7,8,9,10,11), labels = c("Educa","Artes y Hu","C. SocialesDerecho","Admin y negocios","C. Naturales, mate","Tecno. Info.","Ing.Manuf.Contr.","Agronomia y Vete","C. Salud","Servicios","Tecnico"))
Datosfiltrados$experiencia<-factor(Datosfiltrados$experiencia, levels = c(1,2), labels = c("Sin EXP.","Con EXP."))
Datosfiltrados$CategoríaIngresos<-factor(Datosfiltrados$CategoríaIngresos, levels = c(1,2,3,4,5,6), labels = c("Nada","Muy bajo","Bajo","Medio","Alto","Muy alto"))


#5. Ajuste del modelo


regresion <- glm(sexo ~ nivel_esc + lenguaje + puesto + carrera_cve + experiencia + CategoríaIngresos,
                 data = Datosfiltrados, family = "binomial")
summary(regresion)


#6. Interpretacion

momios<-exp(coefficients(regresion))%>%round(digits = 4)%>%data.frame()
momios
