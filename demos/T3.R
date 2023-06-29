###############################################################################-
# Taller 3: ENDIREH
# Autor: AREC
# Fecha: 27-06-23
############################################################################# -

## Previo ----

### Descarga el proyecto desde acá ----

# En esta liga puedes descarga el proyecto de trabajo. De esta manera no tendremos problemas con las rutas relativas.
# 
# <https://tinyurl.com/demos-talleres>

### Paquetes ----

if(!require("pacman")) install.packege("pacman")

pacman::p_load(tidyverse, skimr, janitor, magrittr, sjlabelled)


## Funciones ----

mi_funcion <- function(x) {
  resultado <- x + 1
  return(resultado)
}

mi_funcion(x=4)


mi_funcion <- function(x, a) {
  resultado <- x + a
  return(resultado)
}

mi_funcion(x=5, a=10)
mi_funcion(5,10)


## Bucles ----

for (i in 1:10) {
  print(i+1)
}


for (j in c("Hugo", "Paco", "Luis")) {
  
  x <-paste("hola", j, sep=", " )
  print(x)
}


a <- "Hola"
b <- "¿Cómo estás"

## Importación de datos ----

TVIV <- readr::read_csv("data_t3/conjunto_de_datos_endireh_2021_csv/conjunto_de_datos_TVIV/conjunto_de_datos/conjunto_de_datos_TVIV.csv")

cd0 <- "data_t3/conjunto_de_datos_endireh_2021_csv/conjunto_de_datos_"
cd1 <- "/conjunto_de_datos/conjunto_de_datos_"

paste0(cd0,"TVIV",cd1,"TVIV",".csv")

### Funcion para importar 1 ----

importar <- function(tabla) {
  
  ruta <-paste0(cd0, tabla ,cd1, tabla,".csv")
  x <- readr::read_csv(ruta)
  return(x)
  
}

TVIV<-importar("TVIV")
TB_SEC_XIV<-importar("TB_SEC_XIV")


# Importando el indice

indice_tablas <- read_csv("data_t3/conjunto_de_datos_endireh_2021_csv/0_indice_tablas_ENDIREH_2021.csv",
locale = locale(encoding = "latin1"),
skip = 1) %>% clean_names()

indice_tablas %<>%
  mutate(nombre_de_archivo=stringr::str_remove_all(nombre_de_archivo, "conjunto_de_datos_"))
head(indice_tablas)

tablas<-indice_tablas$nombre_de_archivo[c(1:4, 9)]

## Bucle para la importación ----

for (i in tablas) {
  
  y <- importar(i)
  
  assign(paste(i), y)
}

rm(y)

## Importando diccionarios -----

cd0 <- "data_t3/conjunto_de_datos_endireh_2021_csv/conjunto_de_datos_"
cd1 <- "/conjunto_de_datos/conjunto_de_datos_"

paste0(cd0,"TVIV","/","diccionario_de_datos","/", "diccionario_de_datos", "TVIV", ".csv"  )

### Funcion para importar 1 ----

importar2 <- function(tabla, elemento) {
  
  ruta <-paste0(cd0,tabla,"/",elemento,"/", elemento,"_", tabla, ".csv"  )
  x <- readr::read_csv(ruta, locale = locale(encoding = "latin1"))
  return(x)
  
}

dicc<-importar2(tabla="TVIV", elemento="diccionario_de_datos")

## Bucles para diccionarios ----


for (i in tablas) {
  
  y <- importar2(tabla=i, elemento = "diccionario_de_datos") %>% 
    select(NOMBRE_CAMPO:TIPO) %>% 
    unique()
  
  assign(paste0("DICC_", i), y)
}

## Etiquetado de las variables

DICC_TB_SEC_III$NEMONICO
names(TB_SEC_III)

DICC_TB_SEC_III[DICC_TB_SEC_III$NEMONICO=="ID_VIV", ]$NOMBRE_CAMPO

TB_SEC_III[["ID_VIV"]]

for (i in names(TB_SEC_III)) {
  
  TB_SEC_III[[i]]<-sjlabelled::set_label(TB_SEC_III[[i]], 
                                         label=DICC_TB_SEC_III[DICC_TB_SEC_III$NEMONICO==i, ]$NOMBRE_CAMPO)

}

for (i in names(TB_SEC_IV)) {
 
   
  TB_SEC_IV[[i]]<-sjlabelled::set_label(TB_SEC_IV[[i]], 
                                         label=DICC_TB_SEC_IV[DICC_TB_SEC_IV$NEMONICO==i, ]$NOMBRE_CAMPO)
  
}

## Etiquetado de valores de variables 

dir("data_t3/conjunto_de_datos_endireh_2021_csv/conjunto_de_datos_TB_SEC_III/catalogos")

cd3<-paste0(cd0,"TB_SEC_III","/catalogos")

paste0(cd3,"/", "P3_1", ".csv")

vars<-dir(cd3) %>% stringr::str_remove_all(".csv")


for (i in vars[14:21]) {
  
  x<-read_csv(paste0(cd3,"/", i, ".csv"), locale = locale(encoding = "latin1"))
  
  TB_SEC_III[[i]]<-as.numeric(TB_SEC_III[[i]])
  
  TB_SEC_III[[i]]<-sjlabelled::set_labels(TB_SEC_III[[i]], 
                                          labels=x$descrip)
  
}

TB_SEC_III %>% 
  mutate(P3_3=as_label(P3_3)) %>% 
  janitor::tabyl(P3_3)


## Sección IV


cd3<-paste0(cd0,"TB_SEC_IV","/catalogos")

vars<-dir(cd3) %>% stringr::str_remove_all(".csv")


for (i in vars[15:72]) {
  
  x<-read_csv(paste0(cd3,"/", i, ".csv"), locale = locale(encoding = "latin1"))
  
  TB_SEC_IV[[i]]<-as.numeric(TB_SEC_IV[[i]])
  
  TB_SEC_IV[[i]]<-sjlabelled::set_labels(TB_SEC_IV[[i]], 
                                         labels=x$descrip)
  
}

 
dplyr::glimpse(TB_SEC_IV$P4_9_2)


rm(list=ls(pattern = "^DICC"))
