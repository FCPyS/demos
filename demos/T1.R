###############################################################################-
# Taller 1: Cuestionario ampliado
# Autor: AREC
# Fecha: 20-06-23
############################################################################# -

## Previo ----

### Paquetes ----
  
if (!require("pacman")) install.packages("pacman") # instala pacman si se requiere
pacman::p_load(tidyverse,
               haven, 
               readr,
               foreign,
               janitor) #carga los paquetes necesarios 

## Importación de datos ----

# Recuerdo que se debe revisar la documentación 
# 
# https://www.inegi.org.mx/programas/ccpv/2020/#Microdatos 
#   
  
### Desde .csv ----
  
# Los archivos ".csv" son archivos separados por comas. Los podemos abrir con un bloc de notas para revisar
# 
# Hay dos comandos que nos permiten importar archivos de texto, el de base `read.csv()` y `readr::read_csv()`

viviendas01 <- read.csv(file = "data_t1/Viviendas01.CSV")


# revisemos los nombres y las primeres 6 líneas 

names(viviendas01)
head(viviendas01)

viviendas01 <- read_csv("data_t1/Viviendas01.CSV")

names(viviendas01)
head(viviendas01)

### Desde SAS ----

viviendas01 <- haven::read_sas("data_t1/viviendas01.sas7bdat")

names(viviendas01)
head(viviendas01)

### Desde .dta


viviendas01 <- haven::read_dta("data_t1/Viviendas01.dta")


### Desde .sav ----


viviendas01 <- haven::read_sav("data_t1/Viviendas01.sav")


## Fusionado de datos ----

# Para ello vamos a importar también la base de personas


personas01 <- haven::read_sav("data_t1/Personas01.SAV")

# Veamos las dimensiones de estas tablas


dim(viviendas01)
dim(personas01)


# Con el INEGI no hay problema, pero bien vale revisar los id sean únicos:
  
  
viviendas01 %>% 
  janitor::get_dupes(ID_VIV)


personas01 %>% 
  janitor::get_dupes(ID_PERSONA)



### Con `merge()` ----

#### Casos en ambas bases ----

# Por *default*, el comando tiene activado la opción "all = FALSE", que nos deja los datos de ambas bases comunes. (tipo una intersección)


cacenso01<-merge(x=viviendas01, 
                 y=personas01,
                 by="ID_VIV", 
                 all = F)
dim(cacenso01)

names(cacenso01)



#### Todos los casos ----

# Si cambiamos la opción "all = TRUE", que nos deja los datos comunes a ambas bases. (como una unión)


cacenso01<-merge(viviendas01,
                 personas01, 
                 by="ID_VIV", 
                 all = T)
dim(cacenso01)


#### Casos en la base 1 ----

# Si queremos quedarnos con todos los datos que hay en la primera base, x, vamos a usar a opción all.x = TRUE.


cacenso01<-merge(viviendas01, 
                 personas01,
                 by="ID_VIV", 
                 all.x  = TRUE)
dim(cacenso01)


#### Casos de la base 2 ----

# Notamos que hoy sí tenemos los datos de toda la población y hay missings en las variables aportadas por la base de trabajo
 
# Si queremos lo contrario, quedarnos con los datos aportados por la segunda base, y, vamos a usar la opción all.y=TRUE


cacenso01<-merge(viviendas01, 
                 personas01,
                 by="ID_VIV",
                 all.y  = TRUE)
dim(cacenso01)


### Con `{dplyr}` ----

# El caso 1:
  
  
cacenso01<-dplyr::inner_join(viviendas01,
                             personas01,
                             by="ID_VIV")

cacenso01<-viviendas01 %>% # tabla left
  dplyr::inner_join(personas01, by="ID_VIV") # tabla right

dim(cacenso01)

names(cacenso01)

# El caso 2:
  
  
cacenso01<-dplyr::full_join(viviendas01, 
                            personas01, 
                            by="ID_VIV")
dim(cacenso01)



# El caso 3:
  
  
cacenso01<-dplyr::left_join(viviendas01,
                            personas01, 
                            by="ID_VIV")
dim(cacenso01)



# El caso 4:
  
  
cacenso01<-dplyr::right_join(viviendas01, 
                             personas01,
                             by="ID_VIV")
dim(cacenso01)

dplyr::glimpse(cacenso01$JEFE_SEXO)

cacenso01<-merge(viviendas01, personas01, 
                 by=intersect(names(viviendas01), names(personas01)), # ojo esto tendriamos que verificar
                 suffixes = c("_v", "_p"))
names(cacenso01)

dplyr::glimpse(cacenso01$JEFE_SEXO)

# También se puede usar con pipes, cualquier opción de dplyr


cacenso01<-viviendas01 %>% # pongo el conjunto que será la "izquierda
  dplyr::right_join(personas01, 
                    by="ID_VIV")
dim(cacenso01)

## Actividad: ----- 

## Hacer el merge de TLAXCALA



## Agregar casos

# Supongamos que queremos pegar la información de viviendas de Aguascalientes y Tlaxcala


viviendas29 <- read_sav("data_t1/Viviendas29.SAV")
personas29 <- read_sav("data_t1/Personas29.SAV")

cacenso29<-merge(viviendas29, personas29, 
                 by="ID_VIV")

### Con `rbind()` ----


cacenso01_29<-rbind(cacenso01, cacenso29)
# Aquí las tablas tienen que tener exactamente los mismos nombres


table(viviendas01_29$ENT)



### Con `dplyr::bind_rows`

cacenso01_29<-dplyr::bind_rows(cacenso01, cacenso29)
# Aqui es más libre

table(cacenso01_29$ENT.x)
