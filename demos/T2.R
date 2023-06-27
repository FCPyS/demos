###############################################################################-
# Taller 2: ENDISEG
# Autor: AREC
# Fecha: 23-06-23
############################################################################# -

## Previo ----

### Descarga el proyecto desde acá ----

# En esta liga puedes descarga el proyecto de trabajo. De esta manera no tendremos problemas con las rutas relativas.
# 
# <https://tinyurl.com/demos-talleres>

### Paquetes ----

if (!require("pacman")) install.packages("pacman") # instala pacman si se requiere
pacman::p_load(tidyverse,
 haven, 
 readr,
 foreign,
 janitor,
 magrittr,
 pollster,
 srvyr,
 sjlabelled) #carga los paquetes necesarios 


## Introducción a la fuente ----


idviv<-c("folio", "viv_sel")
idper<-c("folio", "viv_sel", "hogar", "n_ren")


## Importación de los datos ----

tvivienda <- readr::read_csv("data_t2/data_t2/conjunto_de_datos_endiseg_2021_csv/conjunto_de_datos_tvivienda_endiseg_2021/conjunto_de_datos/conjunto_de_datos_tvivienda_endiseg_2021.csv") %>%
  janitor::clean_names()

names(tvivienda)


tsdem <- readr::read_csv("data_t2/data_t2/conjunto_de_datos_endiseg_2021_csv/conjunto_de_datos_tsdem_endiseg_2021/conjunto_de_datos/conjunto_de_datos_tsdem_endiseg_2021.csv") %>%
  janitor::clean_names()

tmodulo <- readr::read_csv("data_t2/data_t2/conjunto_de_datos_endiseg_2021_csv/conjunto_de_datos_tmodulo_endiseg_2021/conjunto_de_datos/conjunto_de_datos_tmodulo_endiseg_2021.csv") %>%
  janitor::clean_names() %>% 
  dplyr::mutate(seleccionade=1)

tapart_a <- readr::read_csv("data_t2/data_t2/conjunto_de_datos_endiseg_2021_csv/conjunto_de_datos_tapart_a_endiseg_2021/conjunto_de_datos/conjunto_de_datos_tapart_a_endiseg_2021.csv") %>%
  janitor::clean_names()

tapart_b <- readr::read_csv("data_t2/data_t2/conjunto_de_datos_endiseg_2021_csv/conjunto_de_datos_tapart_b_endiseg_2021/conjunto_de_datos/conjunto_de_datos_tapart_b_endiseg_2021.csv") %>% 
  janitor::clean_names()



## Fusionado uno a uno con diferentes conjuntos ----


endiseg2021<-tsdem %>% # left o x
  dplyr::left_join(tmodulo, by=idper) %>% 
  mutate(fac_per=factor.x) %>% # factor que venía en el modulo de tsdem
  mutate(fac_ele=factor.y) %>%   # factor de la persona elegida
  select(-c(factor.x, factor.y)) %>% #bota las variables
  dplyr::select(-ends_with(".y")) %>% # quita todas las variables que terminan en .y
  dplyr::rename_with(~ stringr::str_remove(.x, pattern = ".x"), ends_with(".x")) 


endiseg2021<-endiseg2021 %>% 
dplyr::left_join(tapart_a, by=idper)

endiseg2021<-endiseg2021%>% 
  select(-factor) %>%   
  dplyr::select(-ends_with(".y")) %>% # quita todas las variables que terminan en .y
  dplyr::rename_with(~ stringr::str_remove(.x,pattern = ".x"),ends_with(".x")) 


endiseg2021<-endiseg2021 %>% 
dplyr::left_join(tapart_b, by=idper) %>% 
  select(-factor) %>%   
  dplyr::select(-ends_with(".y")) %>% # quita todas las variables que terminan en .y
  dplyr::rename_with(~ stringr::str_remove(.x,pattern = ".x"),ends_with(".x")) 


endiseg2021<-tvivienda %>% 
dplyr::left_join(endiseg2021, by=idviv) #ojo cambiamos acá

names(endiseg2021)
endiseg2021 %>% 
  select(starts_with("fac"))

endiseg2021 %<>% 
rename(fac_viv=factor) %>% 
dplyr::select(-ends_with(".y")) %>% # quita todas las variables que terminan en .y
dplyr::rename_with(~ stringr::str_remove(.x,pattern = ".x"),ends_with(".x")) 


rm(tvivienda, tsdem, tmodulo, tapart_a, tapart_b)
gc() # limpiamos la memoria


## Etiquetado de variables ----

### Diccionario de variables ----

dicc_tmodulo <- read_csv("data_t2/data_t2/conjunto_de_datos_endiseg_2021_csv/conjunto_de_datos_tmodulo_endiseg_2021/diccionario_de_datos/diccionario_datos_tmodulo_endiseg_2021.csv",
col_types = cols(...7 = col_skip(), ...8 = col_skip(),...9 = col_skip())) %>% 
  clean_names()

dicc_tmodulo<-dicc_tmodulo %>% 
mutate(nombre_campo=stringr::str_remove_all(nombre_campo, "Pregunta "))

dicc_tmodulo %<>% janitor::clean_names()

# Pensemos que queremos saber cuál es el título de variable p7


dicc_tmodulo %>% 
filter(nemonico=="p7_1") %>% 
select(nombre_campo)


# Vamos a guardar esto


dicc_tmodulo %>% 
filter(nemonico=="p7_1") %>% 
select(nombre_campo) -> label_p7_1


# Con este proceso se puede automatizar un poco:


endiseg2021 %<>% 
mutate(p7_1=sjlabelled::set_label(p7_1, label=label_p7_1$nombre_campo))



dicc_tmodulo %>% 
  filter(nemonico=="p4_9c") %>% 
  select(nombre_campo) -> label_p4_9c
# Con este proceso se puede automatizar un poco:
endiseg2021 %<>% 
  mutate(p4_9c=sjlabelled::set_label(p4_9c, label=label_p4_9c$nombre_campo))


# Revisamos que tengamos el atributo


dplyr::glimpse(endiseg2021$p7_1)


### Catalogos de valores ----

# Estos nos servirán para etiquetas los valores de las variables. 
# Hay un catálogo por variable. Sigamos con la variable p7_1


labels_p7_1 <- read_csv("data_t2/data_t2/conjunto_de_datos_endiseg_2021_csv/conjunto_de_datos_tmodulo_endiseg_2021/catalogos/p7_1.csv") %>%
  clean_names()


# Vamos a etiquetar hoy los valores


endiseg2021 %<>% 
mutate(p7_1=sjlabelled::set_labels(p7_1, labels = labels_p7_1$descrip))

# Revisamos que tengamos el atributo


glimpse(endiseg2021$p7_1)



# Etiquetado de variable ----
dicc_tmodulo %>% 
  filter(nemonico=="p8_1") %>% 
  select(nombre_campo) -> label_p8_1

# Con este proceso se puede automatizar un poco:

endiseg2021 %<>% 
  mutate(p8_1=sjlabelled::set_label(p8_1, label=label_p8_1$nombre_campo))

# etiquetado de valores ----

labels_p8_1 <- read_csv("data_t2/data_t2/conjunto_de_datos_endiseg_2021_csv/conjunto_de_datos_tmodulo_endiseg_2021/catalogos/p8_1.csv",
                        locale = locale(encoding = "latin1")) %>%  clean_names()

endiseg2021 %<>% 
  mutate(p8_1=sjlabelled::set_labels(p8_1, labels = labels_p8_1$descrip))

glimpse(endiseg2021$p8_1)


## Tabulados ----

### Tabulados con {janitor} ----

# Sin factores de expansión este es un gran comando.

endiseg2021 %>% 
  filter(seleccionade==1) %>% 
  mutate(p7_1=as_label(p7_1)) %>% 
  janitor::tabyl(p7_1) %>% 
  janitor::adorn_totals()


endiseg2021 %>% 
  filter(seleccionade==1) %>% 
  mutate(p7_1=as_label(p7_1)) %>% 
  janitor::tabyl(p7_1) %>% 
  janitor::adorn_totals() %>% 
  janitor::adorn_pct_formatting(digits=2)
  

#Vamos a utilizar una segunda variable



endiseg2021 %>% 
  filter(seleccionade==1) %>% 
  mutate(p7_1=as_label(p7_1)) %>% 
  mutate(p8_1=as_label(p8_1)) %>% 
  janitor::tabyl(p8_1, p7_1) %>% 
  janitor::adorn_totals(where=c("row", "col")) %>%  
  janitor::adorn_percentages("all")  %>%  # "row" "all"
  janitor::adorn_pct_formatting(digits=2)


### Tabulados con {pollster} ----

#Podemos incluir el factor de expansión


endiseg2021 %>% 
  select(starts_with("fac"))

endiseg2021 %>% 
  filter(seleccionade==1) %>% 
  mutate(p7_1=as_label(p7_1)) %>% 
  pollster::topline(p7_1, weight = fac_ele) 


endiseg2021 %>% 
  filter(seleccionade==1) %>% 
  mutate(p8_1=as_label(p8_1)) %>% 
  pollster::topline(p8_1, weight = fac_ele) 


endiseg2021 %>% 
filter(seleccionade==1) %>% 
mutate(p7_1=as_label(p7_1)) %>% 
pollster::moe_topline(p7_1, weight = fac_ele) # margin of error


endiseg2021 %>% 
  filter(seleccionade==1) %>% 
  mutate(p8_1=as_label(p8_1)) %>% 
  pollster::moe_topline(p8_1, weight = fac_ele) # margin of error _ supone muestreo aleatorio simple

###  exportación de tabulados ----
tabulado<-endiseg2021 %>% 
  filter(seleccionade==1) %>% 
  mutate(p8_1=as_label(p8_1)) %>% 
  pollster::moe_topline(p8_1, weight = fac_ele) # margin of error _ supone muestreo aleatorio simple

writexl::write_xlsx(tabulado, path = "tabulado.xlsx")



## Doble entrada ----

endiseg2021 %>% 
  filter(seleccionade==1) %>% 
  mutate(p7_1=as_label(p7_1)) %>% 
  mutate(p8_1=as_label(p8_1)) %>% 
  pollster::crosstab(p8_1, p7_1, weight = fac_ele, pct_type = "cell") # row, col y cell



endiseg2021 %>% 
  filter(seleccionade==1) %>% 
  mutate(p7_1=as_label(p7_1)) %>% 
  mutate(p8_1=as_label(p8_1)) %>% 
  pollster::moe_crosstab(p8_1, p7_1, weight = fac_ele) # distribución de la segunda variable

endiseg2021 %>% 
  filter(seleccionade==1) %>% 
  mutate(p7_1=as_label(p7_1)) %>% 
  mutate(p8_1=as_label(p8_1)) %>% 
  pollster::moe_crosstab(p7_1, p8_1, weight = fac_ele) # distribución de la segunda variable



## Diseño muestral ----


endiseg_svyr <- endiseg2021 %>%
  filter(seleccionade==1) %>% 
  select(upm_dis, est_dis, fac_ele, seleccionade, p7_1, p8_1, p4_1) %>% 
  srvyr::as_survey_design(
  upm = upm_dis, 
  strata = est_dis,
  weights = fac_ele,
  nest = TRUE)


# Para una media ponderada


endiseg_svyr %>%
  group_by(as_label(p8_1)) %>% 
  summarise(media_ponderada = survey_mean(p4_1, na.rm=T, vartype=c("se", "ci", "cv")))



#Si queremos los intervalos de confianza:


endiseg_svyr %>%
filter(seleccionade==1) %>% 
summarise(
media_ponderada = survey_mean(p4_1, na.rm=T, vartype="ci"))



#Esto se puede tardar


endiseg_svyr %>%
filter(seleccionade==1) %>% 
mutate(p8_1=as_label(p8_1)) %>% 
group_by(p8_1) %>% 
summarise(
media_ponderada = survey_mean(p4_1, na.rm=T, vartype="ci"))




endiseg_svyr %>%
filter(seleccionade==1) %>% 
mutate(p8_1=as_label(p8_1)) %>% 
group_by(p8_1) %>% #variables cuali
summarise(proportion = survey_mean(vartype=c("se", "ci", "cv")))

