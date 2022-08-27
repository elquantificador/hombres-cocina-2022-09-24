## --------------------------------------------------------------------------- ##
## ------------------------------ Librerías ---------------------------------- ##
## --------------------------------------------------------------------------- ##

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(haven)) install.packages("haven", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(radiant.data)) install.packages("radiant.data", repos = "http://cran.us.r-project.org")
if(!require(foreign)) install.packages("foreign", repos = "http://cran.us.r-project.org")
if(!require(patchwork)) install.packages("patchwork", repos = "http://cran.us.r-project.org")
if(!require(survey)) install.packages("survey", repos = "http://cran.us.r-project.org")
if(!require(srvyr)) install.packages("srvyr", repos = "http://cran.us.r-project.org")


## --------------------------------------------------------------------------- ##
## ---------------------------- Bases de datos ------------------------------- ##
## --------------------------------------------------------------------------- ##

# 2012

data_UT2012 <- read_sav(EUT2012.f.path)


# 2019

data_UT2019 <- read_csv2("https://raw.githubusercontent.com/christianjaviersalasmarquez/ut-ec-hombres-cocina/main/data/201912_multibdd_uso_del_tiempo.sav.csv") # read_csv2 utiliza ; como separador y , para punto decimal

data_personas2019 <- read_csv2("https://raw.githubusercontent.com/christianjaviersalasmarquez/ut-ec-hombres-cocina/main/data/201912_multibdd_personas.sav.csv") # read_csv2 utiliza ; como separador y , para punto decimal


## --------------------------------------------------------------------------- ##
## ------------------------- Data Wrangling 2012 ----------------------------- ##
## --------------------------------------------------------------------------- ##

# UT15: En la semana pasada: ¿Cocinó o preparó alimentos para consumirse en el desayuno, almuerzo, merienda, entre comidas, incluyendo comidas para llevar?

UT15_UT2012 <- data_UT2012 %>%
  transmute(ciudad = CIUDAD,
            sexo = haven::as_factor(P02),
            edad = P03,
            UT15 = haven::as_factor(UT15), # ¿Cocinó alimentos? Si o No
            UT15A = UT15A, # Horas dedicadas (lunes a viernes)
            UT15B = UT15B, # Minutos dedicados (lunes a viernes)
            UT15C = UT15C, # Horas dedicadas (sábado y domingo)
            UT15D = UT15D, # Minutos dedicados (sábado y domingo)
            dominio = dominio, # Estrato
            id_upm = id_upm, # Unidad primaria de muestreo
            fexp = fexp # Factores de expansión
            ) %>% 
  as_tibble()


# Cálculo del total de horas semanales en cocinar o preparar alimentos

UT15_UT2012[c('UT15A','UT15B','UT15C','UT15D')] <- sapply(UT15_UT2012[c('UT15A','UT15B','UT15C','UT15D')], FUN = function(x) ifelse(is.na(x), 0, x))

UT15_UT2012$t_horas_cocina <- UT15_UT2012$UT15A + 
  (UT15_UT2012$UT15B/60) + 
  UT15_UT2012$UT15C + 
  (UT15_UT2012$UT15D/60)


# Variable de grupos de edad

# 98: 98 y más
# 99: No informa

UT15_UT2012 <- UT15_UT2012 %>% 
  mutate(edad_rango = case_when(edad <= 11 ~ 'Edad entre 0 y 11 años',
                                between(edad, 12, 19) ~ 'Edad entre 12 y 19 años',
                                between(edad, 20, 29) ~ 'Edad entre 20 y 29 años',
                                between(edad, 30, 39) ~ 'Edad entre 30 y 39 años',
                                between(edad, 40, 49) ~ 'Edad entre 40 y 49 años',
                                between(edad, 50, 59) ~ 'Edad entre 50 y 59 años',
                                between(edad, 60, 69) ~ 'Edad entre 60 y 69 años',
                                edad >= 70 ~ 'Edad mayor a 70 años',
                                edad == 99 ~ 'No informa')) %>%
  mutate(edad_rango = as_factor(edad_rango))


# Variable de provincia

# Para la variable 'ciudad', el INEC maneja un código de 6 dígitos, donde el 1er y 2do dígito forman el Código de Provincia, el 3er y 4to dígito forman el Código de Cantón y el 5to y 6to dígito forman el Código de Parroquia. Por ejemplo, en el código 010150, 01 indica la provincia de Azuay, 0101 indica el cantón Cuenca y 010150 indica la parroquia 'Cuenca'
# No existen registros para Galápagos en el dataset del 2012

UT15_UT2012 <- UT15_UT2012 %>% 
  mutate(prov = ifelse(nchar(ciudad) == 5, paste("0", ciudad, sep = ""), ciudad)) %>%
  mutate(prov = substr(prov,1,2)) %>% 
  mutate(prov = as_factor(prov))

levels(UT15_UT2012$prov) <- 
  c('Azuay','Bolívar','Cañar','Carchi','Cotopaxi','Chimborazo','El Oro','Esmeraldas',
    'Guayas','Imbabura','Loja','Los Ríos','Manabí','Morona Santiago','Napo','Pastaza',
    'Pichincha','Tungurahua','Zamora Chinchipe','Sucumbíos','Orellana',
    'Santo Domingo de los Tsáchilas','Santa Elena','Zonas no delimitadas')


## --------------------------------------------------------------------------- ##
## ------------------------- Data Wrangling 2019 ----------------------------- ##
## --------------------------------------------------------------------------- ##

# s51p1: ¿Participa en los quehaceres de su hogar?
# s51p2: La semana pasada, ¿Usted cocinó o preparó alimentos?

# Merge de la base de datos de la sección de uso del tiempo con la sección de personas.

df_info_personas2019 <- data.frame(id_per = data_personas2019$id_per, 
                                   s1p2 = data_personas2019$s1p2, 
                                   s1p3 = data_personas2019$s1p3)

df_analisis_UT2019 <- merge(df_info_personas2019, 
                            data_UT2019, 
                            by = "id_per")

S51P2_UT2019 <- df_analisis_UT2019 %>%
  transmute(id_per = id_per,
            ciudad = ciudad,
            sexo = fct_recode(factor(s1p2), "Hombre"="1","Mujer"="2" ),
            edad = s1p3,
            s51p1 = fct_recode(factor(s51p1), "Si"="1","No"="2" ), # ¿Participa en los quehaceres de su hogar? Si o No
            s51p2 = fct_recode(factor(s51p2), "Si"="1","No"="2" ), # ¿Cocinó alimentos? Si o No
            s51p2a = s51p2a, # Horas dedicadas (lunes a viernes)
            s51p2b = s51p2b, # Minutos dedicados (lunes a viernes)
            s51p2c = s51p2c, # Horas dedicadas (sábado y domingo)
            s51p2d = s51p2d, # Minutos dedicados (sábado y domingo)
            conglomerado = conglomerado,
            estrato = estrato, 
            upm = upm, # Unidad primaria de muestreo
            fexp = fexp # Factores de expansión
  ) %>%
  as_tibble()


# Cálculo del total de horas semanales en cocinar o preparar alimentos

S51P2_UT2019[c('s51p2a','s51p2b','s51p2c','s51p2d')] <- sapply(S51P2_UT2019[c('s51p2a','s51p2b','s51p2c','s51p2d')], FUN = function(x) ifelse(is.na(x), 0, x))

S51P2_UT2019$t_horas_cocina <- (S51P2_UT2019$s51p2a) + 
  (S51P2_UT2019$s51p2b/60) + 
  (S51P2_UT2019$s51p2c) + 
  (S51P2_UT2019$s51p2d/60)


# Variable de grupos de edad

S51P2_UT2019 <- S51P2_UT2019 %>% 
  mutate(edad_rango = case_when(edad <= 11 ~ 'Edad entre 0 y 11 años',
                                between(edad, 12, 19) ~ 'Edad entre 12 y 19 años',
                                between(edad, 20, 29) ~ 'Edad entre 20 y 29 años',
                                between(edad, 30, 39) ~ 'Edad entre 30 y 39 años',
                                between(edad, 40, 49) ~ 'Edad entre 40 y 49 años',
                                between(edad, 50, 59) ~ 'Edad entre 50 y 59 años',
                                between(edad, 60, 69) ~ 'Edad entre 60 y 69 años',
                                edad >= 70 ~ 'Edad mayor a 70 años',
                                edad == 99 ~ 'No informa')) %>%
  mutate(edad_rango = as_factor(edad_rango))


# Variable de provincia

S51P2_UT2019$prov <- as.factor(substr(df_analisis_UT2019$ciudad,start = 1, stop = 2))

levels(S51P2_UT2019$prov) <- c('Azuay','Bolívar','Cañar','Carchi','Cotopaxi','Chimborazo','El Oro','Esmeraldas',
                               'Guayas','Imbabura','Loja','Los Ríos','Manabí','Morona Santiago','Napo','Pastaza',
                               'Pichincha','Tungurahua','Zamora Chinchipe','Galápagos','Sucumbíos','Orellana',
                               'Santo Domingo de los Tsáchilas','Santa Elena','Zonas no delimitadas')

