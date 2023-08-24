


# Manipulacion data -----------------------------------------------




# Data main ---------------------------------------------------------------

# crear nuevas variables

## empezaremos por el tiempo

#recorrido nocturno 9 pm hasta terminar
# recorrido mañana de 5 am hasta terminar
mañana <- c(3:19)

#mañana <- c(4:16)
#noche <- c(17:24,0:3)

dfobservaciones <- df1 %>% 
  mutate(AÑO = year(FECHA),
         MES = month(FECHA,label = TRUE, abbr = FALSE),
         DIA = mday(FECHA),
         DIAYEAR = yday(FECHA),
         DIAWEEK = wday(FECHA, label = TRUE, abbr = FALSE),   
         SEMANA = week(FECHA),
         TURNO = ifelse(hour(HORA) %in% mañana, "mañana", "noche"),
         OBSERVACION_TF = ifelse(OBSERVACION_TF == "TRUE", TRUE, FALSE),
         TEMPORADA = ifelse(TEMPORADA == "TRANSICIÓN DE LLUVIAS A SECAS"|
                              TEMPORADA == "TRANSICION DE SECAS A LLUVIA", 
                            "TRANSICION",
                            TEMPORADA)
  ) %>% 
  select(FECHAF, AÑO, MES, SEMANA, DIA, TURNO, TRANSECTO, SUBTRANSECTO, everything())

dfobservaciones
view(dfobservaciones)
view(df1)



# Separar informacion  -----------------------------------------------------


# df recorridos -----------------------------------------------------------


# Separar observaciones y recorridos

# crear data set de los recorridos realizados
dfrecorridos <- dfobservaciones %>% 
  group_by(RECORRIDO) %>% 
  summarise(MIN = max(MIN, na.rm = T),
            OBSERVACIONES = mfv1(OBSERVACIONES, na_rm = T),
            TURNO = mfv(TURNO, na_rm = T),
            AÑO = mfv1(AÑO, na_rm = T),
            MES = mfv1(MES, na_rm = T),
            SEMANA = mfv1(SEMANA, na_rm = T),
            DIA = mfv1(DIA, na_rm = T),
            MOST_TRANSECTO = mfv1(TRANSECTO, na_rm = T),
            MOST_SUBTRANSECTO = mfv1(SUBTRANSECTO, na_rm = T),
            MOST_CLASE = mfv1(CLASE, na_rm = F),
            MOST_GENERO = mfv1(GENERO, na_rm = F),
            MOST_ESPECIE = mfv1(ESPECIE, na_rm = F),
            N_INVESTIGATOR = mfv1(PERSONAS, na_rm = T),
            DEG_MEAN = mean(DEG, trim = 0.05, na.rm = T),
            CHILL_MEAN = mean(CHIL, trim = 0.05, na.rm = T),
            RH_MEAN = mean(RH_C, trim = 0.05, na.rm = T),
            HI_MEAN = mean(HI, trim = 0.05, na.rm = T),
            DP_MEAN = mean(DP, trim = 0.05, na.rm = T),
            BULB_MEAN = mean(BULB, trim = 0.05, na.rm = T),
            BARO_MEAN = mean(BARO, trim = 0.05, na.rm = T),
            ALT_MEAN = mean(ALT, trim = 0.05, na.rm = T),
            SPD_MEAN = mean(SPD, trim = 0.05, na.rm = T),
            TC_MEAN = mean(TC, trim = 0.05, na.rm = T),
            TS_MEAN = mean(TS, trim = 0.05, na.rm = T),
            LHC_MEAN = mean(LHC, trim = 0.05, na.rm = T),
            L_COLA_MEAN = mean(LONG_COLA, trim = 0.05, na.rm = T),
            L_TOTAL_MEAN = mean(LONG_TOTAL, trim = 0.05, na.rm = T),
            PESO_MEAN = mean(PESO, trim = 0.05, na.rm = T),
            T_ASF_MEAN = mean(TEMP_ASF, trim = 0.05, na.rm = T),
            T_SUELO_MEAN = mean(TEMP_SUELO, trim = 0.05, na.rm = T),
            DIRECCION = mfv1(DIRECCION, na_rm = F),
            UBICACION_T = mfv1(UBICACION_T, na_rm = F),
            CONDICION = mfv1(CONDICION, na_rm = F),
            VIVO = mfv1(VIVO, na_rm = T),
            MUERTO = mfv1(MUERTO, na_rm = T),
            CARROS_TUX = mfv1(CARROS_TUX, na_rm = T),
            CARROS_OAX = mfv1(CARROS_OAX, na_rm = T),
            TOTAL_CARROS = mfv1(TOTAL_CARROS, na_rm = T),
            TEMPORADA = mfv1(TEMPORADA, na_rm = T),
            CAÑADA = mfv1(CAÑADA, na_rm = T),
            ESCURRIMIENTOS = mfv1(ESCURRIMIENTOS, na_rm = T),
            REGISTROS = sum(REGISTROS, na.rm = T)
  )
view(dfrecorridos)

# reemplazo NaN 
dfrecorridos <- dfrecorridos %>% 
  mutate(across(.fns = ~replace(., is.nan(.), NA)))
view(dfrecorridos)
view(df1)
# 
# AllOk 



# df observaciones --------------------------------------------------------

# Data set observaciones
index <- dfobservaciones$OBSERVACION_TF == TRUE
index
dfobservaciones <- dfobservaciones[index,]
view(dfobservaciones)

dfobservaciones <- dfobservaciones %>% 
  mutate(across(c(SUBTRANSECTO), .fns = ~replace(.x, .x == 0, NA)))
view(dfobservaciones)

# AllOk











# Data to GLM -------------------------------------------------------------


# enviromental distance ---------------------------------------------------

view(df2)
dfenvi <- df2 %>% 
  group_by(SUBTRANSECTO) %>% 
  summarise(across(.cols = ALTITUD:DIST_URBANIZACION,
                   .fns = list(MEAN=mean, MEDIAN=median),
                   ... = list(na.rm = TRUE),
                   .names = "{col}_{fn}"),
            TRANSECTO = max(TRANSECTO)) %>% 
  select(TRANSECTO, everything())

view(dfenvi)

# AllOk




# LandSat indices ---------------------------------------------------------

dfland1 <- df3 %>% 
  group_by(SUBTRANSECTO) %>% 
  summarise(
    across(.cols = NDWI_AGUA:LST_TEMPERATURA, 
           .fns = list(MEAN=mean, MEDIAN=median),
           ... = list(na.rm=TRUE, trim=0.05),
           .names = "{col}_{fn}"),
    TRANSECTO = max(TRANSECTO)
  ) %>% 
  select(TRANSECTO, everything())
view(dfland1)

# AllOk






# LandSat indices2 --------------------------------------------------------

tail(df4)

dfland2 <- df4 %>% 
  group_by(SUBTRANSECTO) %>% 
  summarise(
    across(.cols = NDWI_AGUA:LST_TEMPERATURA, 
           .fns = list(MEAN=mean, MEDIAN=median),
           ... = list(na.rm=TRUE, trim=0.05),
           .names = "{col}_{fn}"),
    TRANSECTO = max(TRANSECTO)
  ) %>% 
  select(TRANSECTO, everything())
view(dfland2)

# AllOk








# LandSat indices 3 -------------------------------------------------------
# nuevos datos enviados de ultimo momento

tail(df5)

dfland3 <- df5 %>% 
  group_by(SUBTRANSECTO) %>% 
  summarise(
    across(.cols = NDWI_AGUA:LST_TEMPERATURA, 
           .fns = list(MEAN=mean, MEDIAN=median),
           ... = list(na.rm=TRUE, trim=0.05),
           .names = "{col}_{fn}"),
    TRANSECTO = max(TRANSECTO)
  ) %>% 
  select(TRANSECTO, everything())
view(dfland3)

# AllOk



#creo que son exactamente iguales los nuevos datos

identical(dfland2, dfland3)
#true
# supongo que solo era confusion por el orden en que estaban


# AllOk















# deprecated:
# manually split data
# 
# df subtransectos global -------------------------------------------------

view(dfobservaciones)
str(dfobservaciones)


dfsub <- dfobservaciones %>% 
  group_by(SUBTRANSECTO) %>% 
  summarise(
    CONTEO = n(),
    N_INVESTIGATOR = mfv1(PERSONAS, na_rm = T),
    across(.cols = c(AÑO:TRANSECTO, DIAYEAR, DIAWEEK, RECORRIDO, CLASE:ESPECIE,
                     DIRECCION:MUERTO, TEMPORADA, CAÑADA, ESCURRIMIENTOS),
           .fns = list(MOST = ~mfv1(.x, na_rm = TRUE)),
           .names = "{.col}"),
    across(.cols = c(ALTITUD, DEG:TEMP_SUELO, CARROS_TUX:TOTAL_CARROS),
           .fns = list(promedio = ~mean(x = .x, trim = 0.05, na.rm=TRUE)), 
           .names = "{.col}"))

view(dfsub)


# reemplazo valores faltantes Y DATA SUBTRANSECTO
dfsub1 <- dfsub %>% 
  mutate(across(.cols = c(where(is.numeric), -c(SUBTRANSECTO)),
                .fns = ~replace(.x, is.na(.x)|is.nan(.x), mean(.x, na.rm=T))),
         SUBTRANSECTO = replace(SUBTRANSECTO, is.na(SUBTRANSECTO), 0))
view(dfsub1)
#comprobar
any(is.na(dfsub1))


# AllOk






# df´s subtransectos por CLASE --------------------------------------------

dfgrupos <- dfobservaciones %>% 
  group_split.(CLASE, .keep = TRUE, .named = TRUE)

dfgrupos$Reptilia

str(dfgrupos)
length(dfgrupos)
# dfgrupos$Reptilia_Geophis

# AllOk




# Data compilation --------------------------------------------------------------


view(dfobservaciones)
view(dfrecorridos)
view(dfenvi)
view(dfland1)
view(dfland2)
view(dfland3)


view(dfsub1)
View(dfgrupos)


# AllOk





View()





























































# No usar
# First wrangling ------------------------------------------------------------


# realizada en los primeros datos recibidos, los cuales tenian muchos errores
# de tipado, muchas inconsistencias entre los datos y en general estaban incorrectos.
# Estan en la carpeta /Data_raw
# Posterior a esto se recibieron nuevos archivos que eran diferentes, por lo 
# que esta parte del codigo se volvio inecesaria

# Se deja el codigo por si acaso


#####
# en el dataset hay varias variables con valores faltantes, veamos cuales y
# cuantas son

faltantes <- function(x) {
  any(is.na(x))
}

View(df1)

faltan <- apply(df1, MARGIN = 2, FUN = faltantes)
View(faltan)

#vemos ya en que variables nos faltan valores,

#cuantas son??
sum(faltan)
length(df1)-1

(length(df1)-1) - sum(faltan)



# Por lo general es necesario (pero no siempre) reemplazar los valores faltantes
# para asi poder ocupar todas las muestras recolectadas, existen diferentes
# metodos dependiendo de la variable y del tipo de dato, asi como del numero 
# faltante de estos, una buena aproximacion es identificar aquellas variables en
# las que falten menos del 30% - 40% de los datos 

faltantes1 <- function(x) {
  sum(is.na(x))
}
faltan1 <- apply(df1, MARGIN = 2, FUN = faltantes1)
View(faltan1)
sort(faltan1, decreasing = T)
# vemos las variables en las que faltan mas datos

# veamos los porcentajes de valores faltantes
faltantes2 <- function(x) {
  (sum(is.na(x)) / length(x)) * 100
}
faltan2 <- apply(df1, MARGIN = 2, FUN = faltantes2)
faltan2
View(faltan2)

#vemos cuales son las variables que mas falta de datos tienen
sort(faltan2, decreasing = T)

#veamos aquellas en donde el porcentaje sea mayor
porcentaje <- 40
faltan2[faltan2 > porcentaje]
# pues afortunadamente vemos que no son tantas las variables que tendriamos que
# descartar
# en las demas es necesario sutituir. Esta parte la hare talvez por algun 
# valor representativo del conjunto, o posiblemente mediante algun modelo 
# de regresion (necesito mas informacion de que representan esas variables)



# por lo mientras es posible obtener algunas estadisticas descriptivas 
# del conjunto de datos

summary(df1$MIN)
summary(df1$CLASE)

resum <- function(x) {
  summary(x)
}
dfprueba <- df1
dfprueba <- as.data.frame(dfprueba)

# obtengamos un resumen de las variables numericas excluyendo los NA

indices <- sapply(X = df1, FUN = is.numeric)
indices
resumenes1 <- apply(X = dfprueba[,indices], MARGIN = 2, FUN = resum)
resumenes1

#usando el paquete plyr (no es necesario al menos ahora)
#lprueba <- alply(.data = dfprueba, .margins = 2, .fun = resum)
#lprueba

#increible que esta sea la forma mas facil
resumenes <- sapply(X = df1, FUN = resum)
resumenes$Y_LAT
resumenes

# veo algo raro en la variable latitud y lngitud, ver que onda

#ahora valores unicos
unicos <- sapply(X = df1, FUN = unique)
unicos


resum1 <- function(x) {
  data.frame(var = var(x, na.rm = T),
             sd = sd(x, na.rm = T),
             rango = range(x, na.rm = T))
}
dispersion <- sapply(X = df1, FUN = resum1, simplify = F)
dispersion
str(dispersion)

### aqui puedo identificar las variables con mayor dispersion, seleccionarlas 
# automaticamentey en esas aplicar algun tipo de transformacion y eliminar
# outliers

any(is.na(df1))
any(is.na(df2))
any(is.na(df3))
any(is.na(df4))

#en los tres ultimos datasets no hay valores faltantes ni problemas













