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



dfrecorridos %>% group_by(TOTAL_CARROS) %>% 
  summarise()


p1 <- dfobservaciones %>% arrange(DIAWEEK)
view(p1)




names(mtcars)
variable <- names(mtcars)[1]
variable

mtcars %>% count(mpg) %>% 
  print()


mtcars %>% count(.data[[variable]]) %>% 
  print()

mtcars %>% count(.data$disp) %>% 
  print()




######### no funciono correctamente

funcion2 <- function(variables) {
  dfgrupos3 <- dfobservaciones %>% 
    group_split.(.data[[variables]], .keep = TRUE, .named = TRUE)
  return(dfgrupos3)
}

variable2 <- c("CLASE")

funcion2(variable2)




funcion3 <- function(variables) {
  dfgrupos4 <- dfobservaciones %>% 
    group_split.(unlist(.data[variables]), .keep = TRUE, .named = TRUE)
  return(dfgrupos4)
}

variable2 <- c("CLASE")

funcion3(variable1)



#########################







# perfecto
dfsub2 <- dfobservaciones %>% 
  group_by(SUBTRANSECTO) %>% 
  summarise(across(DEG:TEMP_SUELO, .fns = ~mean(.x, trim = 0.05, na.rm = TRUE)))

view(dfsub2)

identical(dfsub1, dfsub2)

























# creacion nuevas variables -----------------------------------------------

# Data main
## empezaremos por el tiempo
#
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
         TURNO = ifelse(hour(HORA_1ST) %in% mañana, "mañana", "noche"),
         OBSERVACION_TF = ifelse(OBSERVACION_TF == "TRUE", TRUE, FALSE),
  ) %>% 
  select(FECHAF, AÑO, MES, SEMANA, DIA, TURNO, TRANSECTO, SUBTRANSECTO, everything())

dfobservaciones
view(dfobservaciones)
view(df1)



# Separar informacion  -----------------------------------------------------


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


# Data set observaciones
index <- dfobservaciones$OBSERVACION_TF == TRUE
index
dfobservaciones <- dfobservaciones[index,]
view(dfobservaciones)

# AllOk


# Subtransectos -----------------------------------------------------------

dfsub <- dfobservaciones %>% 
  group_by(SUBTRANSECTO) %>% 
  summarise(across(DEG:TEMP_SUELO, .fns = ~ ./ max(., na.rm = TRUE)))

view(dfsub)

dfsub1 <- dfobservaciones %>% 
  group_by(SUBTRANSECTO) %>% 
  summarise(across(DEG:TEMP_SUELO, .fns = mean, na.rm=T))

view(dfsub1)




# perfecto
dfsub2 <- dfobservaciones %>% 
  group_by(SUBTRANSECTO) %>% 
  summarise(across(DEG:TEMP_SUELO, .fns = ~mean(.x, trim = 0.05, na.rm = TRUE)))

view(dfsub2)

identical(dfsub1, dfsub2)


















gm1 <- glm(formula = CONTEO ~ SUBTRANSECTO + ALTITUD + HI + SPD + TEMP_SUELO + 
             TOTAL_CARROS + CAÑADA + ESCURRIMIENTOS + 
             DIST_PENDIENTE_MEAN + DIST_RIOS_MEAN + DIST_AGRICOLA_MEAN + 
             DIST_URBANIZACION_MEAN + NDWI_AGUA_MEAN + NDMI_HUMEDAD_MEAN + 
             NDVI_VEGETACION_MEAN + DIAWEEK, 
           family = poisson(link = "log"), data = xxall)



gm1 <- glm(formula = CONTEO ~ ALTITUD + HI + SPD + TEMP_SUELO + 
             TOTAL_CARROS + DIST_ELECTRICOS_MEAN + 
             DIST_PENDIENTE_MEAN + DIST_RIOS_MEAN + DIST_AGRICOLA_MEAN + 
             DIST_URBANIZACION_MEAN + NDWI_AGUA_MEAN + NDMI_HUMEDAD_MEAN + 
             NDVI_VEGETACION_MEAN + LST_TEMPERATURA_MEAN + DIAWEEK,
           family = poisson(link = "log"), data = xxall)

gm2 <- glm(formula = CONTEO ~ SUBTRANSECTO + TEMPORADA + ALTITUD + 
             TOTAL_CARROS + TURNO + CLASE +
             DIST_RIOS_MEAN + DIST_URBANIZACION_MEAN + DIST_AGRICOLA_MEAN +
             DIST_PENDIENTE_MEAN + NDVI_VEGETACION_MEAN + HI + SPD +
             TEMP_SUELO + DIST_ELECTRICOS_MEAN + NDMI_HUMEDAD_MEAN, 
           family = poisson(link = "log"), data = xxall)

str(xxall)

summary(gm2)







# identificar variables de un tipo de dato

xx <- list_ready$Reptilia
cor(xx[,sapply(xx, is.numeric)])
# columnas numericas
nums <- unlist(lapply(xx, is.numeric))  
nums

xx[, nums]














# GLMs fail ---------------------------------------------------------------







# GLM models --------------------------------------------------------------



# data a usar
View(list_main_var)
View(list_ready_var)


dfallgroup <- list_main_var[[1]]
str(dfallgroup)






# probaremos con las dos funciones de identidad canonicas -----------------

# logaritmica

gm1 <- glm(formula = CONTEO ~., family = poisson(link = "log"), data = dfallgroup)
summary(gm1)
alias(gm1)
AIC(gm1)
BIC(gm1)
1-gm1$deviance/gm1$null.deviance   #este es el pseudo r2



# identidad

gm2 <- glm(formula = CONTEO ~., family = poisson(link = "identity"), data = dfallgroup)
summary(gm2)
alias(gm2)
AIC(gm2)
BIC(gm2)
1-gm2$deviance/gm2$null.deviance   #este es el pseudo r2





# All posible combinations ------------------------------------------------

backup_options <- options()
options(na.action = "na.fail")

#funcion de enlace logaritmica
fullmodels1 <- dredge(gm1, trace = 2)
View(fullmodels1)
sw(fullmodels1)
view(fullmodels1)


write.csv(x = fullmodels1, file = here("output/GLM resum/fullmodels_poisson_log.csv"))




#funcion de enlace identidad
fullmodels2 <- dredge(gm2, trace = 2)
View(fullmodels2)
sw(fullmodels2)


write.csv(x = fullmodels2, file = here("output/GLM resum/fullmodells_poisson_identity.csv"))

options(backup_options)





# get models best ---------------------------------------------------------

gmbest <- get.models(object = fullmodels1, subset = min(AICc))
gmbest$`4929`



gmbest <- eval(getCall(fullmodels1, 1))
summary(gmbest)

visreg(gmbest, gg = FALSE, scale = "response", ask=FALSE)
visreg(gmbest, scale = "response")


gmbestiden <- eval(getCall(fullmodels2, 1))
summary(gmbestiden)
visreg(gmbestiden, gg = FALSE, scale = "response", ask=FALSE)
visreg(gmbestiden, gg = TRUE, scale = "response")


a <- tidy((gmmolde1))
b <- glance(gmmolde1)

write.csv(a, here("Tereso/GLM 1/output data/glm1.csv"))
write.csv(b, here("Tereso/GLM 1/output data/glm1_performance.csv"))

# como se puede ver ambos modelos son muy muy buenos, siendo ligeramente
# mejores los de la funcion logaritmica





sumaw1 <- sw(fullmodels1)
sumaw1 <- tidy(sumaw1)

names(sumaw1) <- c("Variables", "Suma de los pesos de akaike")
view(sumaw1)


# AllOk


















# funcion glm -------------------------------------------------------------

func_glm <- function(df) {
  
  gm1 <- glm(formula = CONTEO ~., family = poisson(link = "log"), data = df)
  resumen1 <- summary(gm1)
  alias(gm1)
  AIC(gm1)
  BIC(gm1)
  1-gm1$deviance/gm1$null.deviance   #este es el pseudo r2
  
  
  # identidad
  
  gm2 <- glm(formula = CONTEO ~., family = poisson(link = "identity"), data = df)
  resumen2 <- summary(gm2)
  alias(gm2)
  AIC(gm2)
  BIC(gm2)
  1-gm2$deviance/gm2$null.deviance   #este es el pseudo r2
  
  
  
  # All posible combinations ------------------------------------------------
  
  backup_options <- options()
  options(na.action = "na.fail")
  
  #funcion de enlace logaritmica
  fullmodels1 <- dredge(gm1, trace = 2)
  View(fullmodels1)
  sw(fullmodels1)
  
  
  options(backup_options)
  
  
  
  ##
  
  gmbest <- eval(getCall(fullmodels1, 1))
  summary(gmbest)
  
  # visreg(gmbest, gg = FALSE, scale = "response", ask=FALSE)
  
  
  
  # suma de peso akaikes
  
  sumaw1 <- sw(fullmodels1)
  sumaw1 <- tidy(sumaw1)
  
  names(sumaw1) <- c("Variables", "Suma de los pesos de akaike")
  view(sumaw1)
  # AllOk
  lista1 <- list(resumen1=resumen1, resumen2=resumen2, gmbest=gmbest, sumaw=sumaw1)
  return(lista1)
  
}







dfreptile <- list_ready_var$Reptilia
list_reptile <- func_glm(dfreptile)

str(dfreptile)


gmrep <-glm()
















par(ask=F)
devAskNewPage(ask = FALSE)


#the solution
plotit <- function() {
  visreg(gmbest, scale = "response", ask = FALSE)
}

visreg(gmbest, scale = "response", ask= FALSE)
grafica1 <- plotit()


plotit2 <- function(modelo) {
  visreg(modelo, scale = "response", ask = FALSE)
}

grafica1 <- function() {
  plotit2(gmbest)
}


a <- grafica1()

a

aa <- list(modelo_grafica = grafica1())

aa$modelo_grafica

# retirar variables -------------------------------------------------------

# metodo analitico --------------------------------------------------------

# retirar variables mediante el grado de correlacion que existe entre ellas:

#asignar variables para prueba
mcormain <- list_cor$Main
mcormain <- as.matrix(mcormain)
str(mcormain)


#encontrar variables
index_quitar <- findCorrelation(x = mcormain, cutoff = 0.2, names = FALSE)
index_quitar
index <- which(index_quitar==2)
index
index_quitar <- index_quitar[-index]

dfsub2 <- list_ready$Main
dfsub2 <- dfsub2[,-index_quitar]
str(dfsub2)


# models dredge -----------------------------------------------------------

dfsub2 <- dfsub2[,c(2,5:10)]
str(dfsub2)

gmmolde1 <- glm(formula = CONTEO ~., family = poisson(link = "log"), data = dfsub2)
summary(gmmolde1)
1-gmmolde1$deviance/gmmolde1$null.deviance

visreg(gmmolde1, gg = TRUE, scale = "response")

## modelo 1

library(broom)

a <- tidy((gmmolde1))
b <- glance(gmmolde1)

write.csv(a, here("Tereso/GLM 1/output data/glm1.csv"))
write.csv(b, here("Tereso/GLM 1/output data/glm1_performance.csv"))


backup_options <- options()
options(na.action = "na.fail")

#funcion de enlace inversa
fullmodels1 <- dredge(gmmolde1, trace = 2) 
View(fullmodels1)
sw(fullmodels1)

#funcion de enlace logaritmica
fullmodels2 <- dredge(gma2)
View(fullmodels2)
sw(fullmodels2)


options(backup_options)

# es indispensable reducir el modelo, con 31 variables la cantidad de tiempo 
# en resolverse es abismal

# FAIL




# modelo seleccionado por criterio de akaike
gmmolde2 <- glm(formula = CONTEO ~ ALTITUD_MEAN + DIST_ELECTRICOS_MEAN +
                  NDMI_HUMEDAD_MEAN+ NDWI_AGUA_MEAN, family = poisson(link = "log"), data = dfsub2)
summary(gmmolde2)
1-gmmolde1$deviance/gmmolde1$null.deviance

visreg(gmmolde2, gg = TRUE, 
       scale="response")


ccc <- tidy((gmmolde2))
ddd <- glance(gmmolde2)

write.csv(ccc, here("Tereso/GLM 2/output data/glm2.csv"))
write.csv(ddd, here("Tereso/GLM 2/output data/glm2_performance.csv"))







xx <- list_ready$Main
write.csv(x = xx, file = here("Data/Ready_data/obs.csv"))














# glm descartado funcion --------------------------------------------------



# funcion glm -------------------------------------------------------------

func_glm <- function(df) {
  
  gm1 <- glm(formula = CONTEO ~., family = poisson(link = "log"), data = df)
  resumen1 <- summary(gm1)
  alias(gm1)
  AIC(gm1)
  BIC(gm1)
  1-gm1$deviance/gm1$null.deviance   #este es el pseudo r2
  
  
  # identidad
  
  gm2 <- glm(formula = CONTEO ~., family = poisson(link = "identity"), data = df)
  resumen2 <- summary(gm2)
  alias(gm2)
  AIC(gm2)
  BIC(gm2)
  1-gm2$deviance/gm2$null.deviance   #este es el pseudo r2
  
  
  
  # All posible combinations ------------------------------------------------
  
  backup_options <- options()
  options(na.action = "na.fail")
  
  #funcion de enlace logaritmica
  fullmodels1 <- dredge(gm1, trace = 2)
  View(fullmodels1)
  sw(fullmodels1)
  
  
  options(backup_options)
  
  
  
  ##
  
  gmbest <- eval(getCall(fullmodels1, 1))
  summary(gmbest)
  
  # visreg(gmbest, gg = FALSE, scale = "response", ask=FALSE)
  
  
  
  # suma de peso akaikes
  
  sumaw1 <- sw(fullmodels1)
  sumaw1 <- tidy(sumaw1)
  
  names(sumaw1) <- c("Variables", "Suma de los pesos de akaike")
  view(sumaw1)
  # AllOk
  lista1 <- list(resumen1=resumen1, resumen2=resumen2, gmbest=gmbest, sumaw=sumaw1)
  return(lista1)
  
}








# graficas ----------------------------------------------------------------





dfreptile <- list_ready_var$Reptilia
list_reptile <- func_glm(dfreptile)

str(dfreptile)


gmrep <-glm()
















par(ask=F)
devAskNewPage(ask = FALSE)


#the solution
plotit <- function() {
  visreg(gmbest, scale = "response", ask = FALSE)
}

visreg(gmbest, scale = "response", ask= FALSE)
grafica1 <- plotit()


plotit2 <- function(modelo) {
  visreg(modelo, scale = "response", ask = FALSE)
}

grafica1 <- function() {
  plotit2(gmbest)
}


a <- grafica1()

a

aa <- list(modelo_grafica = grafica1())

aa$modelo_grafica



# dredge ------------------------------------------------------------------

# retirar variables -------------------------------------------------------

# metodo analitico --------------------------------------------------------

# retirar variables mediante el grado de correlacion que existe entre ellas:

#asignar variables para prueba
mcormain <- list_cor$Main
mcormain <- as.matrix(mcormain)
str(mcormain)


#encontrar variables
index_quitar <- findCorrelation(x = mcormain, cutoff = 0.2, names = FALSE)
index_quitar
index <- which(index_quitar==2)
index
index_quitar <- index_quitar[-index]

dfsub2 <- list_ready$Main
dfsub2 <- dfsub2[,-index_quitar]
str(dfsub2)


# models dredge -----------------------------------------------------------

dfsub2 <- dfsub2[,c(2,5:10)]
str(dfsub2)

gmmolde1 <- glm(formula = CONTEO ~., family = poisson(link = "log"), data = dfsub2)
summary(gmmolde1)
1-gmmolde1$deviance/gmmolde1$null.deviance

visreg(gmmolde1, gg = TRUE, scale = "response")

## modelo 1

library(broom)

a <- tidy((gmmolde1))
b <- glance(gmmolde1)

write.csv(a, here("Tereso/GLM 1/output data/glm1.csv"))
write.csv(b, here("Tereso/GLM 1/output data/glm1_performance.csv"))


backup_options <- options()
options(na.action = "na.fail")

#funcion de enlace inversa
fullmodels1 <- dredge(gmmolde1, trace = 2) 
View(fullmodels1)
sw(fullmodels1)

#funcion de enlace logaritmica
fullmodels2 <- dredge(gma2)
View(fullmodels2)
sw(fullmodels2)


options(backup_options)

# es indispensable reducir el modelo, con 31 variables la cantidad de tiempo 
# en resolverse es abismal

# FAIL




# modelo seleccionado por criterio de akaike
gmmolde2 <- glm(formula = CONTEO ~ ALTITUD_MEAN + DIST_ELECTRICOS_MEAN +
                  NDMI_HUMEDAD_MEAN+ NDWI_AGUA_MEAN, family = poisson(link = "log"), data = dfsub2)
summary(gmmolde2)
1-gmmolde1$deviance/gmmolde1$null.deviance

visreg(gmmolde2, gg = TRUE, 
       scale="response")


ccc <- tidy((gmmolde2))
ddd <- glance(gmmolde2)

write.csv(ccc, here("Tereso/GLM 2/output data/glm2.csv"))
write.csv(ddd, here("Tereso/GLM 2/output data/glm2_performance.csv"))







xx <- list_ready$Main
write.csv(x = xx, file = here("Data/Ready_data/obs.csv"))









# Correlacion deprecated --------------------------------------------------





# Correlacion entre variables ---------------------------------------------



# data a usar
View(list_main_var) #global
View(list_ready_var) #por grupos


# Se analizara la correlacion en dos casos:

# 1. data total (sin agrupar)
# data agrupada de acuerdo a la seleccion del usuario (en este caso CLASE)



# 1. Global ----------------------------------------------------------------

View(list_main_var)

#obtener unicamente las columnas numericas
list_numeric1 <- map(list_main_var, .f = ~.x %>% select(where(is.numeric)))



# matrices de correlacion --------------------------------------------------
# Se usara el metodo de spearman ya que es el mas adecuado para este conjunto
# de datos

list_cor1 <- map(list_numeric1, 
                 .f = ~as.data.frame(cor(.x,use ="complete.obs",
                                         method = "spearman")))

View(list_cor1)
# write.csv(x = list_cor$Reptilia, file = here("output/Correlacion/Reptile/matriz.csv"))
# write.csv(x = list_cor$Amphibia, file = here("output/Correlacion/Anfibios/Data/matrizcor.csv"))


# nombres <- c(here("output/Correlacion/Reptile/data/matriz2"), 
#              here("output/Correlacion/Anfibios/Data/matriz3"))
# mapply(write.csv, list_cor, file=names)


# ya sale algo mucho mas interesante, ya podemos ver que variables estan 
# relacionadas unas con otras



# aqui esta el orden de importancia de las variables de acuerdo al la correlacion
# por el metodo de spearman
list_cor1 <- map(list_cor, 
                 .f = ~.x[order(abs(.x[,"CONTEO"]), decreasing = TRUE) ,"CONTEO", drop = FALSE])
View(list_cor1)
# allok

# write.csv(x = list_cor1$Reptilia, file = here("output/Correlacion/Reptile/Data/orden.csv"))
# write.csv(x = list_cor1$Amphibia, file = here("output/Correlacion/Anfibios/Data/orden.csv"))





# otras estadisticas de la correlacion
# el p-value de cada relacion
list_cor2 <- map(list_numeric, .f = ~rcorr(x = as.matrix(.x), type = "spearman"))
View(list_cor2)

list_cor2 <- map(list_cor2, .f = ~map(.x, .f = as.data.frame))
View(list_cor2)


# write.csv(x = list_cor2$Reptilia, file = here("output/Correlacion/Reptile/Data/pvalue.csv"))
# write.csv(x = list_cor2$Amphibia, file = here("output/Correlacion/Anfibios/Data/pvalue.csv"))



# graficos correlacion ----------------------------------------------------

map(list_numeric, .f = ~chart.Correlation(R = .x, method = "spearman"))


# quitando algunas variables menos interesantes
# map(list_numeric, .f = ~chart.Correlation(R = .x[,-c(1,6,10,11)], method = "spearman"))

# map(list_numeric, .f = pairs.panels, method = "spearman", stars = TRUE,  
#     hist.col = 4, smooth = TRUE, scale = F, density = TRUE,
#     pch = 21, lm = F, jiggle = T, ci = TRUE)


# quitando algunas variables menos interesantes
map(list_numeric, .f = ~pairs.panels(x = .x[,], method = "spearman"))


# AllOk









View()
















# por grupos de acuerdo a los parametros iniciales -------------------------



# View(list_grupos)
# View(list_ready)
View(dfallgroup)
View(list_main_var)


#obtener unicamente las columnas numericas
list_numeric <- map(list_ready_var, .f = ~.x %>% select(where(is.numeric)))




# matrices de correlacion spearman ----------------------------------------

list_cor <- map(list_numeric, 
                .f = ~as.data.frame(cor(.x,use ="complete.obs",
                                        method = "spearman")))

View(list_cor)
# write.csv(x = list_cor$Reptilia, file = here("output/Correlacion/Reptile/matriz.csv"))
# write.csv(x = list_cor$Amphibia, file = here("output/Correlacion/Anfibios/Data/matrizcor.csv"))


# nombres <- c(here("output/Correlacion/Reptile/data/matriz2"), 
#              here("output/Correlacion/Anfibios/Data/matriz3"))
# mapply(write.csv, list_cor, file=names)


# ya sale algo mucho mas interesante, ya podemos ver que variables estan 
# relacionadas unas con otras



# aqui esta el orden de importancia de las variables de acuerdo al la correlacion
# por el metodo de spearman
list_cor1 <- map(list_cor, 
                 .f = ~.x[order(abs(.x[,"CONTEO"]), decreasing = TRUE) ,"CONTEO", drop = FALSE])
View(list_cor1)
# allok

# write.csv(x = list_cor1$Reptilia, file = here("output/Correlacion/Reptile/Data/orden.csv"))
# write.csv(x = list_cor1$Amphibia, file = here("output/Correlacion/Anfibios/Data/orden.csv"))





# otras estadisticas de la correlacion
# el p-value de cada relacion
list_cor2 <- map(list_numeric, .f = ~rcorr(x = as.matrix(.x), type = "spearman"))
View(list_cor2)

list_cor2 <- map(list_cor2, .f = ~map(.x, .f = as.data.frame))
View(list_cor2)


# write.csv(x = list_cor2$Reptilia, file = here("output/Correlacion/Reptile/Data/pvalue.csv"))
# write.csv(x = list_cor2$Amphibia, file = here("output/Correlacion/Anfibios/Data/pvalue.csv"))



# graficos correlacion ----------------------------------------------------

map(list_numeric, .f = ~chart.Correlation(R = .x, method = "spearman"))


# quitando algunas variables menos interesantes
# map(list_numeric, .f = ~chart.Correlation(R = .x[,-c(1,6,10,11)], method = "spearman"))

# map(list_numeric, .f = pairs.panels, method = "spearman", stars = TRUE,  
#     hist.col = 4, smooth = TRUE, scale = F, density = TRUE,
#     pch = 21, lm = F, jiggle = T, ci = TRUE)


# quitando algunas variables menos interesantes
map(list_numeric, .f = ~pairs.panels(x = .x[,], method = "spearman"))


# AllOk









View()






