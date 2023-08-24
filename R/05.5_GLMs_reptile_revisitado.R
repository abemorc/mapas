



# GLM models --------------------------------------------------------------




# data a usar
View(list_ready_var)

# df reptile
dfreptile1 <- list_ready_var$Reptilia %>% 
  select(-c(CLASE, SPD))
str(dfreptile1)
view(dfreptile1)



# All possible combinations -----------------------------------------------


# para un glm de la familia poisson existen dos funciones de enlace canonicas
# (log, identiy) se probara con ambas para determinar cual explica mejor 
# la interaccion en base al criterio de akaike (AIC)

# se contruiran todas las posibles combinaciones para ambas funciones


# Funcion de enlace log ---------------------

gm3_1 <- glm(formula = CONTEO ~.,
           family = poisson(link = "log"), 
           data = dfreptile1)
summary(gm3_1)
alias(gm3_1)
AIC(gm3_1)
BIC(gm3_1)
1-gm3_1$deviance/gm3_1$null.deviance   #este es el pseudo r2


# Funcion de enlace identity ---------------------

gm4_1 <- glm(formula = CONTEO ~., 
           family = poisson(link = "identity"), 
           data = dfreptile1)
summary(gm4_1)
alias(gm4_1)
AIC(gm4_1)
BIC(gm4_1)
1-gm4_1$deviance/gm4_1$null.deviance   #este es el pseudo r2





# All posible combinations ------------------------------------------------

backup_options <- options()
options(na.action = "na.fail")

#funcion de enlace log
fullmodels3_1 <- dredge(gm3_1, trace = 2)
View(fullmodels3_1)
sw(fullmodels3_1)
view(fullmodels3_1)
sumaw3_1 <- sw(fullmodels3_1)
sumaw3_1 <- tidy(sumaw3_1)
names(sumaw3_1) <- c("Variables", "Suma de los pesos de akaike")


#funcion de enlace identidad
fullmodels4_1 <- dredge(gm4_1, trace = 2)
View(fullmodels4_1)
sw(fullmodels4_1)
view(fullmodels4_1)
sumaw4_1 <- sw(fullmodels4_1)
sumaw4_1 <- tidy(sumaw4_1)
names(sumaw4_1) <- c("Variables", "Suma de los pesos de akaike")


options(backup_options)





# get models best ---------------------------------------------------------

# gmbest <- get.models(object = fullmodels1, subset = min(AICc))
# gmbest$`4929`

# models log
gmbest3_1 <- eval(getCall(fullmodels3_1, 2))
summary(gmbest3_1)

# graficos
visreg(gmbest3_1, gg = FALSE, scale = "response", ask=FALSE)

# models identiy
gmbest4_1 <- eval(getCall(fullmodels4_1, 1))
summary(gmbest4_1)

visreg(gmbest4_1, gg = FALSE, scale = "response", ask=FALSE)



# resumen muy corto best glm ----------------------------------------------


# funcion log
gmbestsum3_1 <- tidy((gmbest3_1))
gmbestcrit3_1 <- glance(gmbest3_1)
View(gmbestsum3_1)
View(gmbestcrit3_1)


# funcion identity
gmbestsum4_1 <- tidy((gmbest4_1))
gmbestcrit4_1 <- glance(gmbest4_1)
View(gmbestsum4_1)
View(gmbestcrit4_1)



# como se puede ver ambos modelos son bastante buenos, teniendo un AIC un
# poco mejor los de la funcion log como ya era de esperarse, aun asi se probo 
# para ambas para confirmar




# Conclusiones ------------------------------------------------------------

# tabla de importancia de las variables en el modelo de acuerdo a la suma de
# los pesos de akaike

# funcion log
view(sumaw3_1)

# funcion identity
view(sumaw4_1)

# AllOk



# En este caso del glm para toda la data en conjunto se observa que:
# 
# 1. Las variables del entorno son las mas relevantes teniendo presencia en 
# la gran mayoria de los modelos. (variables antropogenicas)
# 2. Aqui tambien importa mucho la variable SPD y la variable HI, ambas 
# intimamente relacionadas a la temperatura, es decir, los reptiles si se ven
# influenciados por esta
# si hay variacion significativa de acuerdo a los horarios, presentando mayor 
# numero de muertos en el dia correspondiendo tambien con la actividad de estos







