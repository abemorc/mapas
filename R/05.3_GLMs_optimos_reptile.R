



# GLM models --------------------------------------------------------------




# data a usar
View(list_ready_var)

# df reptile
dfreptile <- list_ready_var$Reptilia %>% 
  select(-c(CLASE))
str(dfreptile)
view(dfreptile)



# All possible combinations -----------------------------------------------


# para un glm de la familia poisson existen dos funciones de enlace canonicas
# (log, identiy) se probara con ambas para determinar cual explica mejor 
# la interaccion en base al criterio de akaike (AIC)

# se contruiran todas las posibles combinaciones para ambas funciones


# Funcion de enlace log ---------------------

gm3 <- glm(formula = CONTEO ~.,
           family = poisson(link = "log"), 
           data = dfreptile)
summary(gm3)
alias(gm3)
AIC(gm3)
BIC(gm3)
1-gm3$deviance/gm3$null.deviance   #este es el pseudo r2


# Funcion de enlace identity ---------------------

gm4 <- glm(formula = CONTEO ~., 
           family = poisson(link = "identity"), 
           data = dfreptile)
summary(gm4)
alias(gm4)
AIC(gm4)
BIC(gm4)
1-gm4$deviance/gm4$null.deviance   #este es el pseudo r2





# All posible combinations ------------------------------------------------

backup_options <- options()
options(na.action = "na.fail")

#funcion de enlace log
fullmodels3 <- dredge(gm3, trace = 2)
View(fullmodels3)
sw(fullmodels3)
view(fullmodels3)
sumaw3 <- sw(fullmodels3)
sumaw3 <- tidy(sumaw3)
names(sumaw3) <- c("Variables", "Suma de los pesos de akaike")


#funcion de enlace identidad
fullmodels4 <- dredge(gm4, trace = 2)
View(fullmodels4)
sw(fullmodels4)
view(fullmodels4)
sumaw4 <- sw(fullmodels4)
sumaw4 <- tidy(sumaw4)
names(sumaw4) <- c("Variables", "Suma de los pesos de akaike")


options(backup_options)





# get models best ---------------------------------------------------------

# gmbest <- get.models(object = fullmodels1, subset = min(AICc))
# gmbest$`4929`

# models log
gmbest3 <- eval(getCall(fullmodels3, 1))
summary(gmbest3)

# graficos
visreg(gmbest3, gg = FALSE, scale = "response", ask=FALSE)

# models identiy
gmbest4 <- eval(getCall(fullmodels4, 1))
summary(gmbest4)

visreg(gmbest4, gg = FALSE, scale = "response", ask=FALSE)



# resumen muy corto best glm ----------------------------------------------


# funcion log
gmbestsum3 <- tidy((gmbest3))
gmbestcrit3 <- glance(gmbest3)
View(gmbestsum3)
View(gmbestcrit3)


# funcion identity
gmbestsum4 <- tidy((gmbest4))
gmbestcrit4 <- glance(gmbest4)
View(gmbestsum4)
View(gmbestcrit4)



# como se puede ver ambos modelos son bastante buenos, teniendo un AIC un
# poco mejor los de la funcion log como ya era de esperarse, aun asi se probo 
# para ambas para confirmar




# Conclusiones ------------------------------------------------------------

# tabla de importancia de las variables en el modelo de acuerdo a la suma de
# los pesos de akaike

# funcion log
view(sumaw3)

# funcion identity
view(sumaw4)

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







