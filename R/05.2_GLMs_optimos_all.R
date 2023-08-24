



# GLM models --------------------------------------------------------------




# data a usar
View(list_main_var)

# df global
dfallgroup <- list_main_var[[1]]
str(dfallgroup)




# All possible combinations -----------------------------------------------


# para un glm de la familia poisson existen dos funciones de enlace canonicas
# (log, identiy) se probara con ambas para determinar cual explica mejor 
# la interaccion en base al criterio de akaike (AIC)

# se contruiran todas las posibles combinaciones para ambas funciones


# Funcion de enlace log ---------------------

gm1 <- glm(formula = CONTEO ~.,
           family = poisson(link = "log"), 
           data = dfallgroup)
summary(gm1)
alias(gm1)
AIC(gm1)
BIC(gm1)
1-gm1$deviance/gm1$null.deviance   #este es el pseudo r2


# Funcion de enlace identity ---------------------

gm2 <- glm(formula = CONTEO ~., 
           family = poisson(link = "identity"), 
           data = dfallgroup)
summary(gm2)
alias(gm2)
AIC(gm2)
BIC(gm2)
1-gm2$deviance/gm2$null.deviance   #este es el pseudo r2





# All posible combinations ------------------------------------------------

backup_options <- options()
options(na.action = "na.fail")

#funcion de enlace log
fullmodels1 <- dredge(gm1, trace = 2)
View(fullmodels1)
sw(fullmodels1)
view(fullmodels1)
sumaw1 <- sw(fullmodels1)
sumaw1 <- tidy(sumaw1)
names(sumaw1) <- c("Variables", "Suma de los pesos de akaike")


#funcion de enlace identidad
fullmodels2 <- dredge(gm2, trace = 2)
View(fullmodels2)
sw(fullmodels2)
view(fullmodels2)
sumaw2 <- sw(fullmodels2)
sumaw2 <- tidy(sumaw2)
names(sumaw2) <- c("Variables", "Suma de los pesos de akaike")


options(backup_options)





# get models best ---------------------------------------------------------

# gmbest <- get.models(object = fullmodels1, subset = min(AICc))
# gmbest$`4929`

# models log
gmbest1 <- eval(getCall(fullmodels1, 1))
summary(gmbest1)

# graficos
visreg(gmbest1, gg = FALSE, scale = "response", ask=FALSE)

# models identiy
gmbest2 <- eval(getCall(fullmodels2, 1))
summary(gmbest2)

visreg(gmbest2, gg = FALSE, scale = "response", ask=FALSE)



# resumen muy corto best glm ----------------------------------------------


# funcion log
gmbestsum1 <- tidy((gmbest1))
gmbestcrit1 <- glance(gmbest1)
View(gmbestsum1)
View(gmbestcrit1)


# funcion identity
gmbestsum2 <- tidy((gmbest2))
gmbestcrit2 <- glance(gmbest2)
View(gmbestsum2)
View(gmbestcrit2)



# como se puede ver ambos modelos son bastante buenos, teniendo un AIC un
# poco mejor los de la funcion log como ya era de esperarse, aun asi se probo 
# para ambas para confirmar




# Conclusiones ------------------------------------------------------------

# tabla de importancia de las variables en el modelo de acuerdo a la suma de
# los pesos de akaike

# funcion log
view(sumaw1)

# funcion identity
view(sumaw2)

# AllOk



# En este caso del glm para toda la data en conjunto se observa que:
# 
# 1. Las variables del entorno son las mas relevantes teniendo presencia en 
# la gran mayoria de los modelos. (variables antropogenicas)
# 





