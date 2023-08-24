



# GLM models --------------------------------------------------------------




# data a usar
View(list_ready_var)

# df reptile
dfanf <- list_ready_var$Amphibia %>% 
  select(-c(CLASE))
str(dfanf)
view(dfanf)



# All possible combinations -----------------------------------------------


# para un glm de la familia poisson existen dos funciones de enlace canonicas
# (log, identiy) se probara con ambas para determinar cual explica mejor 
# la interaccion en base al criterio de akaike (AIC)

# se contruiran todas las posibles combinaciones para ambas funciones


# Funcion de enlace log ---------------------

gm5 <- glm(formula = CONTEO ~.,
           family = poisson(link = "log"), 
           data = dfanf)
summary(gm5)
alias(gm5)
AIC(gm5)
BIC(gm5)
1-gm5$deviance/gm5$null.deviance   #este es el pseudo r2


# Funcion de enlace identity ---------------------

gm6 <- glm(formula = CONTEO ~., 
           family = poisson(link = "identity"), 
           data = dfanf)
summary(gm6)
alias(gm6)
AIC(gm6)
BIC(gm6)
1-gm6$deviance/gm6$null.deviance   #este es el pseudo r2





# All posible combinations ------------------------------------------------

backup_options <- options()
options(na.action = "na.fail")

#funcion de enlace log
fullmodels5 <- dredge(gm5, trace = 2)
View(fullmodels5)
sw(fullmodels5)
view(fullmodels5)
sumaw5 <- sw(fullmodels5)
sumaw5 <- tidy(sumaw5)
names(sumaw5) <- c("Variables", "Suma de los pesos de akaike")


#funcion de enlace identidad
fullmodels6 <- dredge(gm6, trace = 2)
View(fullmodels6)
sw(fullmodels6)
view(fullmodels6)
sumaw6 <- sw(fullmodels6)
sumaw6 <- tidy(sumaw6)
names(sumaw6) <- c("Variables", "Suma de los pesos de akaike")


options(backup_options)





# get models best ---------------------------------------------------------

# gmbest <- get.models(object = fullmodels1, subset = min(AICc))
# gmbest$`4929`

# models log
gmbest5 <- eval(getCall(fullmodels5, 1))
summary(gmbest5)

# graficos
visreg(gmbest5, gg = FALSE, scale = "response", ask=FALSE)

# models identiy
gmbest6 <- eval(getCall(fullmodels6, 1))
summary(gmbest6)

visreg(gmbest6, gg = FALSE, scale = "response", ask=FALSE)



# resumen muy corto best glm ----------------------------------------------


# funcion log
gmbestsum5 <- tidy((gmbest5))
gmbestcrit5 <- glance(gmbest5)
View(gmbestsum5)
View(gmbestcrit5)


# funcion identity
gmbestsum6 <- tidy((gmbest6))
gmbestcrit6 <- glance(gmbest6)
View(gmbestsum6)
View(gmbestcrit6)



# como se puede ver ambos modelos son bastante buenos, teniendo un AIC un
# poco mejor los de la funcion log como ya era de esperarse, aun asi se probo 
# para ambas para confirmar




# Conclusiones ------------------------------------------------------------

# tabla de importancia de las variables en el modelo de acuerdo a la suma de
# los pesos de akaike

# funcion log
view(sumaw5)

# funcion identity
view(sumaw5)

# AllOk



# En este caso del glm para toda la data en conjunto se observa que:
# 
# 1. Las variables del entorno son las mas relevantes teniendo presencia en 
# la gran mayoria de los modelos. (variables antropogenicas)
# 2. Para los anfibios importa mucho mas a comparacion la distancia a zonas
# urbanizadas, es decir la precencia de humanos
# 3. En el caso de anfibios si se ven influenciados por la temporada, es decir 
# las variables del clima si son mas relevantes para los anfibios







