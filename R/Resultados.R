


# Datos importados de excel y csv -----------------------------------------


# datos importados
#data main
view(df1)

# enviromental distances
view(df2)

#landsat primero recibido
view(df3)
#segundo enviado
view(df4)

#final recibido
view(df5)





# data wrangling ----------------------------------------------

# transformaciones data main
# unicamente observaciones
view(dfobservaciones)

# data frame recorridos
view(dfrecorridos)

#enviromental transformada
view(dfenvi)

#land sat
view(dfland1)

#landsat2
view(dfland2)

#landsat3
view(dfland3)





# Data ready to glm ----------------------------------------------------------

# todos los datos sin separar
View(list_main_var)

# datos separados por clase (puede ser por cualquier categoria)
View(list_ready_var)

# Los dos anteriores juntos
View(list_ready_var1)




# Analisis correlacion ----------------------------------------------------

# correlacion spearman
View(list_cor1)

# tabla de orden de imporatancia de acuerdo para el numero de atropellamientos
View(list_cor_conteo)


# matriz correlacion (r)
# matriz significancia (p)
View(list_cor_p)

# ttablas orde separadas
tabla_all <- list_cor_conteo$All
tabla_all

tabla_rep <- list_cor_conteo$Reptilia
tabla_rep

tabla_anf <- list_cor_conteo$Amphibia
tabla_anf

# p value separado

pvalAll <- list_cor_p$All$P
pvalAll <- pvalAll["CONTEO"]
pvalAll

pvalrep <- list_cor_p$Reptilia$P
pvalrep <- pvalrep["CONTEO"]
pvalrep

pvalanf <- list_cor_p$Amphibia$P
pvalanf <- pvalanf["CONTEO"]
pvalanf

t_all <- merge(x = tabla_all, y = pvalAll, by = 0)
t_rep <- merge(x = tabla_rep, y = pvalrep, by = 0)
t_anf <- merge(x = tabla_anf, y = pvalanf, by = 0)

l_dfs <- list(t_all, t_rep, t_anf)
write.csv(t_all, file = here("Output_data/Tablas_cor-pvalue/tabla_total.csv"))
write.csv(t_rep, file = here("Output_data/Tablas_cor-pvalue/tabla_reptiles.csv"))
write.csv(t_anf, file = here("Output_data/Tablas_cor-pvalue/tabla_anfibios.csv"))

# graficos correlacion
#  cada grafico corresponde en este orden:
# reptiles
# anfibios
# Todo en conjunto

# a mi parecer estas son las que condensan mas informacion
map(list_numeric1, .f = ~chart.Correlation(R = .x, method = "spearman"))


map(list_numeric1, .f = pairs.panels, method = "spearman", stars = TRUE,
    hist.col = 4, smooth = TRUE, scale = F, density = TRUE,
    pch = 21, lm = F, jiggle = T, ci = TRUE)

map(list_numeric1, .f = ~pairs.panels(x = .x[,], method = "spearman"))






# GLM explore -------------------------------------------------------------


# primer glm 
summary(gm1)
alias(gm1)

gm1res <- resid(gm1)
qqnorm(gm1res)
hist(gm1res)
plot(density(gm1res))

#pruebas mas formales
AIC(gm1)
BIC(gm1)
1-gm1$deviance/gm1$null.deviance   #este es el pseudo r2





# GLM optimos ALL DATA ------------------------------------------------------

# log
View(fullmodels1)

# inverse
View(fullmodels2)

# despues de contruir todos los modelos posibles sy compararlos bajo el
# criterio de akaike, este es el modelo que aparece al principio
#  para la funcion logaritmica

gmbest1 <- eval(getCall(fullmodels1, 1))
summary(gmbest1)
# graficos
visreg(gmbest1, gg = FALSE, scale = "response", ask=FALSE)
View(gmbestsum1)
View(gmbestcrit1)



# lo mismo de arriba pero para la otra funcion de enlace identity


# models identiy
gmbest2 <- eval(getCall(fullmodels2, 1))
summary(gmbest2)
# graficos
visreg(gmbest2, gg = FALSE, scale = "response", ask=FALSE)
View(gmbestsum2)
View(gmbestcrit2)


# tabla de la suma de los pesos de akaike
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



# GLM reptile -------------------------------------------------------------


View(fullmodels3)
View(fullmodels4)

# log
summary(gmbest3)
# graficos
visreg(gmbest3, gg = FALSE, scale = "response", ask=FALSE)


# models identiy
summary(gmbest4)
visreg(gmbest4, gg = FALSE, scale = "response", ask=FALSE)



View(gmbestsum3)
View(gmbestcrit3)


View(gmbestsum4)
View(gmbestcrit4)



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







# GLM anfibio -------------------------------------------------------------


View(fullmodels5)
View(fullmodels6)


# log
summary(gmbest5)
# graficos
visreg(gmbest5, gg = FALSE, scale = "response", ask=FALSE)

# models identiy
summary(gmbest6)
visreg(gmbest6, gg = FALSE, scale = "response", ask=FALSE)



View(gmbestsum5)
View(gmbestcrit5)


View(gmbestsum6)
View(gmbestcrit6)




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














# GLM reptile revisitado -----------------------------------------------------


View(fullmodels3_1)
View(fullmodels4_1)

# log
summary(gmbest3_1)
# graficos
visreg(gmbest3_1, gg = FALSE, scale = "response", ask=FALSE)


# models identiy
summary(gmbest4_1)
visreg(gmbest4_1, gg = FALSE, scale = "response", ask=FALSE)



View(gmbestsum3_1)
View(gmbestcrit3_1)


View(gmbestsum4_1)
View(gmbestcrit4_1)



# tabla de importancia de las variables en el modelo de acuerdo a la suma de
# los pesos de akaike

# funcion log
view(sumaw3_1)

# funcion identity
view(sumaw4_1)

# AllOk


# conclusiones: la variable spd en realidad es un "sintoma" de otras variables
# en particular de las antropogenicas, es decir de la presencia de humanos y de
# la temperatura








