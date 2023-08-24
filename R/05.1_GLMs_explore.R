



# GLMs per group  ---------------------------------------------------------



# data a usar
View(list_main_var)
View(list_ready_var)


dfallgroup <- list_main_var[[1]]
str(dfallgroup)


# primer glm exploratorio -------------------------------------------------

# definir un primer glm que pueda ser util
# se muestra unicamente el que si fue util

gm1 <- glm(formula = CONTEO ~., family = poisson(link = "log"), data = dfallgroup)

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


# perfecto, ya es un muy buen modelo inicial, ahora hay que intentar ajustarlo

# AllOk















###
# reduccion de variables correlacionadas ----------------------------------

xxcor <- list_cor$Main
xxcor1 <- as.matrix(xxcor)
str(xxcor1)

quitar <- findCorrelation(x = xxcor1, cutoff = 0.4, names = FALSE)
quitar <- quitar[-15]
quitar
xxx <- list_ready$Main
xxx <- xxx[,-quitar]

view(xxx)
str(xxcor1)

xxx <- xxx %>% 
  select(-c(VIVO, MUERTO))


gm1 <- glm(formula = CONTEO ~., family = poisson(link = "log"), data = xxx)
summary(gm1)















