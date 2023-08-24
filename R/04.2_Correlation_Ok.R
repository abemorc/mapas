



# Correlacion entre variables ---------------------------------------------
# -------------------------------------------------------------------------




# data a usar
View(list_main_var) #global
View(list_ready_var) #por grupos



# -------------------------------------------------------------------------

# Se analizara la correlacion para todos los grupos indicados

# 1. data total (sin agrupar)
# data agrupada de acuerdo a la seleccion del usuario (en este caso CLASE)

list_ready_var1 <- list_ready_var
list_ready_var1$All <- list_main_var[[1]]
View(list_ready_var1)


# 1. ----------------------------------------------------------------

View(list_ready_var1)

#obtener unicamente las columnas numericas
list_numeric1 <- map(list_ready_var1, .f = ~.x %>% select(where(is.numeric)))



# matrices de correlacion --------------------------------------------------
# Se usara el metodo de spearman ya que es el mas adecuado para este conjunto
# de datos

list_cor1 <- map(list_numeric1, 
                .f = ~as.data.frame(cor(.x,use ="complete.obs",
                                        method = "spearman")))

View(list_cor1)


# ya sale algo mucho mas interesante, ya podemos ver que variables estan 
# relacionadas unas con otras



# aqui esta el orden de importancia de las variables de acuerdo al la correlacion
# por el metodo de spearman
list_cor_conteo <- map(list_cor1, 
                 .f = ~.x[order(abs(.x[,"CONTEO"]), decreasing = TRUE) ,"CONTEO", drop = FALSE])
View(list_cor_conteo)

# AllOk



# otras estadisticas de la correlacion
# el p-value de cada relacion
list_cor_p <- map(list_numeric1, .f = ~rcorr(x = as.matrix(.x), type = "spearman"))
View(list_cor_p)

# Todas las matrices en una lista
list_cor_p <- map(list_cor_p, .f = ~map(.x, .f = as.data.frame))
View(list_cor_p)




# graficos correlacion ----------------------------------------------------

map(list_numeric1, .f = ~chart.Correlation(R = .x, method = "spearman"))


map(list_numeric1, .f = pairs.panels, method = "spearman", stars = TRUE,
    hist.col = 4, smooth = TRUE, scale = F, density = TRUE,
    pch = 21, lm = F, jiggle = T, ci = TRUE)

map(list_numeric1, .f = ~pairs.panels(x = .x[,], method = "spearman"))


# AllOk


# Como se puede apreciar en unico llamado se realiza el analisis deseado 
# sobre todos los subgrupos deseados a la vez

# Algo asi se intenta para los GLM por grupos.





