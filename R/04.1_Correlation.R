

# 


# Correlacion entre variables ---------------------------------------------



# por grupos de acuerdoa los parametros iniciales -------------------------



View(list_grupos)
View(list_ready)
View(list_ready_var)

#obtener unicamente las columnas numericas
list_numeric <- map(list_ready_var, .f = ~.x %>% select(where(is.numeric)))




# matrices de correlacion spearman ----------------------------------------

list_cor <- map(list_numeric, 
                .f = ~as.data.frame(cor(.x,use ="complete.obs",
                                        method = "spearman")))

View(list_cor)

# ya sale algo mucho mas interesante, ya podemos ver que variables estan 
# relacionadas unas con otras



# aqui esta el orden de importancia de las variables de acuerdo al la correlacion
# por el metodo de spearman
list_cor1 <- map(list_cor, 
                 .f = ~.x[order(abs(.x[,"CONTEO"]), decreasing = TRUE) ,"CONTEO", drop = FALSE])
View(list_cor1)
# allok



# otras estadisticas de la correlacion
# el p-value de cada relacion
list_cor2 <- map(list_numeric, .f = ~rcorr(x = as.matrix(.x), type = "spearman"))
View(list_cor2)

list_cor2 <- map(list_cor2, .f = ~map(.x, .f = as.data.frame))
View(list_cor2)






# graficos correlacion ----------------------------------------------------

map(list_numeric, .f = ~chart.Correlation(R = .x, method = "spearman"))


# quitando algunas variables menos interesantes
map(list_numeric, .f = ~chart.Correlation(R = .x[,-c(1,6,10,11)], method = "spearman"))

# map(list_numeric, .f = pairs.panels, method = "spearman", stars = TRUE,  
#     hist.col = 4, smooth = TRUE, scale = F, density = TRUE,
#     pch = 21, lm = F, jiggle = T, ci = TRUE)


# quitando algunas variables menos interesantes
map(list_numeric, .f = ~pairs.panels(x = .x[,-c(1,6,10,11)], method = "spearman"))


# AllOk


map(list_cor ,.f = ~write())






View()



















write.csv(x = list_cor$Amphibia, file = here("Data/Output_data/correlaciones_amphibia.csv"))
write.csv(x = list_cor$Reptilia, file = here("Data/Output_data/correlaciones_reptilia.csv"))
write.csv(x = list_cor$Main, file = here("Data/Output_data/correlaciones_main.csv"))



write.csv(x = list_cor1$Main, file = here("Data/Output_data/correlaciones_main_order.csv"))
write.csv(x = list_cor1$Reptilia, file = here("Data/Output_data/correlaciones_rep_order.csv"))
write.csv(x = list_cor1$Amphibia, file = here("Data/Output_data/correlaciones_amp_order.csv"))






