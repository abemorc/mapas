




# Split data by desired groups --------------------------------------------

# La idea de este script es crear listas de dataframes divididos por 
# cualesquiera categorias seleccionadas, transformar esos datos si es 
# necesario y posteriormente aplicar cualquier analiis deseado sobre todos 
# los dataframes al mismo tiempo


# Para este primer caso se solicitaron GLM sobre todo el conjunto y
# aparte divididos por clase



# all data ----------------------------------------------------------------


# sin grupos
list_main <- subgrupos()
View(list_main)

# transformar data 
list_main <- func_df_ready(list_main)
View(list_main)

# solicitud tereso
# seleccion de variables que se indicaron
list_main_var <- func_important_vars(list_main)
View(list_main_var)

dfallgroup <- list_main_var[[1]]
view(dfallgroup)


# AllOk






# By groups ---------------------------------------------------------------

# grupos deseados
desired_groups <- c("CLASE")

#desired_groups <- c("CLASE", "GENERO", "CONDICION")
#desired_groups <- c("TEMPORADA", "GENERO")


# por grupos
list_grupos <- subgrupos(desired_groups)
View(list_grupos)

# transformar
list_ready <- func_df_ready(list_grupos)
View(list_ready)

# solicitud tereso
# seleccion de variables que se indicaron
list_ready_var <- func_important_vars(list_ready)
View(list_ready_var)



# AllOk



View()

































# No usar
# Se solicito tambien solo variables antropogenicas y ambientales separadas
# para los glm, pero posteriormente fue cancelado, por lo que ya no se uso
# Ambiental variables -----------------------------------------------------


list_ambien <- func_important_var_ambien(list_ready)
View(list_ambien)





# antropogenica variables -------------------------------------------------


list_antro <- func_important_var_antro(list_ready)
View(list_antro)




