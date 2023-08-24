

# function all sub groups -------------------------------------------------


subgrupos <- function(variables=NULL) {
  if(is.null(variables)){
    dfgrupos2 <- dfobservaciones %>% 
      group_split.(.keep = TRUE, .named = TRUE)
  } else {
    dfgrupos2 <- dfobservaciones %>% 
      group_split.(all_of(variables), .keep = TRUE, .named = TRUE)
    
  }
  
  return(dfgrupos2)
  
  # AllOk
}

# EJEMPLO:
# desired_groups <- c("CLASE", "GENERO")
# desired_groups <- c("CLASE")
# 
# dfgrupos2 <- subgrupos(desired_groups)
# View(dfgrupos2)
# 










# function all dfs subtr per group ------------------------------------

funcallsub <- function(df) {
  
  
  dfsub <- df %>% 
    group_by(SUBTRANSECTO) %>% 
    summarise(
      CONTEO = n(),
      N_INVESTIGATOR = mfv1(PERSONAS, na_rm = T),
      across(.cols = c(AÑO:TRANSECTO, DIAYEAR, DIAWEEK, RECORRIDO, CLASE:ESPECIE,
                       DIRECCION:MUERTO, TEMPORADA, CAÑADA, ESCURRIMIENTOS),
             .fns = list(MOST = ~mfv1(.x, na_rm = TRUE)),
             .names = "{.col}"),
      across(.cols = c(ALTITUD, DEG:TEMP_SUELO, CARROS_TUX:TOTAL_CARROS),
             .fns = list(promedio = ~mean(x = .x, trim = 0.05, na.rm=TRUE)), 
             .names = "{.col}"))
  
  
  # reemplazo valores faltantes
  dfsub <- dfsub %>% 
    mutate(across(.cols = where(is.numeric), 
                  .fns = ~replace(.x, is.na(.x)|is.nan(.x), mean(.x, na.rm=T))))
  
  return(dfsub)
  
  
}


















# limpiar nas por promedio ------------------------------------------------

funcprueba <- function(listdfs) {
  
  listdf <- map(listdfs, 
                   ~ .x %>% 
                     mutate(across(.cols = where(is.numeric),
                                   .fns = ~replace(.x, 
                                                   is.na(.x)|is.nan(.x), 
                                                   mean(.x, na.rm=T)))))
  
  return(listdf)
}



#funcprueba(dfgrupos2)

## si funciona

# -----------------------------------------------------------------







# dfs ready to analisys ------------------------------------------------

#recibe como argumento una lista de dataframes para preparar todos
#para el analisis

func_df_ready <- function(listdfs) {
  
  #PREPARE DATA AGREGAR
  dfenvi <- dfenvi %>% 
    select(c(SUBTRANSECTO, ends_with("MEAN")))
  
  dfland2 <- dfland2 %>% 
    select(c(SUBTRANSECTO, ends_with("MEAN")))
  
  
  
  
  listdf <- map(listdfs, 
                ~ .x %>% 
                  group_by(SUBTRANSECTO) %>% 
                  summarise(
                    CONTEO = n(),
                    N_INVESTIGATOR = mfv1(PERSONAS, na_rm = T),
                    across(.cols = c(ALTITUD, DEG:TEMP_SUELO, CARROS_TUX:TOTAL_CARROS),
                           .fns = list(promedio = ~mean(x = .x, trim = 0.05, na.rm=TRUE)),
                           .names = "{.col}"),
                    across(.cols = c(AÑO, SEMANA, DIA, DIAYEAR, TRANSECTO, RECORRIDO,
                                     VIVO, MUERTO, CAÑADA, ESCURRIMIENTOS),
                           .fns = list(moda = ~mfv1(.x, na_rm = TRUE)),
                           .names = "{.col}"),
                    across(.cols = c(MES, TURNO, DIAWEEK, CLASE:ESPECIE, 
                                     DIRECCION:CONDICION, TEMPORADA),
                           .fns = list(moda1 = ~mfv1(.x, na_rm = FALSE)),
                           .names = "{.col}")) %>% 
                    mutate(across(.cols = c(where(is.numeric), -c(SUBTRANSECTO)),
                                  .fns = ~replace(.x, 
                                                is.na(.x)|is.nan(.x), 
                                                mean(.x, na.rm=T))),
                           across(.cols = where(is.character),
                                  .fns = ~replace(.x, 
                                                  is.na(.x)|is.nan(.x), 
                                                  mfv1(.x, na_rm=FALSE))),
                           SUBTRANSECTO = replace(SUBTRANSECTO, is.na(SUBTRANSECTO), 0)
                    ) %>% 
                  fill(-c(SUBTRANSECTO), .direction = "downup") %>% 
                  left_join(y = dfenvi, by = "SUBTRANSECTO") %>% 
                  left_join(y = dfland2, by = "SUBTRANSECTO") %>% 
                  mutate(across(.cols = ends_with("MEAN"),
                                .fns = ~replace(.x, is.na(.x), mean(.x, na.rm=TRUE))))
  
                )
  
  return(listdf)
  
  # AllOk
  
}


# EJEMPLO:
#  
# listready <- func_df_ready(dfgrupos2)
# View(listready)












# a solicitud de tereso ----------------------------
# seleccion variables en base a un equipo de expertos :)
# funcion seleccion vaiables importantes ----------------------------------


func_important_vars <- function(list_dfs) {
  
  list_dfs <- 
  map(list_dfs, .f = ~.x %>% 
        select(CONTEO, SUBTRANSECTO, TEMPORADA, ALTITUD, 
               TURNO,
               CLASE, DIST_RIOS_MEAN, DIST_URBANIZACION_MEAN, 
               DIST_AGRICOLA_MEAN, DIST_PENDIENTE_MEAN, NDVI_VEGETACION_MEAN,
               HI, SPD, TEMP_SUELO, DIST_ELECTRICOS_MEAN, NDMI_HUMEDAD_MEAN
               ) %>% 
        rename(PENDIENTES_MEAN=DIST_PENDIENTE_MEAN)
      )
  
  return(list_dfs)
}







# deprecated:
# funcion seleccionar variables (antropogenicas) --------------------------


func_important_var_antro <- function(list_dfs) {
  
  list_dfs <- 
    map(list_dfs, .f = ~.x %>% 
          select(CONTEO, SUBTRANSECTO, DIAWEEK,
                 CLASE, DIST_URBANIZACION_MEAN, 
                 DIST_AGRICOLA_MEAN, DIST_PENDIENTE_MEAN,
                 DIST_ELECTRICOS_MEAN, TOTAL_CARROS)
    )
  
  return(list_dfs)
}






# funcion variables ambientales -------------------------------------------



func_important_var_ambien <- function(list_dfs) {
  
  list_dfs <- 
    map(list_dfs, .f = ~.x %>% 
          select(CONTEO, SUBTRANSECTO, TEMPORADA, ALTITUD, 
                 TURNO, CAÑADA, ESCURRIMIENTOS, 
                 CLASE, DIST_RIOS_MEAN, 
                 NDVI_VEGETACION_MEAN, TEMPORADA,
                 HI, SPD, TEMP_SUELO, NDMI_HUMEDAD_MEAN, NDWI_AGUA_MEAN,
                 NDVI_VEGETACION_MEAN, LST_TEMPERATURA_MEAN)
    )
  
  return(list_dfs)
}



