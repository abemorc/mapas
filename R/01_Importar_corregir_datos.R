
# Cargar y limpieza de datos de excel ----------------------------------------



#importar los datos usados en el proyecto, asegurar tipo de dato de R correcto,
#correccion de errorees y NA de tipo character, es decir, dejar los dataframe
#listos para poder ser ocupados en R


# Son 4 archivos princpales


# 1. Data main ---------------------------------------------------------------

# asegurar que cada variable sea del tipo de dato correcto
df1 <- read_excel(here("Data/Ready_data/Data_main.xlsx"), sheet = "DATOSS",
                        col_types = c("numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "date", "date", 
                                      "date", "date", "numeric", "numeric", 
                                      "numeric", "text", "numeric", "numeric", 
                                      "numeric", "numeric", "text", "text", 
                                      "text", "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "text", "text", 
                                      "text", "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "text", "numeric", 
                                      "numeric"))
view(df1)
str(df1)


# reemplazar na y correccion tipo de datos --------------------------------


df1 <- df1 %>% 
  rename_all(toupper)
str(df1)

df1$DIRECCION

chartoreplace <- c("NA", "-", ".") 
df1 <- df1 %>% 
  mutate(across(.cols = where(is.character), 
                .fns = ~replace(.x, .x %in% chartoreplace, NA)))
view(df1)






###----------------------------------
#prueba: es lo mismo que arriba pero de otra notacion
remplazar <- function(q) {
  replace(x = q, list = q == "NA" , values = NA)
}

df1 <-  df1 %>% 
  mutate(across(.cols = where(is.character), .fns = remplazar))
###
# usando argumento .x
df12 <- df1 %>% 
  mutate(across(.cols = where(is.character), .fns = ~replace(.x, .x == "NA" , NA) ))
view(df12)
###--------------------------------------


# fechas y conversiones necesarias para trabajar en R
df1 <- df1 %>% 
  mutate(FECHA = as_date(FECHA),
         HORA = as_datetime(HORA),
         FECHAF = update(HORA, year = year(FECHA), month = month(FECHA),
                         mday = mday(FECHA))) %>% 
  select(FECHAF, TRANSECTO, SUBTRANSECTO, everything(), -c())

view(df1)
# Data_main ya esta preparado para empezar a trabajar los datos en R

#AllOk








# 2. Enviromental distances --------------------------------------------------


df2 <- read_excel(here("Data/Ready_data/Environmental_distances.xlsx"), 
                  sheet = "Distanccia a", 
                  col_types = c("numeric", "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", "numeric", 
                                "numeric"))
View(df2)

#ver si hay algun na
any(is.na(df2))
df2 <- df2 %>% 
  rename_all(toupper)
str(df2)

view(df2)
# AllOk





# 3. LandSat indices ------------------------------------------------------

df3 <- read.csv(here("Data/Ready_data/LandSat_indices.csv"))
df3 <- as_tibble(df3)

#ver si hay algun na
any(is.na(df3))
df3 <- df3 %>% 
  rename_all(toupper)
str(df3)

view(df3)

# AllOk









# 4. LandSat indices2 -----------------------------------------------------

df4 <- read.csv(here("Data/Ready_data/LandSat_indices2.csv"))
df4 <- as_tibble(df4)

#ver si hay algun na
any(is.na(df4))
df4 <- df4 %>% 
  rename_all(toupper)
str(df4)

view(df4)

# AllOk









# 5. LandSat indices3 -----------------------------------------------------

df5 <- read.csv(here("Data/Ready_data/LandSat_indices3.csv"))
df5 <- as_tibble(df5)

#ver si hay algun na
any(is.na(df5))
df5 <- df5 %>% 
  rename_all(toupper)
str(df5)

view(df5)

# AllOk











