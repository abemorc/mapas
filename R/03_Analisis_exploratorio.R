

# Analisis exploratorio ---------------------------------------------------




View(list_grupos)
View(list_ready)






# numero de incidentes por transecto
dftransecto <- transectos %>% 
  group_by(TRANSECTO) %>% 
  summarise(conteo = n())
dftransecto
view(dftransecto)

#graficando
ggplot(dftransecto) +
  geom_col(mapping = aes(x = TRANSECTO, y = conteo))

# observamos algo interesante hay un la mayoria de las ocurrencias se presentan
# en el primer transecto y van en descenso conforme se cambia de transecto

# esto es algo inusual ya que se esperaria que no hubiera un patron considerando
# que cada transecto es de igual tamaño y que se recorrieron el mismo numero de
# veces, ademas de que son consecutivos.
# 
# hay que observar si esa tendencia se debe a algun cambio en las condiciones
# del lugar o pudiera ser tambien debido a las condiciones en que se recogieron
# los datos, por ejemplo cansancio de la persona falta de tiempo, seguridad
# del lugar etc


dfsubtransectos <- transectos %>% 
  group_by(SUBTRANSEC) %>% 
  summarise(conteo = n())
dfsubtransectos

view(dfsubtransectos)

#update: ya esta esta parte
# aqui encontramos dos irregularidades, sub 0 y 90, 
# la observacion de 90 es 9
# preguntar a tereso que son para saber que hacer con ellos, por lo pronto
# se descartan
# corregir subtransectos raros
# ind <- transectos$SUBTRANSEC == 90
# ind
# transectos$SUBTRANSEC[ind] <- 9

ggplot(dfsubtransectos) +
  geom_col(mapping = aes(x = SUBTRANSEC, y = conteo))

#quitando outliers
ggplot(dfsubtransectos[-c(1,22),]) +
  geom_col(mapping = aes(x = SUBTRANSEC, y = conteo))

#encontramos casi el mismo patron que en la anterior, pero ya pudimos encontrar
#la razon de que el primer transecto tenga tantas observaciones, se debe a que
#la zona representada por el transecto 3 tiene gran cantidad de observaciones

#hay que poner especial interes en ese lugar

dfano <- transectos %>% 
  group_by(AÑO) %>% 
  summarise(conteo = n())
dfano
view(dfano)

#la mayoria de las observaciones fueron tomadas en el 2021
ggplot(dfano) +
  geom_col(mapping = aes(x = AÑO, y = conteo))

#por meses sera mas interesante ya que responden a una temporada y clima 
#especifico de cada mes

dfmes <- transectos %>% 
  group_by(MES) %>% 
  summarise(conteo = n())
dfmes
view(dfmes)

ggplot(dfmes) +
  geom_col(mapping = aes(x = MES, y = conteo))
# encontramos que los datos fueron tomados solo en 5 meses del año pero 
# estan distribuidos, es decir no existe un sesgo hacia principio o finales del
# año

#por semana para ver si encontramos algo mejor

dfsem <- transectos %>% 
  group_by(SEMANA) %>% 
  summarise(conteo = n())
dfsem
view(dfsem)
#graficar
ggplot(dfsem) +
  geom_col(mapping = aes(x = SEMANA, y = conteo))

#interesante ver que unasema tuvo varias observaciones, auqnue puede ser dibido
#a los dias de esta. pero parece que cada vez que se salia se encontraban 
#aproximadamente el mismo numero de casos

dfdia <- transectos %>% 
  group_by(DIA) %>% 
  summarise(conteo = n())
dfdia
view(dfdia)
#graficar
ggplot(dfdia) +
  geom_col(mapping = aes(x = DIA, y = conteo))

# parece que las salidas a campo se realizaron los ultimos dias de mes en su 
# mayoria

#veamos si las observaciones por dia eran aproximadamente igual
dfdiayear <- transectos %>% 
  group_by(DIAYEAR) %>% 
  summarise(conteo = n())
dfdiayear
view(dfdiayear)
view(dfdiayear)

ggplot(dfdiayear) +
  geom_col(mapping = aes(x = DIAYEAR, y = conteo))


dfdiaweek <- transectos %>% 
  group_by(DIAWEEK) %>% 
  summarise(conteo = n())
dfdiaweek
view(dfdiaweek)

ggplot(dfdiaweek) +
  geom_col(mapping = aes(x = DIAWEEK, y = conteo))

# se observa que los dias viernes es cuando mas incidentes hay y los miercoles













# veamos los anfibios
dfclase <- transectos %>% 
  group_by(CLASE) %>% 
  summarise(conteo = n())
dfclase

#graficar
ggplot(dfclase) +
  geom_col(mapping = aes(x = CLASE, y = conteo))
# hay mas anfibios




#veamos con especies
dfclase1 <- transectos %>% 
  group_by(CLASE, GENERO) %>% 
  summarise(conteo = n())
dfclase1
view(dfclase1)

ggplot(dfclase1, aes(x = GENERO, y = conteo)) + geom_boxplot()



# agrupado pr la temporada
# talvez sea necesario crear mas categorias aqui
dftemp <- transectos %>% 
  group_by(TEMPORADA) %>% 
  summarise(conteo = n())
dftemp

#graficar
ggplot(dftemp) +
  geom_col(mapping = aes(x = TEMPORADA, y = conteo))


dfsub <- transectos %>% 
  group_by(SUBTRANSEC) %>% 
  summarise(conteo = n())


######
# dividir por grupos el DataFrame
dfgeophis <- transectos %>% 
  group_by(GENERO) %>% 
  summarise(conteo = n())
dfgeophis


dfgeophis <- transectos %>% 
  group_by(GENERO) %>% 
  group_split()

str(dfgeophis)
unique(transectos$GENERO)

#el genero geophis
dfgeophis1 <-  dfgeophis[[10]]
view(dfgeophis1)










