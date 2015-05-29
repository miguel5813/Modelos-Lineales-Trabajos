##-------------------------------------------------##
##-----------      Modelos Linelaes     -----------##
##-----------        Trabajo 01         -----------##
##-- Nombre:Miguel Ángel Sánchez guachamín


# 2.1 Leer el archivo de datos data.txt, y analizar de que estructura de datos se trata.
# Utilice la función read.table()
dir<-"C:/Users/viktor/Desktop/Miguel"
setwd(dir)
data<-read.table("data.txt",header = TRUE,dec=",",sep="\t")



# 2.2 Calcular el mínimo, la media, el máximo de la variable Edad.
# Utilice las funciones min(), mean(), max(), de ser necesario utilice 
# el parámetro na.rm = TRUE
min(data[,"EDAD"],na.rm=TRUE)
mean(data[,"EDAD"],na.rm=TRUE)
max(data[,"EDAD"],na.rm=TRUE)


# 2.3 Para la variable Genero, contar cuantos sujetos son de Genero: Femenino.
# Utilice la función table()
data_f<-subset(data,subset=data[,"SEXO"]=="FEMENINO")
table(data_f[,"SEXO"])


# 2.4 Encontrar la Edad mínima, media, máxima de los sujetos que Si son dependientes.
data_d<-subset(data,subset=data[,"dependiente"]=="N")
min(data_d[,"EDAD"],na.rm=TRUE)
mean(data_d[,"EDAD"],na.rm=TRUE)
max(data_d[,"EDAD"],na.rm=TRUE)


# 2.5 Identificar el tipo de elementos que contiene cada variable.
# Utilice la función typeof()
tipo_elemento<-numeric(ncol(data))
for(i in 1:ncol(data)){
  tipo_elemento[i]<-typeof(data[,i])
}
tipo_elemento


# 2.6 Identificar la clase de cada variable (columna).
# Utilice la función class()
clase_variable<-numeric(ncol(data))
for(i in 1:ncol(data)){
  clase_variable[i]<-class(data[,i])
}
clase_variable



# 2.7 Calcular la media de todas las variables numéricas (double, integer).
# Recordar que para un factor no es posible obtener la media debido a que 
# éstos representan variables
media<-numeric(ncol(data))
for(i in 1:ncol(data)){
  if(is.numeric(data[,i])==TRUE){
    media[i]<-mean(data[,i],na.rm=TRUE)
  }
}
media


# 2.8 Calcular el porcentaje de valores perdidos que contiene cada variable.
# Utilice la función is.na()
vacios<-numeric(ncol(data))
porcentaje<-numeric(ncol(data))
for(i in 1:ncol(data)){
  vacios[i]<-sum(is.na(data[,i]))
  porcentaje[i]<-vacios[i]/nrow(data)*100
}
porcentaje


# 3. Selecionando sujetos mediante un determinado criterio:
# 3.1 Seleccione los sujetos con una Edad mayor a 40 años.
# Utilice la función subset()
edad_mayor_40<-subset(data,subset=data[,"EDAD"]>40)



# 3.2 Seleccione los sujetos que tienen Vivienda Propia.
vivienda_propia<-subset(data,subset=data[,"tipo_vivienda"]=="PROPIO")



# 3.3 Seleccione los sujetos que tienen más ($>$) de dos cargas familiatres.
cargas_familiares<-subset(data,subset=data[,"num_cargas"]>2)


# 3.4 Seleccione los sujetos con una Deuda superior o igual a 500 dólares
# y más ($>$) de 8 Dias_Atraso.
deuda<-subset(data,subset=data[,"deuda"]>=500 & data[,"dias_atraso"]>8)



# 3.5 Seleccione los sujetos con un Score mayor o igual a 900 puntos, una Edad menor
# o igual a 35 años y con más ($>$) de 3 tarjetas de crédito (Numero_TC).
seleccion<-subset(data,subset=data[,"score"]>=900 & data[,"EDAD"]<=35 & data[,"NUM_TC_VIGENTES_REVTC"]>3)


# 4. Gráficos:
# 4.1 Realice un histograma de la variable Edad, utilice como color de relleno: red
hist(data[,"EDAD"],col = "RED") 


# 4.2 Realice un diagrama de cajas de la variable Edad, utilice como color de relleno: green
# Utilice la función boxplot()
boxplot(data[,"EDAD"],col = "GREEN")


#fin
