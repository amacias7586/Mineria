#             Integrantes:
#   ANGEL DAVID MACIAS RAMIRES
#   WILSON ARGANDOÑA MACIAS
#
#------------Nivel 6to ""---------------------------
#
#--------- Actividad de UNIDAD1-actividad 2 ------------------
#
#instalamos algunas librerias que podemos usar
if(!require(tidyverse)) {install.packages("tidyverse")}
if(!require(readr)) {install.packages("readr")}
if(!require(DescTools)) {install.packages("DescTools")}
if(!require(ggplot2)) {install.packages("ggplot2")}
if(!require(dplyr)) {install.packages("dplyr")}
install.packages("ggplot")
if(!require(EDAWR)) {install.packages("EDAWR")}
library("ggplot")
library("modeest")

#Cargamos las librerias
library("PASWR")
library("tidyverse")
library("readr")
library("ggplot2")
library("dplyr")
library("magrittr")
library("tibble")
library("dplyr")
library("caret")
library("devtools")
library("xlsx")


#cargamos el dataset
datos <- read.xlsx("Datos/DATASET_RP_1.xlsx", sheetIndex = 1)

#------------------- EXPLORACION DE LOS DATOS--------------------------------

#1 Explorar las variables, ver el tipo de dato y ciertos datos
glimpse(datos)

#2 *****ver las primeras 15 filas*****
#mostramos con head el dataframe datos y lo limitamos a 15
head(datos, n=15)

# *****motramos la ultimas 5 filas*****
#comando tail muestra las ultimas filas, pero lo limitamos a 5, para mostrar las ultimas 5 filas

tail(datos, n=5)

#3. *****Mostrar únicamente las columnas nombre cliente, fecha deuda, producto, total *****
# *****de crédito, plazo, cuotas canceladas, y rango mora. *****

#con select seleccionamos las columnas que deseamos mostrar con el orden establecido 
select(datos,Nombre.Cliente, Fecha.deuda, Producto, Total.credito, Plazo, Cuotas.canceladas, Rango.mora)

#4 *****Mostrar todos los datos, de los créditos con cuotas canceladas mayores a 10.*****
# con el comando filter, filtramos los datos con el dataframe datos, para llamarlos
filtro <- filter(datos,datos$Cuotas.canceladas > 10)

view(filtro)

#-----------------LIMPIEZA Y TRASFORMACION DE DATOS-------------------


#5. *****Ordenar la base de datos por total de crédito en orden descendente.*****
  #con el comando arrange, seleccionamos la base de datos, y su variable total.credito
dts_ordenados <- arrange(datos,desc(datos$Total.credito))

#y para observar que han sido ordenados ponemos
view(dts_ordenados)

#6. *****En la columna cuotas canceladas, son considerados valores inusuales aquellos valores superiores a 10 cuotas, 
  #debido a las políticas de crédito de la tienda. Por tanto, se desea crear una nueva columna reemplazando estos como valores 
  #faltantes. Reemplazar estos datos con el valor “NA”.*****

  #creamos una columna llamada credito limpio usando la funcion mutate y le asignamos lo mismo que tiene Cuotas.canceladas
datos <- datos %>% mutate(creditoLimpio = Cuotas.canceladas)

  #modificamos la columna creditoLimpio creada pero le condicionamos > 6 se ponemos NA
datos$Cuotas.canceladas <- replace(datos$creditoLimpio, datos$creditoLimpio>10, "NA")

#7 *****Crear una nueva columna, de nombre H_CREDITO, que contenga los valores: “habilitado para crédito” si las cuotas canceladas son mayores a 6, caso contrario 
   #el valor será “no habilitado para crédito”. *****

  #usamos mutate para crear una columna H_Credito con case_when como condicion cuotras.limpia es > 6 si cumple ponemos habilitado
  #y por falso ponemos no habilitado para el credito
datos <- datos %>%
  mutate(H_CREDITO = case_when(Cuotas.canceladas > 6 ~ "habilitado para el credito",
                               negate = TRUE ~ "no habilitao para el credito"))
#verificar datos para saber que estan correctamente ordenados
select(datos, Cuotas.canceladas,H_CREDITO)

#8 Se requiere que el nombre de un producto en específico se encuentre 
  #correctamente escrito. Existen productos ingresados como Audifono, y audifono. 
  #Crear una nueva columna que contenga el dato adecuado establecido como 
  #“Audifonos”, para todos los registros 

  #modificaremos con mutate la columna producto, para ello usamos str_replace_all y cambiamos la palabras Audifono -audifono 
  #al crear Audifono(creara Audifonoss, para ello damos para corregmios poniendo una nueva condicion), lo mismo con la palabra
  #secadora, encontraremos secado, secador, por lo que cambiaremos esos valores, con Secadora.
datos <- datos %>% mutate(
  Producto = str_replace_all(Producto, "Audifono", "Audifonos"),
  Producto = str_replace_all(Producto, "Audifonoss", "Audifonos"),
  Producto = str_replace_all(Producto, "audifono", "Audifonos"),
  Producto = str_replace_all(Producto, "audifon", "Audifonos"),
  Producto = str_replace_all(Producto, "secador", "Secadora"),
  Producto = str_replace_all(Producto, "secado", "Secadora"),
  Producto = str_replace_all(Producto, "cecadora", "Secadora")
)

#------------------------vISUALIZACION DE DATOS --------------------------------------

#9 Crear un gráfico de barras para visualizar distribución de rango mora. 

  #usamos ggplot para crear los datos, como data(usamos el dataframe datos) y geom_bar para crear una grafica de barra 
  # con rango Mora
ggplot(data = datos) + geom_bar(aes(x = Rango.mora))
                  

#10. Dibujar un diagrama de cajas y bigotes para analizar la distribución de la variable 
#plazo en función de la variable rango mora.
 #volvemos a usar ggplot para cargar el dataframe datos y usamos geom_boxplot para grafico de cajas y bigote, 
 #con rango x de plazo y Y de mora
ggplot(data = datos) + geom_boxplot(aes(x = Plazo, y = Rango.mora))
