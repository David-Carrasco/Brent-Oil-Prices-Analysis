library(quantmod)
library(ggplot2)
library(plyr)
library(grid)
library(gridExtra)
library(scales)
library(xlsx)
library(rgdal)
library(gdata)
library(sp)

# Carga data barril brent
getSymbols('DCOILBRENTEU', src='FRED')
plot(DCOILBRENTEU)

# Carga datos Gasolina 95 y Gasoleo A de 2000 a 2015
precioCombust <- read.csv('precio_combustible.csv', header = T, sep = ';')

# Agrupado por año y gasolina en función del precio medio en ese rango
precioAnyo <- ddply(precioCombust, c("ANYO", "GASOLINA"), summarise, PrecioMedio = mean(PRECIO))

# Otra forma de hacerlo:
#      precioAnyo <- precioCombust %>% 
#        group_by(ANYO, GASOLINA) %>% 
#        summarise(medio = mean(PRECIO))

#Funcion para obtener como objeto Date todas las fechas del dataset precioCombust
createDate <- function(anio, mes){
  date_month <- ifelse(nchar(mes) == 1, paste(anio, mes, sep='-0'), paste(anio, mes, sep='-'))
  return(as.Date(paste(date_month, c('01'), sep='-'), format="%Y-%m-%d"))
}

#creamos dataset con la fecha correcta del dataset precioCombust
#con formato aaaa-mm
combustibles <- data.frame(fecha = createDate(precioCombust$ANYO, precioCombust$MES),
                           tipo = precioCombust$GASOLINA,
                           precio = precioCombust$PRECIO)


#Agrupamos mensualmente el dataset del Brent y lo filtramos
#desde el primer mes del 2000 hasta ahora y formateamos la fecha en formato aaaa-mm 
brent_monthly <- to.monthly(DCOILBRENTEU['2000::'])[,c(4)]
df.brent <- data.frame(fecha = as.Date(format(index(brent_monthly), "%Y-%m-%d")),
                       tipo = c('brent'),
                       precio = coredata(brent_monthly)[,1])

#Concatenamos df.brent bajo combustible con el tipo brent
df.total <- rbind(combustibles, df.brent)

##################################  GRAFICAS ###########################################

graficaGasolina <- ggplot(combustibles, aes(fecha, precio, shape = tipo, colour = tipo)) + 
  geom_line(aes(group = tipo)) + 
  theme(legend.position = "top", axis.text.x = element_blank(), axis.title.x = element_blank()) + 
  scale_x_date(labels = date_format("%Y"), breaks = "3 month") 


graficaBrent <- ggplot(df.brent, aes(fecha, precio, shape = tipo, colour = tipo)) + 
  geom_line(aes(group = tipo)) + 
  theme(legend.position = "top", axis.text.x = element_text(angle=90)) + 
  scale_x_date(labels = date_format("%Y-%b"), breaks = "3 month")

grid.arrange(graficaGasolina, graficaBrent, ncol=1) 

############################## CORRELACIONES ################################

cor(combustibles[combustibles$tipo == 95, c('precio')], df.brent$precio)
cor(combustibles[combustibles$tipo == 'GASOLEO_A', c('precio')], df.brent$precio)

# Parece que hay más correlación entre el diesel y el brent que con el gasoil 95 históricamente


########################### ANALISIS PRECIO GASOLINA EN MADRID ##########################

# Vamos a analizar los precios de ambos combustibles en Madrid y
# determinar las zonas más baratas de media donde repostar
# junto con la marca más barata en dichas zonas

#Con el fichero descargado con los datos,
#creamos el dataframe y limpiamos los datos
#Fuente: http://geoportalgasolineras.es/
gasolineras.madrid <- read.xls('PRECIOS_SHP_23022015.xls', sheet = 'datos', header = TRUE)

#Leyenda

#Venta  
##P: Venta al público en general.
##R: Venta restringida a socios o 

#Remisión (Rem.):	
##dm: Datos procedentes del distribuidor minorista.
##OM: Datos procedentes del operador mayorista.





####################### REPRESENTACION CON SHAPEFILE #########################################

municipios <- readOGR(dsn = ".", layer = "municipios")

plot(municipios)

# para ver los datos del shp;

municipios@data  # el campo GEOCODIGO mantiene el "0" a la izquierda.

# se carga el precio medio de gasolina y gasoleo por Municipios de Madrid

precMedio.gasoleo <- read.xls('PRECIOS_SHP_23022015.xls', sheet = "promedio_gasoleo", header = TRUE, colClasses=c("Provincia"= "character","Localidad"= "character","TIPO"= "character","GEOCODIGO"= "character"),stringsAsFactors=FALSE)
DatosGasoleo <- precMedio.gasoleo[,1:5] # Realizo esta seleccion ya que ponia muchas columnas sin dato a la izquierda de la talba

precMedio.gasolina <- read.xls('PRECIOS_SHP_23022015.xls', sheet = "promedio_gasolina", header = TRUE, colClasses=c("Provincia"= "character","Localidad"= "character","TIPO"= "character","GEOCODIGO"= "character"),  stringsAsFactors=FALSE)
DatosGasolina <- precMedio.gasolina[,1:5] # Realizo esta seleccion ya que ponia muchas columnas sin dato a la izquierda de la talba


########################## FORTIFY DEL DATAFRAME ############################################

municipios@data$id <- rownames(municipios@data)
municipios.df <- fortify(municipios)
municipios.df <- join(municipios.df, municipios@data, by="id")

########################## JOIN CON LOS PRECIOS ##############################################

municipios.df <- join(municipios.df, DatosGasolina, by = c('GEOCODIGO'), type = "inner")
municipios.df <- join(municipios.df, DatosGasoleo, by = c('GEOCODIGO', 'Localidad', 'Provincia'), type = 'inner')

######################### PLOT DE PRECIOS DE GASOLINA  #################################

ggp <- ggplot(data=municipios.df, aes(x=long, y=lat, group=group)) 
ggp <- ggp + geom_polygon(aes(fill = PrecioGasolina))         # draw polygons
ggp <- ggp + geom_path(color="grey", linestyle=2)# draw boundaries
ggp <- ggp + coord_equal()
ggp <- ggp + scale_fill_gradient(low = "#ffffcc", high = "#ff4444", space = "Lab", na.value = "grey50", guide = "colourbar")

print(ggp)

######################### PLOT DE PRECIOS DE GASOLINA  #################################

ggp <- ggplot(data=municipios.df, aes(x=long, y=lat, group=group)) 
ggp <- ggp + geom_polygon(aes(fill = PrecioGasoleo))         # draw polygons
ggp <- ggp + geom_path(color="grey", linestyle=2)# draw boundaries
ggp <- ggp + coord_equal()
ggp <- ggp + scale_fill_gradient(low = "#ffffcc", high = "#ff4444", space = "Lab", na.value = "grey50", guide = "colourbar")

print(ggp)
