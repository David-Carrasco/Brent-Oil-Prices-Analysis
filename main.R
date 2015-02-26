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

# Carga datos Gasolina 95 y Gasoleo A de 2000 a 2015
precioCombust <- read.csv('src/precio_combustible.csv', header = T, sep = ';')

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
  theme(legend.position = "top", axis.text.x = element_blank(), axis.ticks = element_blank(), axis.title.x = element_blank()) + 
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

# Vamos a analizar los precios de los combustibles en Madrid
# mediante un mapa de calor para cada tipo

#Con el fichero descargado con los datos de ambos combustibles,
#creamos el dataframe
#Fuente: http://geoportalgasolineras.es/

precMedio.gasoleo <- read.xls('src/PRECIOS_SHP_23022015.xls', sheet = "promedio_gasoleo", header = TRUE, colClasses=c("Provincia"= "character","Localidad"= "character","TIPO"= "character","GEOCODIGO"= "character"),stringsAsFactors=FALSE)
DatosGasoleo <- precMedio.gasoleo[,1:5] # Realizo esta seleccion ya que ponia muchas columnas sin dato a la izquierda de la talba

precMedio.gasolina <- read.xls('src/PRECIOS_SHP_23022015.xls', sheet = "promedio_gasolina", header = TRUE, colClasses=c("Provincia"= "character","Localidad"= "character","TIPO"= "character","GEOCODIGO"= "character"),  stringsAsFactors=FALSE)
DatosGasolina <- precMedio.gasolina[,1:5] # Realizo esta seleccion ya que ponia muchas columnas sin dato a la izquierda de la talba

#Leyenda

#Venta  
##P: Venta al público en general.
##R: Venta restringida a socios o 

#Remisión (Rem.):	
##dm: Datos procedentes del distribuidor minorista.
##OM: Datos procedentes del operador mayorista.

#Cargamos el shapefile con los datos geoespaciales de los municipios de Madrid
municipios <- readOGR(dsn = "src/shapefile", layer = "municipios")

########################## FORTIFY DEL DATAFRAME ############################################

municipios@data$id <- rownames(municipios@data)
municipios.df <- fortify(municipios)
municipios.df <- join(municipios.df, municipios@data, by="id")

########################## JOIN CON LOS PRECIOS ##############################################

municipios.df <- join(municipios.df, DatosGasolina, by = c('GEOCODIGO'), type = "inner")
municipios.df <- join(municipios.df, DatosGasoleo, by = c('GEOCODIGO', 'Localidad', 'Provincia'), type = 'inner')

######################### GGPLOT PRECIOS DE GASOLINA  #################################

plotGasolina <- ggplot(data=municipios.df, aes(x=long, y=lat, group=group)) 
plotGasolina <- plotGasolina + geom_polygon(aes(fill = PrecioGasolina))     # draw polygons
plotGasolina <- plotGasolina + theme(legend.position = "bottom", axis.ticks = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank())
plotGasolina <- plotGasolina  + labs(title="Precio medio Gasolina 95 por municipio")
#plotGasolina <- plotGasolina + geom_path(color="grey", linestyle=2) # draw boundaries
plotGasolina <- plotGasolina + coord_equal()
plotGasolina <- plotGasolina + scale_fill_gradient(low = "#F5FBEF", high = "#38610B", space = "Lab", na.value = "grey50", guide = "colourbar")

######################### GGPLOT PRECIOS DE GASOLEO  #################################

plotGasoleo <- ggplot(data=municipios.df, aes(x=long, y=lat, group=group)) 
plotGasoleo <- plotGasoleo + geom_polygon(aes(fill = PrecioGasoleo))         # draw polygons
plotGasoleo <- plotGasoleo + theme(legend.position = "bottom", axis.ticks = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank())
plotGasoleo <- plotGasoleo  + labs(title="Precio medio Gasoleo A por municipio")
#plotGasoleo <- plotGasoleo + geom_path(color="grey", linestyle=2)# draw boundaries
plotGasoleo <- plotGasoleo + coord_equal()
plotGasoleo <- plotGasoleo + scale_fill_gradient(low = "#FBEFEF", high = "#610B0B", space = "Lab", na.value = "grey50", guide = "colourbar")

#Ambos plot dividiendo el grid en 2
grid.arrange(plotGasolina, plotGasoleo, ncol=2)

