library(quantmod)
library(ggplot2)
library(data.table)
library(dplyr)
library(plyr)

# Carga data barril brent
getSymbols('DCOILBRENTEU', src='FRED')
plot(DCOILBRENTEU)

# Carga datos Gasolina 95 y Gasoleo A de 2000 a 2015
precioCombust <- fread("precio_combustible.csv", header = TRUE)

# Agrupado por año y gasolina en función del precio medio en ese rango
precioAnyo <- ddply(precioCombust, c("ANYO", "GASOLINA"), summarise, PrecioMedio = mean(PRECIO))

# Otra forma de hacerlo:
#      precioAnyo <- precioCombust %>% 
#        group_by(ANYO, GASOLINA) %>% 
#        summarise(medio = mean(PRECIO))

#Funcion para obtener como objeto Date todas las fechas del dataset precioCombust
createDate <- function(anios, meses){
  date_month <- ifelse(nchar(mes) == 1, paste(anio, mes, sep='-0'), paste(anio, mes, sep='-'))
  return(as.Date(paste(date_month, c('01'), sep='-'), format="%Y-%m-%d"))
}

#creamos dataset con la fecha correcta del dataset precioCombust
#con formato aaaa-mm
combustibles <- data.frame(fecha = format(createDate(precioCombust$ANYO, precioCombust$MES), "%Y-%m"),
                           tipo = precioCombust$GASOLINA,
                           precio = precioCombust$PRECIO)

#Agrupamos mensualmente el dataset del Brent y lo filtramos
#desde el primer mes del 2000 hasta ahora y formateamos la fecha en formato aaaa-mm 
brent_monthly <- to.monthly(DCOILBRENTEU['2000::'])[,c(4)]
df.brent <- data.frame(fecha = format(index(brent_monthly), "%Y-%m"),
                       tipo = c('brent'),
                       precio = coredata(brent_monthly)[,1])

#Concatenamos df.brent bajo combustible con el tipo brent
df.total <- rbind(combustibles, df.brent)

#Ejemplo chart 3 precios a la vez
#chart1 <- ggplot(df.brent.oil, aes(fecha, PrecioMedio, shape = GASOLINA, colour = GASOLINA)) + layer("line")


##################################  GRAFICAS ###########################################

# Representación rapida de los datos anteriores
grafica1 <- qplot(ANYO, PrecioMedio, data = precioAnyo, geom = "line", colour = GASOLINA)                                        
print(grafica1)

# Ahora usando ggplot en vez de qplot:
grafica2 <- ggplot(precioAnyo, aes(ANYO,PrecioMedio, shape = GASOLINA, colour = GASOLINA)) + layer("line")
print(grafica2)

# En grafico de barras (lo tengo que hacer bien, da un error que no llego a ver)
b <- ggplot(precioAnyo, aes(x = ANYO, y = PrecioMedio), shape = GASOLINA)
b + geom_bar()

