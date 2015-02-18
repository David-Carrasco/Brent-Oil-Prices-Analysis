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

# Representación rapida de los datos anteriores
grafica1 <- qplot(ANYO, PrecioMedio, data = precioAnyo, geom = "line", colour = GASOLINA)                                        
print(grafica1)

# Ahora usando ggplot en vez de qplot:
grafica2 <- ggplot(precioAnyo, aes(ANYO,PrecioMedio, shape = GASOLINA, colour = GASOLINA)) + layer("line")
print(grafica2)

# En grafico de barras (lo tengo que hacer bien, da un error que no llego a ver)
b <- ggplot(precioAnyo, aes(x = ANYO, y = PrecioMedio), shape = GASOLINA)
b + geom_bar()

