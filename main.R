library(quantmod)
library(ggplot2)
library(data.table)

# Carga data barril brent
getSymbols('DCOILBRENTEU', src='FRED')
plot(DCOILBRENTEU)

# Carga datos Gasolina 95 y Gasoleo A de 2000 a 2015
precioCombust <- fread("precio_combustible.csv", header = TRUE)
