# ============================================================
#   PEC1 - Probabilidad y Estadística
#   Parte práctica: Cuestionario (lectura de ficheros "vendes")
#   Autor: Darío Aícua Ubierna
#   Fecha: Sys.Date()
# ============================================================

# 1. Configuración inicial -----------------------------------

# Establecer el directorio de trabajo (ajusta si cambia la ruta)
setwd("G:/Mi unidad/Asignaturas/Probabilidad y Estadística/repo/Probabilidad-y-Estadistica---Dario-Aicua-Ubierna---UOC---Ciencia-de-Datos/Reto 1/PEC1/Cuestionario")

# Mostrar el directorio actual
getwd()

# Mostrar los archivos disponibles
list.files()

# 2. Lectura de los archivos ---------------------------------

# Función genérica para leer archivos CSV con separador ';' y coma decimal ','
leer_vendes <- function(nombre_archivo) {
  read.csv2(nombre_archivo, na.strings = c("", "NA", "NaN"))
}

# Cargar los cuatro ficheros del cuestionario
vendes1 <- leer_vendes("vendes_pac1_P_15_1-1.csv")
vendes2 <- leer_vendes("vendes_pac1_P_15_2-1.csv")
vendes3 <- leer_vendes("vendes_pac1_P_15_3-1.csv")
vendes4 <- leer_vendes("vendes_pac1_P_15_4-1.csv")

# 3. Verificación rápida -------------------------------------

# Mostrar las primeras filas de cada dataset
cat("\n--- vendes 1 ---\n"); head(vendes1)
cat("\n--- vendes 2 ---\n"); head(vendes2)
cat("\n--- vendes 3 ---\n"); head(vendes3)
cat("\n--- vendes 4 ---\n"); head(vendes4)

# Mostrar estructura de columnas (tipos de datos)
cat("\n\n--- Estructuras de cada dataset ---\n")
str(vendes1)
str(vendes2)
str(vendes3)
str(vendes4)
#Nombres columnas
names(vendes1)
names(vendes2)
names(vendes3)
names(vendes4)

#CUESTIONARIO
#Pregunta 1:
dat_preg_1 <- c(9,9,3,10,6,7,10,6,10)

media   <- round(mean(dat_preg_1), 3)
sd_pop  <- round(sqrt(mean((dat_preg_1 - mean(dat_preg_1))^2)), 3)  # desviación estándar poblacional
mediana <- round(median(dat_preg_1), 3)

cat("x̄ =", formatC(media, format = "f", digits = 3),
   "\nσ_x =", formatC(sd_pop, format = "f", digits = 3),
   "\nm =", formatC(mediana, format = "f", digits = 3), "\n")

# Pregunta 2
ni <- c(10, 7, 9, 5)     # frecuencias
xj <- c(88, 80, 102, 112) # valores medios o clases

datos_expand <- rep(xj, ni)
datos_expand
table(datos_expand)

# total de observaciones
N <- sum(ni)

# Media ponderada
media <- sum(ni * xj) / N

# Desviación estándar poblacional (ponderada)
sd_pop <- sqrt(sum(ni * (xj - media)^2) / N)

# Para la mediana, necesitamos expandir los datos según sus frecuencias
datos_expand <- rep(xj, ni)
mediana <- median(datos_expand)

# Redondear a tres decimales
media <- round(media, 3)
sd_pop <- round(sd_pop, 3)
mediana <- round(mediana, 3)

cat("x̄ =", formatC(media, format = "f", digits = 3),
    "\nσ_x =", formatC(sd_pop, format = "f", digits = 3),
    "\nm =", formatC(mediana, format = "f", digits = 3), "\n")

#Pregunta 3:

x <- c(4,6,17,2,19,3,20,4,12,18)
quantile(x, probs = c(0.25, 0.5, 0.75), type = 6)  # método n+1

#Pregunta 4:
# Datos
xi <- c(3, 8, 11)
ni <- c(2, 5, 6)

# Muestra expandida
datos <- rep(xi, ni)

# Q1, mediana y Q3 con el método teórico (n+1) -> type = 6
qs <- quantile(datos, probs = c(0.25, 0.5, 0.75), type = 6)

Q1 <- round(qs[1], 3)
m  <- round(qs[2], 3)
Q3 <- round(qs[3], 3)

cat("Q1 =", formatC(Q1, format="f", digits=3),
    "\nm  =", formatC(m,  format="f", digits=3),
    "\nQ3 =", formatC(Q3, format="f", digits=3), "\n")

#Pregunta 7 (5 y 6 eran teóricas):
# Leemos el fichero vendes_pac1_P_15_3-1.csv
vendes3 <- read.csv2("vendes_pac1_P_15_3-1.csv", na.strings = c("", "NA", "NaN"))

# Comprobamos las columnas disponibles
names(vendes3)

# Si existe la columna "PreuAm2" (puede aparecer con ese nombre o similar, ojo a tildes o espacios)
# Generamos el histograma
hist(vendes3$PreuAm2,
     main = "Histograma de PreuAm2",
     xlab = "Precio por m²",
     ylab = "Frecuencia absoluta",
     col = "lightblue",
     border = "black",
     ylim = c(0, 30))   # <— fija el rango del eje Y de 0 a 30)

#Pregunta 8:
# Leemos el fichero vendes_pac1_P_15_1-1.csv
vendes1 <- read.csv2("vendes_pac1_P_15_1-1.csv", na.strings = c("", "NA", "NaN"))

# Comprobamos las columnas disponibles
names(vendes1)

# Resumen de la variable PreuDm2
summary(vendes1$PreuDm2)

#Pregunta 9:
# Leer el fichero con el formato correcto (coma decimal y ; como separador)
vendes1 <- read.csv2("vendes_pac1_P_15_1-1.csv", na.strings = c("", "NA", "NaN"))

# Comprobar las variables disponibles
names(vendes1)

# Boxplot de la variable PreuAm2
boxplot(vendes1$PreuAm2,
        main = "Boxplot de PreuAm2",
        ylab = "Precio por m² (antes de reformas)",
        col = "lightblue",
        border = "black")

