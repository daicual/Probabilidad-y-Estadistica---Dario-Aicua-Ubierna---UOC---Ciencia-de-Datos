## ============================================================
## Ejercicio 1 - Estadística descriptiva (Sesión 1)
## a) Representación semana 1
## b) Comparación con semana 2 (usar porcentajes para comparar)
## Base R, sin paquetes externos. Genera PNGs y muestra en pantalla.
## ============================================================
# Función auxiliar para etiquetas encima de barras
add_bar_labels <- function(bp, heights, offset = 0.02, fmt = "%.0f") {
  # bp devuelve posiciones x intermedias
  text(x = bp, y = heights + max(heights) * offset,
       labels = sprintf(fmt, heights), xpd = TRUE)
}
# --- Datos (del enunciado) ---
programas <- c("Viva la estadística","El hermano","Matanza sangrienta","Informativo del día")

espectadores <- c(875, 925, 742, 682)  # Semana 1

# --- 1) Barras Semana 1 (frecuencia ABSOLUTA) ---
png("ej1_semana1_barras_absolutas.png", width = 1100, height = 700, res = 140)
par(mar = c(10, 5, 4, 2))
bp1 <- barplot(
  sem1_abs, names.arg = programas, las = 2,
  ylab = "Espectadores (n)",
  main = "Semana 1 - Audiencia por programa (frecuencia absoluta)"
)
grid(nx = NA, ny = NULL)
add_bar_labels(bp1, sem1_abs, fmt = "%.0f")
dev.off()

# --- Totales y porcentajes ---
total1 <- sum(sem1_abs)
total2 <- sum(sem2_abs)

sem1_pct <- sem1_abs / total1 * 100
sem2_pct <- sem2_abs / total2 * 100

# --- Tabla resumen (absolutos y % con dos decimales) ---
resumen <- data.frame(
  Programa = programas,
  Semana_1_abs = sem1_abs,
  Semana_1_pct = round(sem1_pct, 2),
  Semana_2_abs = sem2_abs,
  Semana_2_pct = round(sem2_pct, 2),
  check.names = FALSE
)

cat("\n==== Tabla resumen (absolutos y porcentajes) ====\n")
print(resumen, row.names = FALSE)
cat(sprintf("\nTotal semana 1: %d | Total semana 2: %d\n\n", total1, total2))

# ------------------------------------------------------------
# a) Gráfico de barras (SEMANA 1) - puedes elegir abs o %
# ------------------------------------------------------------

# Función auxiliar para etiquetas encima de barras
add_bar_labels <- function(bp, heights, offset = 0.02, fmt = "%.0f") {
  # bp devuelve posiciones x intermedias
  text(x = bp, y = heights + max(heights) * offset,
       labels = sprintf(fmt, heights), xpd = TRUE)
}

# --- 1) Barras Semana 1 (frecuencia ABSOLUTA) ---
png("ej1_semana1_barras_absolutas.png", width = 1100, height = 700, res = 140)
par(mar = c(10, 5, 4, 2))
bp1 <- barplot(
  sem1_abs, names.arg = programas, las = 2,
  ylab = "Espectadores (n)",
  main = "Semana 1 - Audiencia por programa (frecuencia absoluta)"
)
grid(nx = NA, ny = NULL)
add_bar_labels(bp1, sem1_abs, fmt = "%.0f")
dev.off()

# (opcional) Barras Semana 1 (frecuencia RELATIVA)
png("ej1_semana1_barras_porcentaje.png", width = 1100, height = 700, res = 140)
par(mar = c(10, 5, 4, 2))
bp1r <- barplot(
  sem1_pct, names.arg = programas, las = 2,
  ylab = "Porcentaje (%)",
  main = "Semana 1 - Audiencia por programa (frecuencia relativa)"
)
grid(nx = NA, ny = NULL)
add_bar_labels(bp1r, sem1_pct, fmt = "%.2f")
dev.off()

# (opcional) Diagrama de sectores Semana 1
png("ej1_semana1_sectores.png", width = 900, height = 900, res = 140)
par(mar = c(2, 2, 4, 2))
pie(sem1_pct,
    labels = paste0(programas, "\n", sprintf("%.2f%%", sem1_pct)),
    main = "Semana 1 - Distribución porcentual")
dev.off()

# ------------------------------------------------------------
# b) Comparación Semana 1 vs Semana 2 (mejor con PORCENTAJE)
# ------------------------------------------------------------

mat_pct <- rbind(sem1_pct, sem2_pct)
rownames(mat_pct) <- c("Semana 1", "Semana 2")
colnames(mat_pct) <- programas

png("ej1_comparacion_barras_porcentaje.png", width = 1300, height = 700, res = 140)
par(mar = c(10, 6, 4, 2))
bp_cmp <- barplot(
  mat_pct, beside = TRUE, las = 2,
  ylab = "Porcentaje (%)",
  main = "Comparación de audiencia por programa (porcentajes)"
)
abline(h = seq(0, 100, by = 10), lty = 3)
# Etiquetas por barra
heights <- as.vector(mat_pct)
add_bar_labels(bp_cmp, heights, fmt = "%.2f")
legend("topright", legend = rownames(mat_pct), bty = "n",
       fill = gray.colors(nrow(mat_pct)))
dev.off()

# (opcional) Dos pasteles lado a lado para comparar
png("ej1_comparacion_sectores.png", width = 1400, height = 700, res = 140)
par(mfrow = c(1, 2), mar = c(2, 2, 4, 2))
pie(sem1_pct, labels = paste0(programas, "\n", sprintf("%.2f%%", sem1_pct)),
    main = "Semana 1 - Porcentajes")
pie(sem2_pct, labels = paste0(programas, "\n", sprintf("%.2f%%", sem2_pct)),
    main = "Semana 2 - Porcentajes")
par(mfrow = c(1, 1))
dev.off()

# ------------------------------------------------------------
# Mostrar en pantalla los gráficos principales (si ejecutas interactivo)
# ------------------------------------------------------------
op <- par(no.readonly = TRUE)

# Semana 1 (barras absolutas)
par(mar = c(10, 5, 4, 2))
bp_demo1 <- barplot(
  sem1_abs, names.arg = programas, las = 2,
  ylab = "Espectadores (n)",
  main = "Semana 1 - Audiencia por programa (frecuencia absoluta)"
)
grid(nx = NA, ny = NULL); add_bar_labels(bp_demo1, sem1_abs, fmt = "%.0f")

# Comparación porcentajes (agrupadas)
readline("\nPulsa ENTER para ver el gráfico comparativo de porcentajes...")
par(mar = c(10, 6, 4, 2))
bp_demo2 <- barplot(
  mat_pct, beside = TRUE, las = 2,
  ylab = "Porcentaje (%)",
  main = "Comparación de audiencia por programa (porcentajes)"
)
abline(h = seq(0, 100, by = 10), lty = 3)
add_bar_labels(bp_demo2, as.vector(mat_pct), fmt = "%.2f")
legend("topright", legend = rownames(mat_pct), bty = "n",
       fill = gray.colors(nrow(mat_pct)))

par(op)

cat("Archivos generados:\n",
    "- ej1_semana1_barras_absolutas.png\n",
    "- ej1_semana1_barras_porcentaje.png\n",
    "- ej1_semana1_sectores.png\n",
    "- ej1_comparacion_barras_porcentaje.png\n",
    "- ej1_comparacion_sectores.png\n", sep = "")

