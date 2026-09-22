install.packages("rio")
library("rio")
mis_datos <- rio::import(file.choose())
mis_datos

# Instalar corrplot si no lo tienes:
if (!require(corrplot)) install.packages("corrplot")
library(corrplot)

# Paleta de colores elegante (de azul oscuro a rojo intenso pasando por blanco)
col_paleta <- colorRampPalette(c("#2166AC", "#67A9CF", "#F7F7F7", "#FDDBC7", "#B2182B"))(200)

grafico_eda_completo <- function(x, nombre_var = "Variable") {
  # 1. Cálculo de medidas descriptivas
  stats <- c(
    "Mínimo"       = min(x, na.rm = TRUE),
    "1er Cuartil"  = quantile(x, 0.25, na.rm = TRUE),
    "Mediana"      = median(x, na.rm = TRUE),
    "Media"        = mean(x, na.rm = TRUE),
    "3er Cuartil"  = quantile(x, 0.75, na.rm = TRUE),
    "Máximo"       = max(x, na.rm = TRUE),
    "Desv. Est."   = sd(x, na.rm = TRUE)
  )
  
  # 2. Configurar layout: 2 filas (Gráficos a la izq, Tabla descriptiva a la der)
  # Matriz: fila 1 -> Histograma (1) y Tabla (3)
  #         fila 2 -> Boxplot (2)    y Tabla (3)
  layout(matrix(c(1, 3, 2, 3), nrow = 2, byrow = TRUE), widths = c(2.2, 1.2), heights = c(2, 1))
  
  # ----------------- PANEL 1: HISTOGRAMA -----------------
  par(mar = c(3.5, 4, 3, 1))
  h <- hist(x, col = "#7293CB", border = "white",
            main = paste("Distribución de", nombre_var),
            xlab = "", ylab = "Densidad", prob = TRUE)
  
  # Curva normal teórica de referencia
  x_seq <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = 100)
  lines(x_seq, dnorm(x_seq, mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE)), 
        col = "#D35E60", lwd = 2)
  grid(nx = NA, ny = NULL, col = "gray90")
  
  # ----------------- PANEL 2: BOXPLOT -----------------
  par(mar = c(4, 4, 0.5, 1))
  boxplot(x, horizontal = TRUE, col = "#E1974C", border = "#8C564B",
          xlab = nombre_var, pch = 16, cex = 1.1)
  grid(nx = NULL, ny = NA, col = "gray90")
  
  # ----------------- PANEL 3: TARJETA DE RESUMEN -----------------
  par(mar = c(2, 0.5, 3, 1))
  plot.new() # Lienzo en blanco para escribir texto formateado
  title(main = "Estadísticos", cex.main = 1.1, col.main = "#333333")
  
  # Fondo sombreado tipo recuadro
  rect(0.02, 0.05, 0.98, 0.95, col = "#F7F9FB", border = "#D0D7DE", lwd = 1.5)
  
  # Coordenadas verticales para el texto
  y_pos <- seq(0.85, 0.15, length.out = length(stats))
  
  for (i in seq_along(stats)) {
    # Nombre del estadístico (alineado a la izquierda)
    text(0.08, y_pos[i], names(stats)[i], adj = 0, font = 2, cex = 0.95, col = "#2C3E50")
    # Valor numérico (alineado a la derecha)
    text(0.92, y_pos[i], sprintf("%.2f", stats[i]), adj = 1, font = 1, cex = 0.95, col = "#1F2D3D")
  }
  
  # Restaurar disposición gráfica por defecto
  layout(1)
  par(mar = c(5, 4, 4, 2) + 0.1)
}


# Ejercicio 1 -------------------------------------------------------------
mis_datos <- read.table("ventas.txt", header = TRUE)
names(mis_datos) <- tolower(names(mis_datos))
# a) ----------------------------------------------------------------------
str(mis_datos)
summary(mis_datos)
colSums(is.na(mis_datos))

# Matriz
mat_cor_ventas <- cor(mis_datos)

corrplot(mat_cor_ventas, 
         method = "color",           # Cuadrados con relleno de color
         col = col_paleta,           # Gradiente frío a cálido
         addCoef.col = "black",      # Color de los números de correlación
         number.cex = 0.85,          # Tamaño del texto numérico
         tl.col = "black", tl.srt = 45, # Color y rotación de las etiquetas
         diag = TRUE,                # Muestra la diagonal principal
         title = "Matriz de Correlación: Ventas y Publicidad",
         mar = c(0, 0, 2, 0))

grafico_eda_completo(mis_datos$ventas, "Ventas")
grafico_eda_completo(mis_datos$tv, "Inversión en TV")
grafico_eda_completo(mis_datos$radio, "Inversión en Radio")
grafico_eda_completo(mis_datos$periodico, "Inversión en Periódico")

#Gráfico de los tres predictores 
par(mfrow = c(1, 3))

#ventas y TV
plot(mis_datos$tv, mis_datos$ventas, 
     xlab="Presupuesto TV", ylab="Ventas", main="Ventas vs TV", col="blue") 
abline(lm(ventas ~ tv, data=mis_datos), col="darkblue", lwd=2)
legend("topleft", legend=paste("r =", round(cor(mis_datos$tv, mis_datos$ventas), 3)), bty="n")
#se observa una relación positiva, lo que indicaria que a mayor presupuesto en tv las ventas aumentarian

#ventas vs radio
plot(mis_datos$radio, mis_datos$ventas, 
     xlab="Presupuesto Radio", ylab="Ventas", main="Ventas vs Radio", col="red")
abline(lm(ventas ~ radio, data=mis_datos), col="darkred", lwd=2)
legend("topleft", legend=paste("r =", round(cor(mis_datos$radio, mis_datos$ventas), 3)), bty="n")
#se observa mayor dispersión  

#ventas vs periódico
plot(mis_datos$periodico, mis_datos$ventas, 
     xlab="Presupuesto Periódico", ylab="Ventas", main="Ventas vs Periódico", col="darkgreen")
abline(lm(ventas ~ periodico, data=mis_datos), col="darkolivegreen", lwd=2)
legend("topleft", legend=paste("r =", round(cor(mis_datos$periodico, mis_datos$ventas), 3)), bty="n")

par(mfrow = c(1, 1))


# b) ----------------------------------------------------------------------
modelo <- lm(ventas ~ tv + radio + periodico, data=mis_datos)
summary(modelo)

# c) ----------------------------------------------------------------------
dato_c <- data.frame(tv = 147.05, radio = 23.3, periodico = 31)
predict(modelo, newdata = dato_c, interval = "confidence", level = 0.95)
#con un intervalo de confianza del 95%, el ingreso medio esperado por ventas para los locales que 
#inviertan 147.05 millones en tv, 23.3 millones en radio y 31 millones en periódicos,
#en promedio, las ventas se encuentra entre 13.71351 y 14.22463 millones.

# d) ----------------------------------------------------------------------
dato_d <- data.frame(tv = 160, radio = 26.7, periodico = 35.1)
predict(modelo, newdata = dato_d, interval = "prediction", level = 0.95)
#con un intervalo de confianza del 95% , el volumen de ventas para la nueva sucursal donde invierten 160 millones 
#en tv, 26.7 millones en radio y 35.1 millones en periódicos, el volumen de ventas para solo esa sucursal se 
#encontrarian entre 11.75153 y 18.62085 millones 


# e) ----------------------------------------------------------------------
# 1. E(Y|X=x0) representa el "Valor medio esperado". Es decir, el promedio
#    de las ventas para TODAS las regiones que invierten ese presupuesto
#    específico. Su intervalo de confianza solo cuantifica la 
#    incertidumbre al estimar los coeficientes del modelo (la media).
#
# 2. Y_nuevo|X=x0 representa una "Nueva observación". Es decir, estima las
#    ventas de UNA ÚNICA sucursal individual que invertirá ese presupuesto.
#
# ¿Por qué el intervalo de predicción es más amplio?
# Porque el intervalo de predicción debe incorporar dos fuentes de 
# incertidumbre simultáneas: 
# a) La inexactitud al estimar la recta de regresión (la media)
# b) La variabilidad individual irreducible (el término de error aleatorio) 
#    asociada al comportamiento único de esa nueva sucursal en particular


# Ejercicio 2 -------------------------------------------------------------

mis_datos_suelo <- rio::import("datos.txt")
names(mis_datos_suelo) <- tolower(names(mis_datos_suelo))

# a) ----------------------------------------------------------------------

#i) EDA: estructura, faltantes, descriptivos y correlación
dim(mis_datos_suelo)
str(mis_datos_suelo)
colSums(is.na(mis_datos_suelo))

#ii)
tabla_resumen_suelo <- do.call(cbind, lapply(mis_datos_suelo, summary))
print(round(tabla_resumen_suelo, 3))

#iii) Matriz 
mat_cor_suelo <- cor(mis_datos_suelo)

corrplot(mat_cor_suelo, 
         method = "color", 
         col = col_paleta, 
         addCoef.col = "black", 
         number.cex = 0.85, 
         tl.col = "black", tl.srt = 45, 
         diag = TRUE,
         title = "Matriz de Correlación: Biomasa y Propiedades del Suelo",
         mar = c(0, 0, 2, 0))

#iv) Reemplaza 'mis_datos_suelo' por el nombre con que cargaste datos.txt
grafico_eda_completo(mis_datos_suelo$biomasa, "Biomasa")
grafico_eda_completo(mis_datos_suelo$ph, "pH del Suelo")
grafico_eda_completo(mis_datos_suelo$k, "Potasio (k)")

par(mfrow = c(1, 3), mar = c(4.2, 4.2, 2.5, 1))

# Biomasa vs pH
plot(mis_datos_suelo$ph, mis_datos_suelo$biomasa,
     xlab = "pH", ylab = "Biomasa", 
     main = "Biomasa vs pH", pch = 16, col = "forestgreen")
abline(lm(biomasa ~ ph, data = mis_datos_suelo), col = "darkgreen", lwd = 2)
legend("topleft", legend = paste("r =", round(cor(mis_datos_suelo$ph, mis_datos_suelo$biomasa), 3)), bty = "n")

# Biomasa vs Potasio
plot(mis_datos_suelo$k, mis_datos_suelo$biomasa,
     xlab = "Potasio (k)", ylab = "Biomasa", 
     main = "Biomasa vs Potasio", pch = 16, col = "darkorange")
abline(lm(biomasa ~ k, data = mis_datos_suelo), col = "chocolate", lwd = 2)
legend("topleft", legend = paste("r =", round(cor(mis_datos_suelo$k, mis_datos_suelo$biomasa), 3)), bty = "n")

# Potasio vs ph (muestra la correlación entre predictores)
plot(mis_datos_suelo$ph, mis_datos_suelo$k,
     xlab = "pH", ylab = "Potasio (k)", 
     main = "Potasio vs ph", pch = 16, col = "purple")
abline(lm(k ~ ph, data = mis_datos_suelo), col = "purple4", lwd = 2)
legend("topleft", legend = paste("r =", round(cor(mis_datos_suelo$ph, mis_datos_suelo$k), 3)), bty = "n")

par(mfrow = c(1, 1))


# b) ----------------------------------------------------------------------
#Modelo de Regresión Lineal Simple: y ~ x2[cite: 4, 6]
mod_simple_suelo <- lm(biomasa ~ ph, data = mis_datos_suelo)
summary(mod_simple_suelo) #ver que significa estos numeros resultados interpretar


# c) ----------------------------------------------------------------------
#Residuos para estudiar el efecto parcial de x3 controlando x2[cite: 4, 6]
#i) Residuos de biomasa controlando ph
mod_biomasa_ph <- lm(biomasa ~ ph, data = mis_datos_suelo)
e_biomasa <- resid(mod_biomasa_ph)

#ii) Residuos de k controlando ph
mod_k_ph <- lm(k ~ ph, data = mis_datos_suelo)
e_k <- resid(mod_k_ph)

#iii) Gráfico de dispersión de residuos
plot(e_k, e_biomasa,
     xlab = "Residuos de Potasio ajustados por pH",
     ylab = "Residuos de Biomasa ajustados por pH",
     main = "Aporte Parcial de Potasio sobre Biomasa",
     pch = 16, col = "steelblue")
abline(lm(e_biomasa ~ e_k), col = "navy", lwd = 2)
legend("topleft", legend = paste("r parcial =", round(cor(e_biomasa, e_k), 3)), bty = "n")

#iv) Modelo auxiliar con residuos: e_biomasa = alpha + gamma * e_k
mod_residuos <- lm(e_biomasa ~ e_k)
summary(mod_residuos)

#v) Modelo de regresión múltiple: biomasa ~ ph + k
mod_multiple_suelo <- lm(biomasa ~ ph + k, data = mis_datos_suelo)
summary(mod_multiple_suelo)

# Comparación numérica directa: gamma vs beta_3
coef(mod_residuos)["e_k"]
coef(mod_multiple_suelo)["k"]

# Tabla comparativa de coeficientes para proyectar en la presentación
tabla_comparativa <- data.frame(
  Metodo = c("Regresión Parcial (gamma)", "Regresión Múltiple (beta_3)"),
  Estimacion = c(coef(mod_residuos)["e_k"], coef(mod_multiple_suelo)["k"])
)
print(tabla_comparativa) #interpretaacion de estos numeros


# b) ----------------------------------------------------------------------
mod_simple_suelo <- lm(biomasa ~ ph, data = mis_datos_suelo)
summary(mod_simple_suelo)

# Extracción directa del coeficiente beta_2
beta_2 <- coef(mod_simple_suelo)["ph"]
print(beta_2)
#interpretacion
#Por cada unidad que se incrementa el pH del suelo, la biomasa
#promedio del pasto aumenta en aproximadamente 383.071 unidades de biomasa.

# c) ----------------------------------------------------------------------
# i. Ajustar Biomasa ~ pH y extraer residuos e_Y
mod_biomasa_ph <- lm(biomasa ~ ph, data = mis_datos_suelo)
e_biomasa <- resid(mod_biomasa_ph)

# ii. Ajustar Potasio ~ pH y extraer residuos e_X3
mod_k_ph <- lm(k ~ ph, data = mis_datos_suelo)
e_k <- resid(mod_k_ph)

# iii. Gráfico de dispersión de e_Y versus e_X3 con recta de regresión
plot(e_k, e_biomasa,
     xlab = "Residuos de Potasio controlando pH (e_k)",
     ylab = "Residuos de Biomasa controlando pH (e_biomasa)",
     main = "Aporte Parcial del Potasio (K) aislando el efecto del pH",
     pch = 16, col = "steelblue")
abline(lm(e_biomasa ~ e_k), col = "navy", lwd = 2)
grid()

# Correlación parcial visible en el gráfico
legend("topleft", 
       legend = paste("r=", round(cor(e_k, e_biomasa), 3)), 
       bty = "n")

#iv)
#e_y representa la variabilidad de la biomasa que el pH del suelo no pudo
#explicar. Es la biomasa "limpia" o desprovista de cualquier influencia directa del pH.

#e_k representa la variación en los niveles de potasio que es totalmente 
#independiente del pH. Dado que las propiedades químicas del suelo suelen estar 
#interrelacionadas, este residuo aísla la información nueva y única que aporta el 
#potasio, eliminando la redundancia que comparte con el pH.

#El gráfico e_biomasa vs e_k permite observar directamente si el potasio tiene una
#relación lineal real con la biomasa vegetal después de haber descontado el efecto del pH.


# d) ----------------------------------------------------------------------
# Ajuste del modelo sobre los residuos calculados en el ítem c
mod_residuos <- lm(e_biomasa ~ e_k)
summary(mod_residuos)

# Extracción y presentación de gamma_1 estimado
gamma_1_est <- unname(coef(mod_residuos)["e_k"])
cat("Estimación de gamma_1 (modelo auxiliar de residuos):", gamma_1_est, "\n")

# e) ----------------------------------------------------------------------
mod_multiple_suelo <- lm(biomasa ~ ph + k, data = mis_datos_suelo)
summary(mod_multiple_suelo)

# Extracción del coeficiente beta_3 (asociado a potasio 'k')
beta_3_est <- unname(coef(mod_multiple_suelo)["k"])
cat("Estimación de beta_3 (regresión múltiple):", beta_3_est, "\n")

# Tabla comparativa formal para la presentación/informe
tabla_comparativa <- data.frame(
  Parámetro = c("gamma_1 (Regresión de Residuos)", "beta_3 (Regresión Múltiple)"),
  Estimación = c(gamma_1_est, beta_3_est),
  Diferencia = c("-", abs(gamma_1_est - beta_3_est))
)
print(tabla_comparativa)


# Ejercicio 3 -------------------------------------------------------------

mis_datos_esp <- read.table("esperanza.txt", header = TRUE)
names(mis_datos_esp) <- tolower(names(mis_datos_esp))

# ------------------------------------------------------------------------------
# a) Análisis Exploratorio de Datos (EDA)
# ------------------------------------------------------------------------------

#i) 
dim(mis_datos_esp)
str(mis_datos_esp)

#ii) 
cat("Valores faltantes por variable:\n")
print(colSums(is.na(mis_datos_esp)))

#iii) 
tabla_resumen_esp <- do.call(cbind, lapply(mis_datos_esp, summary))
de_esp <- sapply(mis_datos_esp, sd)
tabla_resumen_esp <- rbind(tabla_resumen_esp, "Std.Dev" = round(de_esp, 2))
print(round(tabla_resumen_esp, 2))

#iv) Matriz
mat_cor_esp <- cor(mis_datos_esp)

corrplot(mat_cor_esp, 
         method = "color", 
         col = col_paleta, 
         addCoef.col = "black", 
         number.cex = 0.8, 
         tl.col = "black", tl.srt = 45, 
         diag = TRUE,
         title = "Matriz de Correlación: Esperanza de Vida e Indicadores",
         mar = c(0, 0, 2, 0))

grafico_eda_completo(mis_datos_esp$esp_vida, "Esperanza de Vida")
grafico_eda_completo(mis_datos_esp$asesinatos, "Tasa de Asesinatos")
grafico_eda_completo(mis_datos_esp$universitarios, "Porcentaje Universitarios")

#v) Gráficos de dispersión entre esp_vida y cada uno de los 7 predictores
par(mfrow = c(2, 4), mar = c(4.2, 4.2, 2.5, 1))

predictores <- names(mis_datos_esp)[names(mis_datos_esp) != "esp_vida"]

for (pred in predictores) {
  x_val <- mis_datos_esp[[pred]]
  y_val <- mis_datos_esp$esp_vida
  r_val <- round(cor(x_val, y_val), 3)
  
  plot(x_val, y_val,
       xlab = pred, 
       ylab = "Esp. de vida",
       main = paste("esp_vida vs", pred),
       pch = 16, col = "steelblue")
  
  # Línea de tendencia lineal
  abline(lm(y_val ~ x_val), col = "firebrick", lwd = 2)
  legend("topright", legend = paste("r =", r_val), bty = "n", cex = 0.9)
}

par(mfrow = c(1, 1))

# b) ----------------------------------------------------------------------
#i) Definir el modelo nulo (solo intercepto) y la fórmula del modelo completo
mod_nulo <- lm(esp_vida ~ 1, data = mis_datos_esp)

# Fórmula con todos los predictores posibles para el argumento 'scope'
formula_completa <- ~ habitantes + ingresos + analfabetismo + asesinatos + 
  universitarios + heladas + area

#etapa 1
add1(mod_nulo, scope = formula_completa, test = "F")

# Ingresa 'asesinatos'
mod_fwd1 <- update(mod_nulo, . ~ . + asesinatos)

#etapa 2
add1(mod_fwd1, scope = formula_completa, test = "F")

# Incorporamos 'universitarios' al modelo
mod_fwd2 <- update(mod_fwd1, . ~ . + universitarios)

# ETAPA 3:
add1(mod_fwd2, scope = formula_completa, test = "F")

mod_fwd3 <- update(mod_fwd2, . ~ . + heladas)

# etapa 4:
add1(mod_fwd3, scope = formula_completa, test = "F")

mod_final_fwd <- mod_fwd3
summary(mod_final_fwd)

#Intercepto beta_0 = 71.4714, Representa la esperanza de vida promedio 
#teórica (en años) para un estado con tasa cero de asesinatos, cero graduados 
#universitarios y cero días de heladas.

#Tasa de asesinatos beta_1 = -0.2896, Manteniendo constantes el porcentaje de
#universitarios y los días de heladas (ceteris paribus), por cada homicidio adicional
#por cada 100.000 habitantes, la esperanza de vida media disminuye en 0.29 años 

#Porcentaje de universitarios beta_2 = 0.0532, Controlando por criminalidad 
#y clima, por cada punto porcentual adicional de graduados universitarios, la esperanza 
#de vida media aumenta en 0.053 años 

#Días de heladas beta_3 = -0.0074, Controlando por educación y criminalidad, por cada 
#día adicional con temperaturas bajo cero, la esperanza de vida media disminuye en 
#0.0074 años 


# c) ----------------------------------------------------------------------
#i) 
mod_completo <- lm(esp_vida ~ habitantes + ingresos + analfabetismo + 
                     asesinatos + universitarios + heladas + area, 
                   data = mis_datos_esp)

#etapa 1 
drop1(mod_completo, test = "F")

# Eliminamos 'analfabetismo' (p = 0.81673)
mod_bwd1 <- update(mod_completo, . ~ . - analfabetismo)

# ------------------------------------------------------------------------------
# ETAPA 2: Evaluar la siguiente variable a eliminar
# ------------------------------------------------------------------------------
drop1(mod_bwd1, test = "F")

# Eliminamos 'ingresos' (p = 0.733022)
mod_bwd2 <- update(mod_bwd1, . ~ . - ingresos)

# ------------------------------------------------------------------------------
# ETAPA 3: Evaluar la siguiente variable a eliminar
# ------------------------------------------------------------------------------
drop1(mod_bwd2, test = "F")

# Eliminamos 'area' (p = 0.629604)
mod_bwd3 <- update(mod_bwd2, . ~ . - area)

# ------------------------------------------------------------------------------
# ETAPA 4: Evaluar la siguiente variable a eliminar
# ------------------------------------------------------------------------------
drop1(mod_bwd3, test = "F")

# Eliminamos 'habitantes' (p = 0.06505)
mod_bwd4 <- update(mod_bwd3, . ~ . - habitantes)

# ------------------------------------------------------------------------------
# ETAPA 5: Evaluar si alguna de las 3 restantes debe salir
# ------------------------------------------------------------------------------
drop1(mod_bwd4, test = "F")

# ------------------------------------------------------------------------------
# Modelo final seleccionado por Backward Selection (alpha = 0.01)
# ------------------------------------------------------------------------------
mod_final_bwd <- mod_bwd4
summary(mod_final_bwd)

#Tanto la selección progresiva (Forward) como la eliminación regresiva (Backward) 
#convergieron exactamente al mismo subconjunto de variables predictoras (asesinatos,
#universitarios, heladas), lo que entrega una evidencia sólida sobre la estabilidad 
#del modelo seleccionado para predecir la esperanza de vida.


# d) ----------------------------------------------------------------------

# 1. Modelo nulo y fórmula con todas las variables candidatas
mod_nulo <- lm(esp_vida ~ 1, data = mis_datos_esp)
formula_completa <- ~ habitantes + ingresos + analfabetismo + asesinatos + 
  universitarios + heladas + area

# Evaluar candidata a entrar
add1(mod_nulo, scope = formula_completa, test = "F")

mod_step1 <- update(mod_nulo, . ~ . + asesinatos)
# Evaluar qué variable ingresa
add1(mod_step1, scope = formula_completa, test = "F")

mod_step2 <- update(mod_step1, . ~ . + universitarios)
drop1(mod_step2, test = "F")

# Evaluar qué variable ingresa
add1(mod_step2, scope = formula_completa, test = "F")

mod_step3 <- update(mod_step2, . ~ . + heladas)
drop1(mod_step3, test = "F")

# Evaluar si ingresa una cuarta variable
add1(mod_step3, scope = formula_completa, test = "F")

# Modelo final seleccionado
mod_final_step <- mod_step3
summary(mod_final_step)

#En ninguna etapa posterior a una adición fue necesario remover una variable previa,
#debido a que las tres covariables seleccionadas aportan información complementaria 
#sin generar una multicolinealidad que anule su significancia individual al 1%. Como 
#resultado, los tres algoritmos analizados (Forward, Backward y Stepwise Bidireccional) 
#convergen de manera consistente al mismo modelo parsimonioso.

# ==============================================================================
# e) COMPARACIÓN DE CRITERIOS: R^2_adj, Cp de Mallows, AIC, BIC
# ==============================================================================

# 1. Definición de los modelos obtenidos
mod_fwd  <- mod_final_fwd
mod_bwd  <- mod_final_bwd
mod_step <- mod_final_step

# Modelo completo para obtener s^2 (estimador insesgado de sigma^2)
mod_completo <- lm(esp_vida ~ habitantes + ingresos + analfabetismo + 
                     asesinatos + universitarios + heladas + area, 
                   data = mis_datos_esp)

s2_completo <- summary(mod_completo)$sigma^2
n <- nrow(mis_datos_esp)

# 2. Función para calcular los 4 indicadores
calcular_criterios <- function(modelo) {
  p <- length(coef(modelo))  # número de parámetros (incluyendo intercepto)
  r2_adj <- summary(modelo)$adj.r.squared
  sse <- sum(resid(modelo)^2)
  cp  <- (sse / s2_completo) - n + (2 * p)
  aic_val <- AIC(modelo)
  bic_val <- BIC(modelo)
  
  c("R2_aj" = r2_adj, "Cp" = cp, "AIC" = aic_val, "BIC" = bic_val)
}

# 3. Construcción de la tabla comparativa solicitada
tabla_comparacion <- rbind(
  Forward  = calcular_criterios(mod_fwd),
  Backward = calcular_criterios(mod_bwd),
  Stepwise = calcular_criterios(mod_step)
)

print(round(tabla_comparacion, 4))

