############################################
# TRABAJO 3 - Encuesta Satisfacción Hospital
# Autor: Felipe Morales (plantilla)
# Objetivo: Predecir VG y evaluar supuestos del modelo
# Requisitos: R 4.x, RStudio
############################################

# Recomendado para evitar strings como factor de forma implícita
options(stringsAsFactors = FALSE)

## 0) Paquetes ---------------------------------------------------------------
paquetes <- c(
  "readxl",     # leer Excel
  "dplyr",      # data wrangling
  "janitor",    # limpiar nombres
  "ggplot2",    # gráficos
  "corrplot",   # matriz de correlación
  "broom",      # tidy/augment/glance de modelos
  "car",        # durbinWatsonTest, VIF
  "lmtest",     # bptest, coeftest
  "sandwich",   # errores robustos (HC)
  "MASS",       # stepAIC
  "psych",      # describe()
  "ggpubr"      # utilidades de gráficos (opcional)
)

instalar_si_falta <- function(pkgs){
  faltan <- pkgs[!pkgs %in% rownames(installed.packages())]
  if(length(faltan) > 0) install.packages(faltan, dependencies = TRUE)
}
instalar_si_falta(paquetes)
invisible(lapply(paquetes, require, character.only = TRUE))

## 1) Ruta y lectura de datos -----------------------------------------------
# Usa "/" en Windows para evitar problemas con "\"
ruta_excel_win <- "C:/Users/piped/OneDrive/Escritorio/MBA2025/METODOS(R)/Trabajo_3/satisfaccion_hospital.xlsx"

if(!file.exists(ruta_excel_win)){
  stop(paste0(
    "No se encontró el archivo en: ", ruta_excel_win, 
    "\nPor favor, corrige 'ruta_excel_win' con la ubicación exacta del Excel."
  ))
}

datos <- readxl::read_excel(ruta_excel_win)
datos <- janitor::clean_names(datos)  # nombres a snake_case/minúsculas

# Chequeo general
cat("\nEstructura inicial de la base:\n")
str(datos)
cat("\nEstadísticos descriptivos:\n")
print(psych::describe(datos))

## 2) Estandarización de nombres esperados ----------------------------------
# Buscamos las columnas esperadas permitiendo nombres alternativos.
# Target: s, e, di, c, d, p1..p7, p8, vg
find_col <- function(cands, cols){
  cands <- tolower(cands)
  hit <- intersect(cands, cols)
  if(length(hit) == 0) NA_character_ else hit[1]
}

cols <- names(datos)

old_s  <- find_col(c("s","sexo"), cols)
old_e  <- find_col(c("e","edad"), cols)
old_di <- find_col(c("di","dias_ingreso","dias_de_ingreso","dias","dias_estadia","dias_estadía"), cols)
old_c  <- find_col(c("c","cirugia","cirugía"), cols)
old_d  <- find_col(c("d","servicio"), cols)
old_vg <- find_col(c("vg","valoracion_general","valoración_general","satisfaccion_global","satisfacción_global"), cols)
old_p1 <- find_col(c("p1","item1"), cols)
old_p2 <- find_col(c("p2","item2"), cols)
old_p3 <- find_col(c("p3","item3"), cols)
old_p4 <- find_col(c("p4","item4"), cols)
old_p5 <- find_col(c("p5","item5"), cols)
old_p6 <- find_col(c("p6","item6"), cols)
old_p7 <- find_col(c("p7","item7"), cols)
old_p8 <- find_col(c("p8","solucion","solución","solucion_del_problema","solución_del_problema"), cols)

faltan <- c(
  if(is.na(old_s))  "s/sexo",
  if(is.na(old_e))  "e/edad",
  if(is.na(old_di)) "di/dias_ingreso",
  if(is.na(old_c))  "c/cirugia",
  if(is.na(old_d))  "d/servicio",
  if(is.na(old_p1)) "p1",
  if(is.na(old_p2)) "p2",
  if(is.na(old_p3)) "p3",
  if(is.na(old_p4)) "p4",
  if(is.na(old_p5)) "p5",
  if(is.na(old_p6)) "p6",
  if(is.na(old_p7)) "p7",
  if(is.na(old_p8)) "p8",
  if(is.na(old_vg)) "vg"
)
if(length(faltan) > 0){
  stop(paste0("No se encontraron las siguientes variables en la base: ",
              paste(faltan, collapse = ", ")))
}

# Renombrar de forma segura
datos <- datos %>%
  dplyr::rename(
    s  = dplyr::all_of(old_s),
    e  = dplyr::all_of(old_e),
    di = dplyr::all_of(old_di),
    c  = dplyr::all_of(old_c),
    d  = dplyr::all_of(old_d),
    p1 = dplyr::all_of(old_p1),
    p2 = dplyr::all_of(old_p2),
    p3 = dplyr::all_of(old_p3),
    p4 = dplyr::all_of(old_p4),
    p5 = dplyr::all_of(old_p5),
    p6 = dplyr::all_of(old_p6),
    p7 = dplyr::all_of(old_p7),
    p8 = dplyr::all_of(old_p8),
    vg = dplyr::all_of(old_vg)
  )

## 3) Tipos de datos y validaciones -----------------------------------------
to_numeric <- function(x){ suppressWarnings(as.numeric(x)) }
to_integer <- function(x){ suppressWarnings(as.integer(x)) }

datos <- datos %>%
  dplyr::mutate(
    s  = to_integer(s),     # 0=H, 1=M
    e  = to_numeric(e),
    di = to_numeric(di),
    c  = to_integer(c),     # 0/1
    p1 = to_numeric(p1), p2 = to_numeric(p2), p3 = to_numeric(p3),
    p4 = to_numeric(p4), p5 = to_numeric(p5), p6 = to_numeric(p6),
    p7 = to_numeric(p7),
    p8 = to_integer(p8),    # 0/1
    vg = to_numeric(vg),
    d  = as.factor(d)       # categórica 
  )

# Chequeo de NA por variable de interés
cat("\nConteo de NA en variables clave:\n")
print(sapply(datos[, c("s","e","di","c","p1","p2","p3","p4","p5","p6","p7","p8","vg")], function(x) sum(is.na(x))))

# Rango de VG (esperado 0-10)
cat("\nResumen VG:\n")
print(summary(datos$vg))

## 4) Correlaciones (numéricas) ---------------------------------------------
num_vars <- c("s","e","di","c","p1","p2","p3","p4","p5","p6","p7","p8","vg")
mat_num  <- datos %>% dplyr::select(dplyr::all_of(num_vars)) %>% as.data.frame()

m_cor <- stats::cor(mat_num, use = "pairwise.complete.obs", method = "pearson")
cat("\nMatriz de correlaciones (redondeada a 2 decimales):\n")
print(round(m_cor, 2))

corrplot::corrplot(m_cor, method = "color", addCoef.col = "black",
                   tl.col = "black", tl.srt = 45, number.cex = 0.6,
                   mar = c(0,0,1,0), title = "Matriz de correlaciones (Pearson)")

## 5) Modelo lineal COMPLETO -------------------------------------------------
form_full <- as.formula(vg ~ s + e + di + c + p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8)
mod_full <- stats::lm(form_full, data = datos)  # lm hace na.omit por defecto

cat("\n=== Resumen Modelo COMPLETO ===\n")
print(summary(mod_full))
cat("\nVIF (modelo completo):\n")
print(tryCatch(car::vif(mod_full), error = function(e) paste("VIF no disponible:", e$message)))

cat("\nIntervalos de confianza 95% (modelo completo):\n")
print(stats::confint(mod_full, level = 0.95))

## 6) Diagnósticos (modelo completo) ----------------------------------------
# Durbin–Watson
cat("\nDurbin–Watson (modelo completo):\n")
print(tryCatch(car::durbinWatsonTest(mod_full), error = function(e) paste("DW no disponible:", e$message)))
# Shapiro–Wilk
cat("\nShapiro–Wilk (residuos del modelo completo):\n")
print(tryCatch(stats::shapiro.test(stats::residuals(mod_full)), error = function(e) paste("Shapiro no disponible:", e$message)))
# Breusch–Pagan
cat("\nBreusch–Pagan (modelo completo):\n")
print(tryCatch(lmtest::bptest(mod_full), error = function(e) paste("BP no disponible:", e$message)))

# Densidad de residuos
res_full <- stats::residuals(mod_full)
ggplot2::ggplot(data.frame(res=res_full), ggplot2::aes(x = res)) +
  ggplot2::geom_density(linewidth = 1) +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +
  ggplot2::labs(title = "Densidad de residuos (modelo completo)", x = "Residual", y = "Densidad") +
  ggplot2::theme_minimal()

# Observado vs. predicho
augment_full <- broom::augment(mod_full)
ggplot2::ggplot(augment_full, ggplot2::aes(x = .fitted, y = vg)) +
  ggplot2::geom_point(alpha = 0.7) +
  ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  ggplot2::labs(title = "VG observado vs. VG predicho (modelo completo)",
                x = "VG predicho", y = "VG observado") +
  ggplot2::theme_minimal()

# Coeficientes con errores robustos (HC3)
cat("\nCoeficientes con errores robustos (HC3) - modelo completo:\n")
print(lmtest::coeftest(mod_full, vcov. = sandwich::vcovHC(mod_full, type = "HC3")))

## 7) Selección por AIC (modelo reducido) -----------------------------------
mod_step <- MASS::stepAIC(mod_full, direction = "both", trace = FALSE)
cat("\n=== Resumen Modelo REDUCIDO (stepAIC) ===\n")
print(summary(mod_step))
cat("\nVIF (modelo reducido):\n")
print(tryCatch(car::vif(mod_step), error = function(e) paste("VIF no disponible:", e$message)))
cat("\nIntervalos de confianza 95% (modelo reducido):\n")
print(stats::confint(mod_step, level = 0.95))

# Diagnósticos del reducido
cat("\nDurbin–Watson (modelo reducido):\n")
print(tryCatch(car::durbinWatsonTest(mod_step), error = function(e) paste("DW no disponible:", e$message)))
cat("\nShapiro–Wilk (residuos del modelo reducido):\n")
print(tryCatch(stats::shapiro.test(stats::residuals(mod_step)), error = function(e) paste("Shapiro no disponible:", e$message)))
cat("\nBreusch–Pagan (modelo reducido):\n")
print(tryCatch(lmtest::bptest(mod_step), error = function(e) paste("BP no disponible:", e$message)))

# Gráficos del reducido
res_step <- stats::residuals(mod_step)
ggplot2::ggplot(data.frame(res=res_step), ggplot2::aes(x = res)) +
  ggplot2::geom_density(linewidth = 1) +
  ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +
  ggplot2::labs(title = "Densidad de residuos (modelo reducido)", x = "Residual", y = "Densidad") +
  ggplot2::theme_minimal()

augment_step <- broom::augment(mod_step)
ggplot2::ggplot(augment_step, ggplot2::aes(x = .fitted, y = vg)) +
  ggplot2::geom_point(alpha = 0.7) +
  ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  ggplot2::labs(title = "VG observado vs. VG predicho (modelo reducido)",
                x = "VG predicho", y = "VG observado") +
  ggplot2::theme_minimal()

cat("\nCoeficientes con errores robustos (HC3) - modelo reducido:\n")
print(lmtest::coeftest(mod_step, vcov. = sandwich::vcovHC(mod_step, type = "HC3")))

## 8) Función de reporte compacto -------------------------------------------
reporte_modelo <- function(modelo, nombre = "Modelo"){
  cat("\n==============================\n")
  cat(paste0(nombre, "\n"))
  cat("==============================\n")
  
  g <- broom::glance(modelo)
  cols <- intersect(c("r.squared","adj.r.squared","statistic","p.value","AIC","BIC"),
                    names(g))
  print(g[, cols, drop = FALSE])
  
  cat("\nCoeficientes (tidy):\n")
  print(broom::tidy(modelo, conf.int = TRUE))
  
  cat("\nVIF (si aplica):\n")
  print(tryCatch(
    car::vif(modelo),
    error = function(e) paste("VIF no disponible:", e$message)
  ))
  
  cat("\nPruebas de supuestos:\n")
  print(tryCatch(
    car::durbinWatsonTest(modelo),
    error = function(e) paste("DW no disponible:", e$message)
  ))
  print(tryCatch(
    stats::shapiro.test(stats::residuals(modelo)),
    error = function(e) paste("Shapiro no disponible:", e$message)
  ))
  print(tryCatch(
    lmtest::bptest(modelo),
    error = function(e) paste("BP no disponible:", e$message)
  ))
  
  invisible(modelo)
}

## 9) Ejecutar reportes ordenados --------------------------------------------
reporte_modelo(mod_full, "Modelo completo")
reporte_modelo(mod_step, "Modelo reducido (stepAIC)")

## 10) Notas si hay supuestos incumplidos -----------------------------------
# - Heteroscedasticidad: reporta coeftest + vcovHC (HC3) y/o considera transformaciones.
# - No-normalidad: con n moderado/grande, no afecta insesgadez de betas; afecta tests exactos.
# - Colinealidad: si VIF alto, elimina ítems muy correlacionados (p.ej., P3/P4/P5 o P6/P7).
############################################




