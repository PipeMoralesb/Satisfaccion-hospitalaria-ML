# Satisfaccion-hospitalaria-ML
Analizamos la satisfacción hospitalaria para explicar y predecir la Valoración General (VG, 0–10) a partir de variables sociodemográficas y de experiencia.

# Satisfacción Hospitalaria — Modelado de VG 
**Autor:** Felipe Morales  
**Stack:** R (RStudio), R Markdown / Quarto, Git/GitHub

## Objetivo
Explicar y predecir la **Valoración General (VG, 0–10)** a partir de S, E, DI, C, P1..P7, P8 (no se usa D en el modelo lineal base). Entregar un modelo interpretable, diagnóstico de supuestos y recomendaciones de gestión.

## Estructura



## Reproducibilidad (R)
```r
pkgs <- c("readxl","dplyr","janitor","psych","corrplot","ggplot2",
          "broom","car","lmtest","sandwich","MASS")
miss <- pkgs[!pkgs %in% rownames(installed.packages())]
if(length(miss)) install.packages(miss, dependencies = TRUE)
invisible(lapply(pkgs, require, character.only = TRUE))

ruta_excel <- "data/raw/satisfaccion_hospital.xlsx"  # si no hay Excel, ver data/raw/README.md


---

## 2) Crea el script **R/Trabajo_3_SatisfaccionHospital.R**

1. **Add file → Create new file**  
2. Nombre: `R/Trabajo_3_SatisfaccionHospital.R`  
3. Pega el script final (el que ya construimos contigo). Si lo quieres brevísimo, pega este esqueleto funcional:

```r
# --- Paquetes ---
pkgs <- c("readxl","dplyr","janitor","psych","corrplot","ggplot2",
          "broom","car","lmtest","sandwich","MASS")
miss <- pkgs[!pkgs %in% rownames(installed.packages())]
if(length(miss)) install.packages(miss, dependencies=TRUE)
invisible(lapply(pkgs, require, character.only = TRUE))

# --- Datos ---
ruta_excel <- "data/raw/satisfaccion_hospital.xlsx"
if(!file.exists(ruta_excel)) stop("Coloca el Excel en data/raw/ o ajusta la ruta.")

datos <- readxl::read_excel(ruta_excel) |> janitor::clean_names()

# --- Renombrado flexible a s,e,di,c,d,p1..p8,vg ---
find_col <- function(cands, cols){ cands <- tolower(cands); hit <- intersect(cands, cols); if(length(hit)==0) NA_character_ else hit[1] }
cols <- names(datos)
old_s<-find_col(c("s","sexo"),cols); old_e<-find_col(c("e","edad"),cols)
old_di<-find_col(c("di","dias_ingreso","dias","dias_estadia","dias_estadía","dias_de_ingreso"),cols)
old_c<-find_col(c("c","cirugia","cirugía"),cols); old_d<-find_col(c("d","servicio"),cols)
old_vg<-find_col(c("vg","valoracion_general","valoración_general","satisfaccion_global","satisfacción_global"),cols)
old_p1<-find_col(c("p1","item1"),cols); old_p2<-find_col(c("p2","item2"),cols); old_p3<-find_col(c("p3","item3"),cols)
old_p4<-find_col(c("p4","item4"),cols); old_p5<-find_col(c("p5","item5"),cols); old_p6<-find_col(c("p6","item6"),cols)
old_p7<-find_col(c("p7","item7"),cols); old_p8<-find_col(c("p8","solucion","solución","solucion_del_problema","solución_del_problema"),cols)

datos <- datos |>
  dplyr::rename(s=all_of(old_s), e=all_of(old_e), di=all_of(old_di), c=all_of(old_c), d=all_of(old_d),
                p1=all_of(old_p1), p2=all_of(old_p2), p3=all_of(old_p3), p4=all_of(old_p4),
                p5=all_of(old_p5), p6=all_of(old_p6), p7=all_of(old_p7), p8=all_of(old_p8), vg=all_of(old_vg)) |>
  dplyr::mutate(
    s=as.integer(s), e=as.numeric(e), di=as.numeric(di), c=as.integer(c),
    p1=as.numeric(p1), p2=as.numeric(p2), p3=as.numeric(p3), p4=as.numeric(p4),
    p5=as.numeric(p5), p6=as.numeric(p6), p7=as.numeric(p7), p8=as.integer(p8),
    vg=as.numeric(vg), d=as.factor(d)
  )

# --- Correlaciones + corrplot ---
num_vars <- c("s","e","di","c","p1","p2","p3","p4","p5","p6","p7","p8","vg")
mat_num  <- datos |> dplyr::select(dplyr::all_of(num_vars)) |> as.data.frame()
m_cor    <- cor(mat_num, use="pairwise.complete.obs", method="pearson")
corrplot::corrplot(m_cor, method="color", addCoef.col="black", tl.col="black", tl.srt=45, number.cex=.7)

# --- Modelo completo ---
mod_full <- lm(vg ~ s + e + di + c + p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8, data=datos)
print(summary(mod_full))
print(broom::glance(mod_full)[,c("r.squared","adj.r.squared","statistic","p.value")])
print(tryCatch(car::vif(mod_full), error=function(e) e$message))
print(confint(mod_full, level=.95))

# --- Diagnósticos ---
print(car::durbinWatsonTest(mod_full))
print(shapiro.test(residuals(mod_full)))
print(lmtest::bptest(mod_full))
print(lmtest::coeftest(mod_full, vcov.=sandwich::vcovHC(mod_full, type="HC3")))

# --- StepAIC (reducido) ---
mod_step <- MASS::stepAIC(mod_full, direction="both", trace=FALSE)
print(summary(mod_step))
print(broom::glance(mod_step)[,c("r.squared","adj.r.squared","statistic","p.value")])
print(tryCatch(car::vif(mod_step), error=function(e) e$message))


