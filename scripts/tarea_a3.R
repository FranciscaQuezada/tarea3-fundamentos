# Autor: Francisca Quezada
# Fecha: 2026-10-23
# Que hace: Desarrollo de tarea 3 con objetivo de estimar, extraer, comparar, diagnosticar e interpretar modelos de regresión en R.

library(dplyr)
library(broom)
library(modelsummary)

## Parte A — CASEN

### A1. Dos modelos
casen <- read.csv("data/raw/casen_reducido.csv") |>
  mutate(experiencia = pmax(edad - educ - 6, 0))

m1 <- lm(ingreso ~ educ, data = casen)
m2 <- lm(ingreso ~ educ + experiencia + genero, data = casen)

### A2. La tabla de coeficientes, con broom + dplyr
tidy(m2, conf.int = TRUE) |>
  filter(term != "(Intercept)") |>                       
  mutate(across(c(estimate, conf.low, conf.high), round),
         significativo = p.value < 0.05) |>           
  select(term, estimate, conf.low, conf.high, p.value, significativo) |>
  arrange(p.value) 

# Se ven 3 filas; educ arriba con significativo = TRUE, generoM y experiencia con FALSE.

### A3. Comparar modelos
m3 <- lm(ingreso ~ educ * genero + experiencia, data = casen)

modelos <- list(
  simple    = m1,
  controles = m2,
  mio    = m3
)

lapply(modelos, glance) |> bind_rows(.id = "modelo")

# ¿Qué pasa con el R² y con el R² *ajustado*? ¿Tu modelo mejora algo?
# Con el R² se ve un aumento de 0.458 en el modelo simple a 0.480 en modelo Mio, 
# Con el R² ajustado se ve una disminución 0.448 a 0.438 por lo que agregar más variables no mejora tanto el modelo como se esperaba.

### A4. ¿Cambió el coeficiente de educación?
bind_rows(simple = tidy(m1), controles = tidy(m2), mio = tidy(m3), .id = "modelo") |>
  filter(term == "educ") |>
  select(modelo, estimate, std.error, p.value)

# Se ve 42080 en simple, 42246 en controles y 46646 en mio.
# El coeficiente de educación aumentó de 42.080 en el modelo simple a 46.646 en el modelo mio. 
# Al agregar más variables aumenta el efecto estimado de la educación sobre el ingreso.
# Se ve en la variable simple a controles el coeficiente casi no cambia (42080 a 42246) menos del 0.4%, esto porque controles no muestra una relación significativa con el ingreso.

### A5. Diagnóstico
# 1. Con augment() + arrange()
augment(m2) |>                                      
  select(ingreso, educ, genero, .fitted, .resid) |>
  mutate(across(c(.fitted, .resid), round)) |>
  arrange(desc(abs(.resid))) |>
  head(3)

# Las tres personas peor predichas ¿Qué tienenen común?
# Las tres tienen su residuo negativo y en genero contienen M.

# 2. Con augment() + summarise(), la máxima distancia de Cook (.cooksd) y el máximo apalancamiento (.hat).
augment(m2) |>
  summarise(max_cooks = max(.cooksd),
            max_hat   = max(.hat)
  )

# max_cooks (0.0869), max_hat (0.175)

#3. Re-estima m2 *sin* la observación de mayor distancia de Cook y reporta cuánto cambia el coeficiente de educación.
m2_sin <- casen |>
  filter(row_number() != 43) |>
  lm(ingreso ~ educ + experiencia + genero, data = _)

m2_sin  

# El coeficiente de educación cambia de 42245.8 a 45217.9, sube 7 %, sigue significativo).

### A6. Interpretación de la Parte A

# En el modelo con controles, cada año adicional de educación se asocia con
# $42.246 más de ingreso mensual, en promedio, manteniendo experiencia y 
# género constantes (IC 95 %: $29.487 a $55.004).
# El coeficiente de generoM es $55.686. Esto significa que los hombres tienen
# un ingreso mensual estimado $55.686 mayor que las mujeres manteniendo constantes la educación y la experiencia.

# ¿Qué coeficiente es grande pero no significativo? ¿Qué significa eso con n = 55?
# El coeficiente de género es relativamente grande pero no es estadísticamente significativo según los resultados anteriores,
# con una muestra de 55 observaciones existe bastante incertidumbre y puede ser difícil detectar diferencias reales.


## Parte B — ENE: tu pregunta
### B1. Declara tu pregunta    
#  Entre las personas ocupadas, ¿Cuántas horas habituales a la semana trabajan menoslas mujeres que los hombres, 
#  con igual nivel educacional, cambia esa brecha según el tramo de edad?

### B2. Prepara la base
# ene_a3.csv no conservó habituales, cine11_1d ni tramo_edad, así que con data/raw/ene-2026-06-mjj.csv se volvió a realizar 
# y se repetir la limpieza (la ene_raw <- read.csv2("data/raw/ene-2026-06-mjj.csv")

class(ene_raw$fact_cal)
table(ene_raw$activ, useNA = "ifany")

ocupados <- ene_raw |> filter(activ == 1)
nrow(ocupados)
table(ocupados$sexo, useNA = "ifany")
table(ocupados$cine11_1d, useNA = "ifany")
table(ocupados$tramo_edad, useNA = "ifany")
summary(ocupados$habituales)
table(ocupados$habituales %in% c(888, 999))
sum(is.na(ocupados$habituales))

ene_b <- ocupados |>
  select(sexo, habituales, cine11_1d, tramo_edad, fact_cal) |>
  mutate(habituales = ifelse(habituales %in% c(888, 999), NA, habituales),
         cine11_1d  = ifelse(cine11_1d == 9, NA, cine11_1d),
         mujer      = sexo == 2,
         cine11_1d  = factor(cine11_1d),
         tramo_edad = factor(tramo_edad)) |>
  na.omit()

nrow(ocupados) - nrow(ene_b)
table(ene_b$cine11_1d)

# Se ve 41.850 ocupados.| 111 centinelas (888 o 999) en habituales.| 58 casos con el 9 (Sin clasificación) en cine11_1d.| 169 filas perdidas en total.| n final de 41.681.

### B3. Al menos dos modelos
m_base <- lm(habituales ~ mujer, data = ene_b, weights = fact_cal)
m_ctrl <- lm(habituales ~ mujer + cine11_1d + tramo_edad,
             data = ene_b, weights = fact_cal)

modelos_ene <- list(base = m_base, controles = m_ctrl)


### B4. Extrae y compara con broom + dplyr
# 1. Coeficiente de mujer en ambos modelos
bind_rows(base = tidy(m_base, conf.int = TRUE),
          controles = tidy(m_ctrl, conf.int = TRUE), .id = "modelo") |>
  filter(term == "mujerTRUE") |>
  select(modelo, estimate, conf.low, conf.high, p.value)

# 2. glance de ambos
lapply(modelos_ene, glance) |>
  bind_rows(.id = "modelo") |>
  select(modelo, r.squared, adj.r.squared, nobs)

# 3. Brecha por tramo de edad
ene_b |>
  group_by(tramo_edad) |>
  group_modify(~ tidy(lm(habituales ~ mujer + cine11_1d,
                         data = .x, weights = fact_cal),
                      conf.int = TRUE)) |>
  filter(term == "mujerTRUE") |>
  select(tramo_edad, estimate, conf.low, conf.high, p.value)
# La brecha de horas habituales de las mujeres es de -4,11 horas semanales en el modelo base y de -4,48 al controlar por nivel educacional y tramo de
# edad (IC 95 %: -4,73 a -4,24), por tramo de edad va de -3,2 horas (25 a 29 años) a -6,5 horas (60 a 64 años).

### B5. Sensibilidad
m_ctrl_sin <- lm(habituales ~ mujer + cine11_1d + tramo_edad, data = ene_b)

bind_rows(ponderado = tidy(m_ctrl, conf.int = TRUE),
          sin_ponderar = tidy(m_ctrl_sin, conf.int = TRUE), .id = "version") |>
  filter(term == "mujerTRUE") |>
  select(version, estimate, conf.low, conf.high)

# Sin ponderar, la brecha es de -4.93 horas semanales y ponderada: -4,48, sería una diferencia de 0.45 horas. 
# Ponderar con fact_cal hace que el coeficiente describa a la población ocupada y sin ponderar describe solo a las personas de la muestra, sin ponderar la brecha es un 10 % mayor pero la conclusión no cambia.

### B6. Interpretación de la Parte B 
# Ser mujer se asocia con trabajar 4,11 horas semanales menos que los hombres ocupados (IC 95 %: -4,36 a -3,86);
# el modelo base compara promedios, al comparar personas del mismo nivel educacional y tramo de edad la brecha estimada es de -4,48 horas 
# (IC 95 %: -4,73 a -4,24), unas 0,4 horas mayor. Por tramo de edad varía entre -3,2 horas (25 a 29 años) y -6,5 (60 a 64 años).
# Con 41.681 observaciones casi todo es significativo pero lo que importa es el tamaño 4 a 4,5 horas son
# aprox un 10 % de la jornada masculina (40,5 horas) aunque el R2 es 0,058 y estas variables explican poco.
# En cuanto a las limitaciones Primero los errores estándar de lm() con weights no consideran el diseño muestral de la ENE
# y segundo, faltan variables como categoría ocupacional o sector.

## Parte C — Comunicar 

# 1. Una tabla con modelsummary() que muestre los modelos de la Parte B lado a lado (estrellas, nobs, R²).
modelsummary(modelos_ene, stars = TRUE,
             gof_omit = "AIC|BIC|Log.Lik.|F|RMSE")

# 2. Exportar a CSV
dir.create("data/processed", showWarnings = FALSE)
modelsummary(modelos_ene, output = "data/processed/t3_tabla_ene.csv")
tidy(m2, conf.int = TRUE) |>
  write.csv("data/processed/t3_coeficientes_casen.csv", row.names = FALSE)