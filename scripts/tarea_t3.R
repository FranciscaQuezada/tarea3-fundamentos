
# Autor: Francisca Quezada
# Fecha: 2026-10-13
# Que hace:
# Regresión económica: CASEN y ENE

library(dplyr)
library(broom)
library(modelsummary)

## Parte A — CASEN (45 puntos)
### A1. Dos modelos
casen <- read.csv("data/raw/casen_reducido.csv") |>
  mutate(experiencia = pmax(edad - educ - 6, 0))

m1 <- lm(ingreso ~ educ, data = casen)
m2 <- lm(ingreso ~ educ + experiencia + genero, data = casen)

### A2. La tabla de coeficientes, con `broom` + `dplyr`
tabla_m2 <- tidy(m2, conf.int = TRUE) |>
  filter(term != "(Intercept)") |>                                        #Sin intercepto
  arrange(p.value) |>                                                     #Ordenar antes de redondear 
  mutate(significativo = p.value < 0.05,                                  #logica, con el p-value exacto    
        across(c(estimate, conf.low, conf.high), ~ round(.x)),            
        p.value = round(p.value,3)) |>                                    #round(p.value,3)
  select(term, estimate, conf.low, conf.high, p.value, significativo)

tabla_m2

### A3. Comparar modelos
#Agrega **un tercer modelo de tu elección** sobre CASEN.
#Tercer modelo: interacción educ * genero (retornos distintos por género)

m3 <- lm(ingreso ~ educ * genero, data = casen)
modelos <- list(simple=m1, controles=m2, mio=m3)

lapply(modelos, glance) |> bind_rows(.id = "modelo")

# Comentario: El R^2 sube de 0.458 (del simple) a 0.475 (controles) y 0.478 (mio). 
# Siempre sube o se mantiene al agregar variables, asi que por si solo no prueba
# que el modelo sea mejor. El R^2 ajustado, que penaliza los parámetro extra, es
# de 0.448, 0.444 y 0.448, el cual bajo al agregar los controles y en mi modelo
# volvio al nivel del simple.
# Mi modelo agrega la interacción educ-genero.

### A4. ¿Cambió el coeficiente de educación?
bind_rows(simple = tidy(m1), controles = tidy(m2), mio = tidy(m3), .id = "modelo") |>
  filter(term == "educ") |>
  select(modelo, estimate, std.error, p.value)

#Para complementar, ¿que tan relacionado esta educ con los controles?
casen |>
  select(ingreso, educ, experiencia) |>
  cor(use = "complete.obs") |>
  round(3)

# Comentario: 
# El coeficiente de educ es de 42.080 en m1 (simple), 42.246 en m2 (controles) y 
# 46.862 en m3 (mio). Casi no cambia de m1 a m2 porque la experiencia casi no se 
# asocia con el ingreso (r=-0.088).
# Para mover un coeficiente, un control debe relacionarse con educ y con el ingreso.
# En m3 no es comprable por la interacción, es la pendiente solo del grupo base (F).

### A5. Diagnóstico
# 1. Con `augment()` + `arrange()`, las tres personas peor predichas. ¿Qué tienen
# en común?
augment(m2) |>
  arrange(desc(abs(.resid))) |>
  select(.rownames, ingreso, educ, genero, .resid)
# Las tres primeras filas son las peor predichas (con residuos de -298.210, -296.393
# y -249.293)

# 2. Con `augment()` + `summarise()`, la máxima distancia de Cook (`.cooksd`) y el
# máximo apalancamiento (`.hat`).
augment(m2) |>
  summarise(max_cooksd = max(.cooksd),
            max_hat    = max(.hat))
# Máxima distancia de 0.0869 y máximo apalancamiento de 0.175

# 3. Re-estima `m2` **sin** la observación de mayor distancia de Cook y reporta
# cuánto cambia el coeficiente de educación.
augment(m2) |>
  slice_max(.cooksd, n = 3) |>
  select(.rownames, ingreso, educ, genero, .cooksd)

m2_sin <- casen |>
  filter(row_number() != 43) |>
  lm(ingreso ~ educ + experiencia + genero, data = _)

bind_rows(todos = tidy(m2), sin_43 = tidy(m2_sin), .id = "muestra") |> 
  filter(term == "educ") |>
  select(muestra, estimate, std.error, p.value)

# Comentario: Los tres peor predichos (filas 47, 51 y 40 ) tienen en comun que 
# son hombres con residuo negativo (-298.210, -296.393 y -249.293), el modelo les predice mas ingreso del que ganan.
# La mayor distancia de Cook es de 0.0869 y el mayor apalancamiento 0.175. Sin
# la fila 43, el coeficiente de educ pasa de 42.246 a 45.218, teniendo un cambio
# de 3.000 pesos aprox.

### A6. Interpretación de la Parte A
tabla_m2 |>
  filter(term %in% c("educ","generoM"))
# En el modelo con controles, cada año adicional de educación se asocia con
# $42.246 más de ingreso mensual, en promedio, manteniendo experiencia y
# género constantes (IC 95 %: 29.487 a 55.004)
# El coeficiente de `generoM` es 55.686 Esto significa que, a igual educacion y
# experiencia, los hombres ganan en promedio $55.686 mas que las mujeres (el 
# grupo base), pero la diferencia no es estadisticamente distinguible de cero
# (p = 0.204).
# Significancia e importancia: generoM es grande (cerca de 8% del ingreso medio
# de 655.291) pero no significativo, con n=55 el error estandar es alto, asi que
# no se puede afirmar que la brecha exista ni que no exista.


## Parte B — ENE: tu pregunta (45 puntos)

### B1. Declara tu pregunta
# ¿Cuantas horas habituales mas o menos trabajan por semana los ocupados por cuenta
# propia que los asalariados del sector privado, a igual edad, sexo y region, y 
# cambia esa diferencia entre regiones?
# Dependiente: horas (habituales limpias, por semana). Interes:categoria (base: 
# Asalariado privado), termino "categoriaCuenta propia". Controles: edad, edad^2, 
# mujer y region. Unidad: horas.

### B2. Prepara la base
# Aqui volvi al crud (no a ene:a3.csv) porque B5 necesita habituales con los centinelas.
# Repeti la limpieza de la A3: 888y 999 -> NA.
ene <- read.csv2("data/raw/ene-2026-06-mjj.csv")

regiones <- c("Tarapacá", "Antofagasta", "Atacama", "Coquimbo", "Valparaíso", 
              "O'Higgins", "Maule", "Biobío", "La Araucanía", "Los Lagos", "Aysén",
              "Magallanes", "Metropolitana", "Los Ríos", "Arica y Parinacota", "Ñuble")

ocupados <- ene |>
  filter(activ == 1) |>
  mutate(mujer     = sexo == 2,
         categoria =case_when(
           categoria_ocupacion == 1     ~ "Empleador",
           categoria_ocupacion == 2     ~ "Cuenta propia",
           categoria_ocupacion == 3     ~ "Asalariado privado",
           categoria_ocupacion == 4     ~ "Asalariado publico",
           categoria_ocupacion %in% 5:6 ~ "Servicio domestico",
           categoria_ocupacion == 7     ~ "Familiar no remunerado"),
         region_txt = regiones[region]) |>
  select(mujer, edad, categoria, region_txt, habituales, fact_cal)

# Centinelas de habituales: 888 y 999 -> NA
ocupados <- ocupados |> 
  mutate(horas = na_if(na_if(habituales, 888),999))

# Auditoría
nrow(ocupados)
table(ocupados$categoria, useNA = "ifany")
table(ocupados$region_txt, useNA = "ifany")
ocupados |> summarise(across(everything(), ~ sum(is.na(.x))))
ocupados |> summarise(n_888 = sum(habituales == 888), n_999 = sum(habituales == 999),
                      min_edad = min(edad), max_edad = max(edad),
                      max_horas = max(horas, na.rm = TRUE))

# Comentario: 103 con 888 y 8 con 999 (111 en total) pasan a NA con na_if(), por eso
# horas tiene 111 NA (centinelas, no estructurales) y lm() los descarta (n = 41.739).
# Categoría, región y edad no tienen NA (filtré activ == 1). Edad: 15 a 99 años, horas
# limpias: maximo de 126 horas.

### B3. Al menos dos modelos
eh1 <- lm(horas ~ categoria, data = ocupados, weights = fact_cal)
eh2 <- lm(horas ~ categoria + edad + I(edad^2) + mujer + region_txt,
          data = ocupados, weights = fact_cal)

modelos_ene <- list(base = eh1, controles = eh2)

### B4. Extrae y compara con `broom` + `dplyr`
# 1. Una tabla con el coeficiente de tu variable de interés en ambos modelos
# (`bind_rows()` de los `tidy()`), en la unidad que corresponde (horas, pp).
bind_rows(base = tidy(eh1, conf.int = TRUE),
          controles = tidy(eh2, conf.int = TRUE), .id = "modelo") |>
  filter(term == "categoriaCuenta propia") |>
  mutate(across(c(estimate, conf.low, conf.high), ~ round(.x, 2))) |>
  select(modelo, estimate, conf.low, conf.high)

# 2. `glance()` de ambos modelos apilados.
lapply(modelos_ene, glance) |>
  bind_rows(.id = "modelo") |>
  select(modelo, r.squared, adj.r.squared, nobs)

# 3. **Uno** de estos dos:
# - una regresión por grupo con `group_by()` + `group_modify()` (por región,
#   sexo, tramo…), con intervalo de confianza; o
# - una interacción (`*`) que ponga a prueba si tu relación difiere entre grupos.
dif_region <- ocupados |>
  group_by(region_txt) |>
  group_modify(~ tidy(lm(horas ~ categoria + edad + I(edad^2) + mujer,
                         data = .x, weights = fact_cal),
                      conf.int = TRUE)) |>
  filter(term == "categoriaCuenta propia") |>
  ungroup() |>
  mutate(across(c(estimate, conf.low, conf.high), ~ round(.x, 1)),
         distinta_de_cero = conf.high < 0 | conf.low > 0 ) |>
  select(region_txt, estimate, conf.low, conf.high, distinta_de_cero) |>
  arrange(desc(estimate))

dif_region
dif_region |> count(distinta_de_cero)

### B5. Sensibilidad
# Muestra **una** decisión que mueva tu resultado y cuánto lo mueve.
# Sensibilidad: con y sin limpiar los centinelas
eh2_sucia <- lm(habituales ~ categoria + edad + I(edad^2) + mujer + region_txt,
                data = ocupados, weights = fact_cal)

bind_rows(sucia = tidy(eh2_sucia), limpia = tidy(eh2), .id = "base") |>
  filter(term == "categoriaCuenta propia") |>
  select(base, term, estimate, std.error)

# Comentario: sin limpiar, la diferencia es de -8.74 horas; limpia, de -8.18.
# Los centinelas la mueven 0.56 horas y el error estándar baja de 0.579 a 0.152
# al limpiar (casi cuatro veces menor).


### B6. Interpretación de la Parte B (esto es lo que más pesa)
ocupados |> summarise(promedio_horas = weighted.mean(horas, fact_cal, na.rm = TRUE)) #media de horas
# Los ocupados por cuenta propia se asocian con trabajar 8.5 horas menos por semana que
# los asalariados privados (IC 95%: -8.8 a -8.2), a igual edad, sexo y región, 8.2 horas
# menos (IC 95%: -8.5 a -7.9). El modelo base compara los grupos tal cual y el de controles
# compara personas parecidas, como la cifra casi no cambia, edad, sexo y región se asocian poco.
# Con 41.739 observaciones el p-value es mínimo, así que importa el tamaño, con 8.2 horas son
# cerca de una quinta parte de las 38.7 horas promedio, una diferencia grande, aunque el
# R^2 es 0.14 y el modelo explica poco de las horas. Por región va de -5.7 (Magallanes) a
# -12.5 (La Araucanía) y aparece en las 16.
# Limitaciones: 1) los errores estándar de lm() con weights no consideran el diseño muestral
# de la ENE; 2) no mide por qué se llega a ser cuenta propia y las horas habituales no son efectivas.


## Parte C — Comunicar (10 puntos)

# 1. Una tabla con `modelsummary()` que muestre los modelos de la Parte B lado a lado
# (estrellas, `nobs`, R²).
modelsummary(modelos_ene,
             stars = TRUE,
             gof_map = c("nobs", "r.squared"))
# 2. Exporta a CSV:
  
dir.create("data/processed", showWarnings = FALSE)

modelsummary(modelos_ene, output = "data/processed/t3_tabla_ene.csv")

tidy(m2, conf.int = TRUE) |>
  write.csv("data/processed/t3_coeficientes_casen.csv", row.names = FALSE)

### Declaración uso de IA
# La use para entender errores de código y la sintaxis de los comandos del lab, 
# para obtener cifras de referencia y para revisar redacción; la pregunta, los 
# controles y las decisiones fueron tomadas por mi.
