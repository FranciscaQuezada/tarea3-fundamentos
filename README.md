
## Tarea 3 Fundamentos

## Pregunta
¿Cuantas horas habituales mas o menos trabajan por semana los ocupados por cuenta
propia que los asalariados del sector privado, a igual edad, sexo y region, y 
cambia esa diferencia entre regiones?
Dependiente: horas habituales por semana. Interes:categoria (base: Asalariado
privado), termino "categoriaCuenta propia". Unidad: horas.

## Respuesta
Los ocupados por cuenta propia se asocian con trabajar unas 8.2 horas menos por
semana que los asalariados privados, a igual edad, sexo y region (IC 95%: -8.5 a 
-7.9). La diferencia aparece en las 16 regiones, de -5.7 horas en Magallanes a 
-12.5 en La Araucanía. Los errores estandar no consideran el diseño muestral de 
la ENE, asi que los intervalos son orientativos.

## Como correr el script
1. Abrir el proyecto con el archivo `R.proj`(no el script suelto).
2. Verificar que existan `data/raw/casen_reducido.csv`y `data/raw/ene-2026-06-mjj.csv`.
3. Si faltan paquetes: ìnstall.packages(c("dplyr", "broom", "modelsummary"))`.
4. Correr `scripts/tarea_t3.R`completo. Genera en `data/processed/`: `t3_tabla_ene.csv`y
`t3_coeficientes_casen.csv`. 