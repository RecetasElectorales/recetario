# Simple 2018 — Regresión normal por candidato

Modelo bayesiano independiente para cada candidato. Estima la intención de voto como distribución normal con efectos de encuestadora.

## Archivos

- `stan/simple_{candidato}.stan` — un modelo por candidato (petro, duque, fajardo, delacalle, vargaslleras)
- `R/simple_todos.R` — compila y estima todos los modelos, guarda draws
- `output/simple_2018_resultados.csv` — draws posteriores de todos los candidatos

## Cómo correrlo

```r
library(rstan)
library(tidyverse)
library(here)
source(here("2018", "simple", "R", "simple_todos.R"))
```

## Supuestos clave

- Distribución normal para intención de voto (no respeta suma=100%)
- Efectos aleatorios por encuestadora (intercepto)
- Prior débil sobre la tendencia temporal
