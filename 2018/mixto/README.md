# Mixto 2018 — Efectos mixtos con correlación LKJ

Extensión del modelo Simple que modela la matriz de correlación entre coeficientes de regresión de cada encuestadora usando un prior LKJ.

## Archivos

- `stan/mixto_{candidato}.stan` — un modelo por candidato
- `R/mixto_todos.R` — estimación completa
- `R/mixtos_ulam_brms.R` — implementación alternativa con `rethinking`/`brms`
- `output/mixto_2018_resultados.csv` — draws posteriores

## Cómo correrlo

```r
library(rstan)
library(tidyverse)
library(here)
source(here("2018", "mixto", "R", "mixto_todos.R"))
```

## Diferencia con Simple

El modelo Simple asume efectos de encuestadora independientes. El Mixto modela la correlación entre el intercepto y las pendientes de cada encuestadora mediante una distribución multivariada con prior LKJ(2) sobre la matriz de correlación.
