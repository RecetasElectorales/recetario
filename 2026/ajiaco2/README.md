# Ajiaco2 2026 — Dirichlet-multinomial sobre conteos

Segunda generación del modelo Ajiaco. La diferencia clave con el Ajiaco 2022 es que modela directamente los conteos de encuesta (n_i respondentes que prefieren al candidato i) en lugar de las proporciones derivadas, mejorando la propagación de incertidumbre muestral.

## Archivos

- `stan/ajiaco2.stan` — modelo Dirichlet-multinomial sobre conteos
- `R/ajiaco_analisis.R` — compilación con `cmdstanr`, estimación y extracción de draws
- `output/ajiaco2_draws.parquet` — draws posteriores en formato parquet (Arrow)

## Cómo correrlo

```r
library(cmdstanr)
library(tidyverse)
library(arrow)
library(here)
source(here("2026", "ajiaco2", "R", "ajiaco_analisis.R"))
```

## Diferencia con Ajiaco 2022

| | Ajiaco 2022 | Ajiaco2 2026 |
|---|---|---|
| Input | Proporciones (%) | Conteos enteros |
| Motor | `rstan` | `cmdstanr` |
| Output | `.csv` | `.parquet` |
| Likelihood | Dirichlet-multinomial | Dirichlet-multinomial |
