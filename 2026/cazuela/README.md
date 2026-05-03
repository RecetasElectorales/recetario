# Cazuela 2026 — Regresión bayesiana actualizada

Actualización del modelo de regresión con el conjunto completo de encuestas disponibles a abril de 2026. Complementa al Ajiaco2 con un enfoque de regresión más directo.

## Archivos

- `stan/cazuela.stan` — modelo de regresión bayesiana
- `output/cazuela_draws.parquet` — draws posteriores

## Cómo correrlo

```r
library(cmdstanr)
library(arrow)
library(here)

mod <- cmdstan_model(here("2026", "cazuela", "stan", "cazuela.stan"))
# Ver ajiaco_analisis.R en ajiaco2/ para el patrón de estimación
```
