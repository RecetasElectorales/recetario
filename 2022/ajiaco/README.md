# Ajiaco 2022 — Dirichlet-multinomial (primera generación)

Primer modelo conjunto para todos los candidatos. Usa una distribución Dirichlet-multinomial que trata las intenciones de voto como proporciones sobre un simplex, garantizando que siempre sumen 1.

## Archivos

- `stan/ajiaco_model.stan` — modelo principal Dirichlet-multinomial
- `stan/ajiaco_model_categorical.stan` — variante con likelihood categórica
- `R/ajiaco_analisis.R` — estimación en tres cortes temporales (marzo, abril, mayo)
- `output/ajiaco_fit.csv` — resultados del modelo

## Por qué Dirichlet-multinomial

Los modelos Simple y Mixto estiman cada candidato por separado: los pronósticos individuales no suman necesariamente 100%, y no capturan que votos que se van a un candidato vienen de otro. El Ajiaco modela el vector completo de proporciones en un solo paso.

## Cómo correrlo

```r
library(rstan)
library(tidyverse)
library(here)
source(here("2022", "ajiaco", "R", "ajiaco_analisis.R"))
```
