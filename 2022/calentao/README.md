# Calentao 2022 — Modelos 2018 recalentados

Adaptación de los modelos Simple y Mixto de 2018 para las elecciones de 2022. El nombre alude a recalentar un plato del día anterior.

## Archivos

- `stan/fg_simple.stan` — modelo simple para Federico Gutiérrez (ejemplo representativo)
- `R/calentao_todos.R` — estimación completa para todos los candidatos 2022
- `output/calentao-2022_resultados.csv` — draws posteriores

## Novedad respecto a 2018

Incorporación de encuestas digitales (AtlasIntel) con una variable indicadora de modo de recolección. Las encuestas telefónicas y presenciales mostraron sesgos sistemáticos distintos respecto a las digitales en 2022.

## Cómo correrlo

```r
library(rstan)
library(tidyverse)
library(here)
source(here("2022", "calentao", "R", "calentao_todos.R"))
```
