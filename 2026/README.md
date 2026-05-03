# Modelos 2026 — Primera vuelta presidencial Colombia

Elección del 25 de mayo de 2026. Todos los modelos usan `cmdstanr` (no `rstan`).

Candidatos principales medidos en encuestas: Claudia López, Juan Daniel Oviedo, Vicky Dávila, Juan Manuel Galán, Paloma Valencia, Sergio Fajardo, Juan Carlos Pinzón, Abelardo de la Espriella, Iván Cepeda, voto en blanco.

## Modelos

### `ajiaco2/` — Dirichlet-multinomial sobre conteos directos
Evolución del Ajiaco 2022. Modela los conteos crudos de encuesta (no proporciones transformadas), lo que mejora la propagación de incertidumbre muestral.

### `cazuela/` — Regresión bayesiana 2026
Modelo de regresión actualizado con el conjunto de encuestas disponibles a abril de 2026.

### `gaussiano/` — Proceso gaussiano multivariado
Modela tendencias simultáneas de todos los candidatos con un proceso gaussiano. Usa prior LKJ sobre la matriz de correlación entre candidatos, capturando que los movimientos de un candidato están correlacionados con los de otros.

## Cambio de motor: rstan → cmdstanr

A partir de 2026 todos los modelos se compilan y estiman con `cmdstanr`. La sintaxis Stan es idéntica; cambia la interfaz R.
