# Modelos 2018 — Primera y segunda vuelta presidencial Colombia

Elección del 27 de mayo (primera vuelta) y 17 de junio de 2018. Candidatos principales: Iván Duque, Gustavo Petro, Sergio Fajardo, Germán Vargas Lleras, Humberto de la Calle.

## Modelos

### `simple/` — Regresión normal independiente por candidato
Un modelo Stan por candidato. Estima intención de voto como función del tiempo, con efectos aleatorios por encuestadora. Usa `rstan`.

### `mixto/` — Efectos mixtos con correlación LKJ
Extiende el Simple modelando la matriz de correlación entre los coeficientes de regresión de cada encuestadora. Primer uso de prior LKJ en el proyecto. Usa `rstan`.

## Resultados primera vuelta

| Candidato | Resultado real |
|---|---|
| Iván Duque | 39.1% |
| Gustavo Petro | 25.1% |
| Sergio Fajardo | 23.7% |
| Germán Vargas Lleras | 7.3% |
| Humberto de la Calle | 2.1% |
