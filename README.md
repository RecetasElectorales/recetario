# Recetario Electoral — Modelos estadísticos 2018–2026

Repositorio de modelos estadísticos de **Recetas Electorales** para las elecciones presidenciales colombianas de 2018, 2022 y 2026. Cada modelo es reproducible: incluye el código Stan, el script R para estimarlo y los resultados generados.

Sitio web: [recetas-electorales.com](https://www.recetas-electorales.com)

---

## Evolución de los modelos

| Generación | Modelo | Tipo | Novedad |
|---|---|---|---|
| 2018 | `simple` | Regresión normal por candidato | Modelo base: efectos de encuestadora |
| 2018 | `mixto` | Efectos mixtos con correlación LKJ | Correlación entre parámetros por encuestadora |
| 2022 | `calentao` | Simple/Mixto adaptado a 2022 | Incluye encuestas digitales (AtlasIntel) |
| 2022 | `ajiaco` | Dirichlet-multinomial | Todos los candidatos simultáneamente; respeta suma=100% |
| 2026 | `ajiaco2` | Dirichlet-multinomial sobre conteos | Modela conteos directos sin transformar proporciones |
| 2026 | `cazuela` | Regresión bayesiana 2026 | Actualización con más encuestas |
| 2026 | `gaussiano` | Proceso gaussiano multivariado | Tendencias simultáneas con prior LKJ sobre correlaciones |

---

## Estructura

```
recetario/
├── data/               # Encuestas por año (input compartido)
│   ├── 2018/
│   ├── 2022/
│   └── 2026/
├── 2018/
│   ├── simple/
│   └── mixto/
├── 2022/
│   ├── calentao/
│   └── ajiaco/
└── 2026/
    ├── ajiaco2/
    ├── cazuela/
    └── gaussiano/
```

Cada carpeta de modelo contiene:
- `stan/` — archivo(s) `.stan`
- `R/` — script(s) para compilar, estimar y extraer resultados
- `output/` — resultados en `.csv` o `.parquet`

---

## Datos

Las encuestas de intención de voto están en `data/{año}/`. Son el input de todos los modelos de ese año. Fuentes: Invamer, Datexco, CNC, AtlasIntel, entre otras.

---

## Dependencias R

```r
# 2018 (rstan)
install.packages(c("rstan", "tidyverse", "here"))

# 2022 (rstan)
install.packages(c("rstan", "tidyverse", "here", "arrow"))

# 2026 (cmdstanr)
install.packages(c("cmdstanr", "tidyverse", "here", "arrow"))
cmdstanr::install_cmdstan()
```

---

## Licencia

[CC BY 4.0](https://creativecommons.org/licenses/by/4.0/) — Recetas Electorales
