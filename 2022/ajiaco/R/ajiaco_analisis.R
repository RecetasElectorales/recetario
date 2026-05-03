# Packages ####
library(lubridate)
library(gganimate)
library(tidyverse)
library(magick)
library(rstan)
library(bayesplot)
library(ggridges)

# Ajiaco Priors ####
ajiaco_marzo <- readr::read_csv("https://raw.githubusercontent.com/nelsonamayad/Elecciones-presidenciales-2022/main/Encuestas%202022/encuestas_2022.csv") %>% 
  dplyr::select(n,fecha,gustavo_petro,federico_gutierrez,sergio_fajardo,rodolfo_hernandez,ingrid_betancourt) %>%
  dplyr::filter(between(lubridate::as_date(fecha),
                        #Start
                        lubridate::as_date("2022-03-11"),
                        # End
                        lubridate::as_date("2022-03-31"))) %>%
  tidyr::pivot_longer(cols=contains("_"),names_to = "candidato", values_to = "int_voto") %>% 
  dplyr::mutate(nombres = case_when(candidato=="gustavo_petro" ~ "Gustavo Petro",
                                    candidato=="federico_gutierrez" ~ "Federico Gutierrez",
                                    candidato=="sergio_fajardo" ~ "Sergio Fajardo",
                                    candidato=="rodolfo_hernandez" ~ "Rodolfo Hernandez",
                                    candidato=="ingrid_betancourt" ~ "Ingrid Betancourt")) %>%
  dplyr::group_by(nombres) %>% 
  dplyr::summarize(promedio_prior = mean(int_voto),
                   promedio_ponderado_t = weighted.mean(int_voto,n)) 


ajiaco_abril <- readr::read_csv("https://raw.githubusercontent.com/nelsonamayad/Elecciones-presidenciales-2022/main/Encuestas%202022/encuestas_2022.csv") %>% 
  dplyr::select(n,fecha,gustavo_petro,federico_gutierrez,sergio_fajardo,rodolfo_hernandez,ingrid_betancourt) %>%
  dplyr::filter(between(lubridate::as_date(fecha),
                        #Start
                        lubridate::as_date("2022-03-11"),
                        # End
                        lubridate::as_date("2022-04-30"))) %>%
  tidyr::pivot_longer(cols=contains("_"),names_to = "candidato", values_to = "int_voto") %>% 
  dplyr::mutate(nombres = case_when(candidato=="gustavo_petro" ~ "Gustavo Petro",
                                    candidato=="federico_gutierrez" ~ "Federico Gutierrez",
                                    candidato=="sergio_fajardo" ~ "Sergio Fajardo",
                                    candidato=="rodolfo_hernandez" ~ "Rodolfo Hernandez",
                                    candidato=="ingrid_betancourt" ~ "Ingrid Betancourt")) %>%
  dplyr::group_by(nombres) %>% 
  dplyr::summarize(promedio_prior = mean(int_voto),
                   promedio_ponderado_t = weighted.mean(int_voto,n)) 

  
ajiaco_mayo <- readr::read_csv("https://raw.githubusercontent.com/nelsonamayad/Elecciones-presidenciales-2022/main/Encuestas%202022/encuestas_2022.csv") %>% 
  dplyr::select(n,fecha,gustavo_petro,federico_gutierrez,sergio_fajardo,rodolfo_hernandez,ingrid_betancourt) %>%
  dplyr::filter(between(lubridate::as_date(fecha),
                        #Start
                        lubridate::as_date("2022-03-11"),
                        # End
                        lubridate::as_date("2022-05-28"))) %>%
  tidyr::pivot_longer(cols=contains("_"),names_to = "candidato", values_to = "int_voto") %>% 
  dplyr::mutate(nombres = case_when(candidato=="gustavo_petro" ~ "Gustavo Petro",
                                    candidato=="federico_gutierrez" ~ "Federico Gutierrez",
                                    candidato=="sergio_fajardo" ~ "Sergio Fajardo",
                                    candidato=="rodolfo_hernandez" ~ "Rodolfo Hernandez",
                                    candidato=="ingrid_betancourt" ~ "Ingrid Betancourt")) %>%
  dplyr::group_by(nombres) %>% 
  dplyr::summarize(promedio_prior = mean(int_voto),
                   promedio_ponderado_t = weighted.mean(int_voto,n)) 

  
# Ajiaco Data ####
ajiaco_data_marzo <- tibble(
  gp = ajiaco_marzo$promedio_ponderado_t[2],
  fg = ajiaco_marzo$promedio_ponderado_t[1],
  sf = ajiaco_marzo$promedio_ponderado_t[5],
  rh = ajiaco_marzo$promedio_ponderado_t[4],
  ib = ajiaco_marzo$promedio_ponderado_t[3]) %>%
  dplyr::mutate(rest = 100-rowSums(across(where(is.numeric)))) %>%
  dplyr::mutate(across(everything(),~./100))

ajiaco_data_abril <- tibble(
  gp = ajiaco_abril$promedio_ponderado_t[2],
  fg = ajiaco_abril$promedio_ponderado_t[1],
  sf = ajiaco_abril$promedio_ponderado_t[5],
  rh = ajiaco_abril$promedio_ponderado_t[4],
  ib = ajiaco_abril$promedio_ponderado_t[3]) %>%
  dplyr::mutate(rest = 100-rowSums(across(where(is.numeric)))) %>%
  dplyr::mutate(across(everything(),~./100))

ajiaco_data_mayo <- tibble(
  gp = ajiaco_mayo$promedio_ponderado_t[2],
  fg = ajiaco_mayo$promedio_ponderado_t[1],
  sf = ajiaco_mayo$promedio_ponderado_t[5],
  rh = ajiaco_mayo$promedio_ponderado_t[4],
  ib = ajiaco_mayo$promedio_ponderado_t[3]) %>%
  dplyr::mutate(rest = 100-rowSums(across(where(is.numeric)))) %>%
  dplyr::mutate(across(everything(),~./100))

# Simulacion numero de encuestas 
ajiaco_n_encuestas <- 1e2
ajiaco_ensayos <- 1e5

# Simulacion de resultados de encuestas
ajiaco_multinomial_marzo <- rmultinom(ajiaco_ensayos,
                                      ajiaco_n_encuestas,
                                      ajiaco_data_marzo) %>%
  t() %>%
  tibble::as_tibble() %>%
  dplyr::summarise(across(everything(),median))

ajiaco_multinomial_abril <- rmultinom(ajiaco_ensayos,
                                      ajiaco_n_encuestas,
                                      ajiaco_data_abril) %>%
  t() %>%
  tibble::as_tibble() %>%
  dplyr::summarise(across(everything(),median))

ajiaco_multinomial_mayo <- rmultinom(ajiaco_ensayos,
                                      ajiaco_n_encuestas,
                                      ajiaco_data_mayo) %>%
  t() %>%
  tibble::as_tibble() %>%
  dplyr::summarise(across(everything(),median))

ajiaco_data_marzo <- list(
  ajiaco_n_encuestas = ajiaco_n_encuestas,
  ajiaco_priors = c(ajiaco_multinomial_marzo$gp,
                    ajiaco_multinomial_marzo$fg,
                    ajiaco_multinomial_marzo$sf,
                    ajiaco_multinomial_marzo$rh,
                    ajiaco_multinomial_marzo$ib,
                    ajiaco_multinomial_marzo$rest)
)

ajiaco_data_abril <- list(
  ajiaco_n_encuestas = ajiaco_n_encuestas,
  ajiaco_priors = c(ajiaco_multinomial_abril$gp,
                    ajiaco_multinomial_abril$fg,
                    ajiaco_multinomial_abril$sf,
                    ajiaco_multinomial_abril$rh,
                    ajiaco_multinomial_abril$ib,
                    ajiaco_multinomial_abril$rest)
)

ajiaco_data_mayo <- list(
  ajiaco_n_encuestas = ajiaco_n_encuestas,
  ajiaco_priors = c(ajiaco_multinomial_mayo$gp,
                    ajiaco_multinomial_mayo$fg,
                    ajiaco_multinomial_mayo$sf,
                    ajiaco_multinomial_mayo$rh,
                    ajiaco_multinomial_mayo$ib,
                    ajiaco_multinomial_mayo$rest)
)
# Censo electoral y tasa participacion 54% ####
censo <- 39002239
votantes_simulacion <- 0.5*censo

# Modelo ####
{ajiaco_mensual <- "
data {
  int<lower = 1> ajiaco_n_encuestas;
  int<lower = 0,upper = ajiaco_n_encuestas> ajiaco_priors[6];
}
parameters {
  simplex[6] theta;
}
model {
  target += dirichlet_lpdf(theta | rep_vector(2, 6));
  
  target += multinomial_lpmf(ajiaco_priors | theta); 
}
generated quantities{
  int pred[6] = multinomial_rng(theta, 100);
}
"
}
# Estimacion ####
ajiaco_marzo_fit <- stan(model = ajiaco_mensual,
                         data = ajiaco_data_marzo,
                         control=list(adapt_delta=0.95),
                         iter=1e4,
                         chains=4,
                         cores=4,
                         seed=332211)

ajiaco_abril_fit <- stan(model = ajiaco_mensual,
                         data = ajiaco_data_abril,
                         control=list(adapt_delta=0.95),
                         iter=1e4,
                         chains=4,
                         cores=4,
                         seed=332211)

ajiaco_mayo_fit <- stan(model = ajiaco_mensual,
                         data = ajiaco_data_mayo,
                         control=list(adapt_delta=0.95),
                         iter=1e4,
                         chains=4,
                         cores=4,
                         seed=332211)

# Visualizacion ####
ajiaco_mes_plot <- ajiaco_marzo_fit %>% 
  as.matrix() %>%
  tibble::as_tibble() %>% 
  dplyr::select(starts_with("pred[")) %>% 
  tidyr::pivot_longer(cols = starts_with("pred["), names_to = "parameter",values_to = "estimate") %>%
  dplyr::mutate(n=stringr::str_extract(parameter,"[:digit:]") %>% as.numeric()) %>%
  dplyr::left_join(ajiaco_priors %>%
                     tidyr::pivot_longer(cols = everything(), names_to = "candidato",values_to="prior") %>%
                     dplyr::mutate(n=row_number(),
                                   mes = "Marzo"), by="n") %>%
  # Abril
  dplyr::bind_rows(
    ajiaco_abril_fit %>% 
      as.matrix() %>%
      tibble::as_tibble() %>% 
      dplyr::select(starts_with("pred[")) %>% 
      tidyr::pivot_longer(cols = starts_with("pred["), names_to = "parameter",values_to = "estimate") %>%
      dplyr::mutate(n=stringr::str_extract(parameter,"[:digit:]") %>% as.numeric()) %>%
      dplyr::left_join(ajiaco_priors %>%
                         tidyr::pivot_longer(cols = everything(), names_to = "candidato",values_to="prior") %>%
                         dplyr::mutate(n=row_number(),
                                       mes = "Abril"), by="n")
  ) %>%
  # Mayo
  dplyr::bind_rows(
    ajiaco_mayo_fit %>% 
      as.matrix() %>%
      tibble::as_tibble() %>% 
      dplyr::select(starts_with("pred[")) %>% 
      tidyr::pivot_longer(cols = starts_with("pred["), names_to = "parameter",values_to = "estimate") %>%
      dplyr::mutate(n=stringr::str_extract(parameter,"[:digit:]") %>% as.numeric()) %>%
      dplyr::left_join(ajiaco_priors %>%
                         tidyr::pivot_longer(cols = everything(), names_to = "candidato",values_to="prior") %>%
                         dplyr::mutate(n=row_number(),
                                       mes = "Mayo"), by="n")
  ) %>%
  #Join names
  dplyr::left_join(tribble(~candidato,~nombre,
                           "fg","Federico Gutierrez",
                           "gp","Gustavo Petro",
                           "sf","Sergio Fajardo",
                           "rh","Rodolfo Hernandez",
                           "ib","Ingrid Betancourt",
                           #"jm","John Milton Rodriguez",
                           "rest","Resto (Otros, Blanco e Indecisos)"),
                   by="candidato") %>%
  dplyr::mutate(nombre = ifelse(str_detect(nombre,"Resto"),"Resto*",nombre)) %>%
  dplyr::mutate(nombre = factor(nombre,levels = c("Gustavo Petro","Federico Gutierrez","Sergio Fajardo","Rodolfo Hernandez","Ingrid Betancourt","Resto*"))) %>%
  dplyr::mutate(nombre = fct_rev(nombre)) %>%
  # Plot
  ggplot(aes(y=nombre,x=estimate,fill = 0.5-abs(0.5-stat(ecdf)), frame=factor(mes)))+
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Prob", direction = -1)+
  scale_x_continuous(limits = c(0,60),breaks=c(0,10,20,30,40,50,60))+
  labs(x="Dist. posterior de la intención de voto para cada candidato",
       y=NULL,
       title = "Pronósticos del Ajiaco para la primera vuelta 2022",
       subtitle = paste0("Estimación para encuestas disponibles en {closest_state}"),
       caption="*Incluye otros candidatos, voto en blanco e indecisos\nFuente: www.recetas-electorales.com")+
  theme(legend.position = "bottom")+
  theme_ridges()


## Animate ####
ajiaco_animation <- ajiaco_mes_plot +
  #Animate
  transition_states(states = factor(mes),
                  transition_length = 0.5,
                  state_length = 0.5,
                  wrap = TRUE)+
  ease_aes()

ajiaco_animation

## Save ####
anim_save("ajiaco_animation.gif", animation = last_animation())


# Ajiaco para otros priors ####
## Ajiaco Jorge Galindo ####
ajiaco_data_jorgegalindo <- tibble(
  gp = elpais_2022$pronostico[2],
  fg = elpais_2022$pronostico[1],
  sf = elpais_2022$pronostico[5],
  rh = elpais_2022$pronostico[4],
  ib = elpais_2022$pronostico[3]) %>%
  dplyr::mutate(rest = 100-rowSums(across(where(is.numeric)))) %>%
  dplyr::mutate(across(everything(),~./100))

ajiaco_multinomial_jorgegalindo <- rmultinom(ajiaco_ensayos,
                                     ajiaco_n_encuestas,
                                     ajiaco_data_jorgegalindo) %>%
  t() %>%
  tibble::as_tibble() %>%
  dplyr::summarise(across(everything(),median))

ajiaco_data_jorgegalindo <- list(
  ajiaco_n_encuestas = ajiaco_n_encuestas,
  ajiaco_priors = c(ajiaco_multinomial_jorgegalindo$gp,
                    ajiaco_multinomial_jorgegalindo$fg,
                    ajiaco_multinomial_jorgegalindo$sf,
                    ajiaco_multinomial_jorgegalindo$rh,
                    ajiaco_multinomial_jorgegalindo$ib,
                    ajiaco_multinomial_marzo$rest)
)
                    
ajiaco_jorgegalindo_fit <- stan(model = ajiaco_mensual,
                         data = ajiaco_data_jorgegalindo,
                         control=list(adapt_delta=0.95),
                         iter=1e4,
                         chains=4,
                         cores=4,
                         seed=332211) 

ajiaco_posterior <- as.matrix(ajiaco_fit)

ajiaco_jorgegalindo_fit %>% 
  as.matrix() %>%
  tibble::as_tibble() %>% 
  dplyr::select(starts_with("pred[")) %>% 
  tidyr::pivot_longer(cols = starts_with("pred["), names_to = "parameter",values_to = "estimate") %>%
  dplyr::mutate(n=stringr::str_extract(parameter,"[:digit:]") %>% as.numeric()) %>%
  dplyr::left_join(ajiaco_priors %>%
                     tidyr::pivot_longer(cols = everything(), names_to = "candidato",values_to="prior") %>%
                     dplyr::mutate(n=row_number()), by="n") %>%
  #Join names
  dplyr::left_join(tribble(~candidato,~nombre,
                           "fg","Federico Gutierrez",
                           "gp","Gustavo Petro",
                           "sf","Sergio Fajardo",
                           "rh","Rodolfo Hernandez",
                           "ib","Ingrid Betancourt",
                           #"jm","John Milton Rodriguez",
                           "rest","Resto (Otros, Blanco e Indecisos)"),
                   by="candidato") %>%
  dplyr::mutate(nombre = ifelse(str_detect(nombre,"Resto"),"Resto*",nombre)) %>%
  dplyr::mutate(nombre = factor(nombre,levels = c("Gustavo Petro","Federico Gutierrez","Rodolfo Hernandez","Sergio Fajardo","Ingrid Betancourt","Resto*"))) %>%
  dplyr::mutate(nombre = fct_rev(nombre)) %>%
  ggplot(aes(y=nombre,x=estimate,fill = 0.5-abs(0.5-stat(ecdf))))+
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Prob", direction = -1)+
  scale_x_continuous(limits = c(0,60),breaks=c(0,10,20,30,40,50,60))+
  labs(x="Distribución posterior de la intención de voto para cada candidato",
       y=NULL,
       title = "Pronósticos del Ajiaco para la primera vuelta 2022",
       subtitle = paste0("Estimación del ",lubridate::today(), " con base en ",ajiaco_disponibles," encuestas."),
       caption="*Incluye otros candidatos, voto en blanco e indecisos\nFuente: www.recetas-electorales.com")+
  theme(legend.position = "bottom")+
  theme_ridges()

## Ajiaco LSV ####
ajiaco_data_lsv <- tibble(
  gp = lsv_2022$pronostico[1],
  fg = lsv_2022$pronostico[2],
  sf = lsv_2022$pronostico[3],
  rh = lsv_2022$pronostico[4],
  #Nota: LSV no repórta para Ingrid Betancourt, usando promedio de El Pais
  ib = elpais_2022$pronostico[3]) %>%
  dplyr::mutate(rest = 100-rowSums(across(where(is.numeric)))) %>%
  dplyr::mutate(across(everything(),~./100))

ajiaco_multinomial_lsv <- rmultinom(ajiaco_ensayos,
                                    ajiaco_n_encuestas,
                                    ajiaco_data_lsv) %>%
  t() %>%
  tibble::as_tibble() %>%
  dplyr::summarise(across(everything(),median))

ajiaco_data_lsv <- list(
  ajiaco_n_encuestas = ajiaco_n_encuestas,
  ajiaco_priors = c(ajiaco_multinomial_lsv$gp,
                    ajiaco_multinomial_lsv$fg,
                    ajiaco_multinomial_lsv$sf,
                    ajiaco_multinomial_lsv$rh,
                    ajiaco_multinomial_lsv$ib,
                    ajiaco_multinomial_lsv$rest)
)

ajiaco_lsv_fit <- stan(model = ajiaco_mensual,
                       data = ajiaco_data_lsv,
                       control=list(adapt_delta=0.95),
                       iter=1e4,
                       chains=4,
                       cores=4,
                       seed=332211) 

ajiaco_lsv_fit %>% 
  as.matrix() %>%
  tibble::as_tibble() %>% 
  dplyr::select(starts_with("pred[")) %>% 
  tidyr::pivot_longer(cols = starts_with("pred["), names_to = "parameter",values_to = "estimate") %>%
  dplyr::mutate(n=stringr::str_extract(parameter,"[:digit:]") %>% as.numeric()) %>%
  dplyr::left_join(ajiaco_priors %>%
                     tidyr::pivot_longer(cols = everything(), names_to = "candidato",values_to="prior") %>%
                     dplyr::mutate(n=row_number()), by="n") %>%
  #Join names
  dplyr::left_join(tribble(~candidato,~nombre,
                           "fg","Federico Gutierrez",
                           "gp","Gustavo Petro",
                           "sf","Sergio Fajardo",
                           "rh","Rodolfo Hernandez",
                           "ib","Ingrid Betancourt",
                           #"jm","John Milton Rodriguez",
                           "rest","Resto (Otros, Blanco e Indecisos)"),
                   by="candidato") %>%
  dplyr::mutate(nombre = ifelse(str_detect(nombre,"Resto"),"Resto*",nombre)) %>%
  dplyr::mutate(nombre = factor(nombre,levels = c("Gustavo Petro","Federico Gutierrez","Rodolfo Hernandez","Sergio Fajardo","Ingrid Betancourt","Resto*"))) %>%
  dplyr::mutate(nombre = fct_rev(nombre)) %>%
  ggplot(aes(y=nombre,x=estimate,fill = 0.5-abs(0.5-stat(ecdf))))+
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Prob", direction = -1)+
  scale_x_continuous(limits = c(0,60),breaks=c(0,10,20,30,40,50,60))+
  labs(x="Distribución posterior de la intención de voto para cada candidato",
       y=NULL,
       title = "Pronósticos del Ajiaco para la primera vuelta 2022",
       subtitle = paste0("Estimación del ",lubridate::today(), " con base en ",ajiaco_disponibles," encuestas."),
       caption="*Incluye otros candidatos, voto en blanco e indecisos\nFuente: www.recetas-electorales.com")+
  theme(legend.position = "bottom")+
  theme_ridges()

# Ajiaco 2018 ####
ajiaco_2018 <- encuestas_2018 %>%
  # Seleccionar candidatos que encabezan las encuestas
  dplyr::select(n,fecha,encuestadora,ivan_duque,gustavo_petro,sergio_fajardo,german_vargas_lleras, humberto_delacalle, m_error=margen_error, muestra) %>%
  # Seleccionar solo las encuestas hechas en 2018
  dplyr::filter(between(lubridate::as_date(fecha),
                        lubridate::as_date('2018-03-11'),
                        lubridate::as_date('2018-05-27'))) %>%
    # Pivotear los datos
  tidyr::pivot_longer(cols = c("ivan_duque","gustavo_petro","sergio_fajardo","german_vargas_lleras", "humberto_delacalle"), 
                      names_to = "candidato", values_to = "int_voto") %>% 
   # Crear algunas variables
  dplyr::mutate(e_max = int_voto + m_error,
                e_min = int_voto - m_error,
                fecha = as_date(fecha),
                candidato = factor(candidato, levels=c("ivan_duque","gustavo_petro","sergio_fajardo","german_vargas_lleras","humberto_delacalle")),
                nombres = case_when(candidato=="ivan_duque" ~ "Iván Duque",
                                    candidato=="gustavo_petro" ~ "Gustavo Petro",
                                    candidato=="sergio_fajardo" ~ "Sergio Fajardo",
                                    candidato=="german_vargas_lleras" ~ "Germán Vargas Lleras",
                                    candidato=="humberto_delacalle" ~ "Humberto de la Calle")) %>%
  dplyr::group_by(nombres) %>% 
  dplyr::summarize(promedio_prior = mean(int_voto),
                   promedio_ponderado_t = weighted.mean(int_voto,n)) 

ajiaco_data_2018 <- tibble(
  id = ajiaco_2018$promedio_ponderado_t[4],
  gp = ajiaco_2018$promedio_ponderado_t[2],
  sf = ajiaco_2018$promedio_ponderado_t[5],
  gv = ajiaco_2018$promedio_ponderado_t[1],
  hc = ajiaco_2018$promedio_ponderado_t[3]) %>%
  dplyr::mutate(rest = 100-rowSums(across(where(is.numeric)))) %>%
  dplyr::mutate(across(everything(),~./100))


ajiaco_multinomial_2018 <- rmultinom(ajiaco_ensayos,
                                     ajiaco_n_encuestas,
                                     ajiaco_data_2018) %>%
  t() %>%
  tibble::as_tibble() %>%
  dplyr::summarise(across(everything(),median))

ajiaco_data_2018 <- list(
  ajiaco_n_encuestas = ajiaco_n_encuestas,
  ajiaco_priors = c(ajiaco_multinomial_2018$id,
                    ajiaco_multinomial_2018$gp,
                    ajiaco_multinomial_2018$sf,
                    ajiaco_multinomial_2018$gv,
                    ajiaco_multinomial_2018$hc,
                    ajiaco_multinomial_2018$rest)
)
ajiaco_2018_fit <- stan(model = ajiaco_mensual,
                         data = ajiaco_data_2018,
                         control=list(adapt_delta=0.95),
                         iter=1e4,
                         chains=4,
                         cores=4,
                         seed=332211)

ajiaco_2018_fit %>% 
  as.matrix() %>%
  tibble::as_tibble() %>% 
  dplyr::select(starts_with("pred[")) %>% 
  tidyr::pivot_longer(cols = starts_with("pred["), names_to = "parameter",values_to = "estimate") %>%
  dplyr::mutate(n=stringr::str_extract(parameter,"[:digit:]") %>% as.numeric()) %>%
  #Join names
  dplyr::left_join(tribble(~n,~nombre,
                           1,"Ivan Duque",
                           2,"Gustavo Petro",
                           3,"Sergio Fajardo",
                           4,"German Vargas Lleras",
                           5,"Humberto de la Calle",
                           6,"Resto (Otros, Blanco e Indecisos)"),
                   by="n") %>%
  dplyr::mutate(nombre = ifelse(str_detect(nombre,"Resto"),"Resto*",nombre)) %>%
  dplyr::mutate(nombre = factor(nombre,levels = c("Ivan Duque","Gustavo Petro","Sergio Fajardo","German Vargas Lleras","Humberto de la Calle","Resto*"))) %>%
  dplyr::mutate(nombre = fct_rev(nombre)) %>%
  ggplot(aes(y=nombre,x=estimate,fill = 0.5-abs(0.5-stat(ecdf))))+
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Prob", direction = -1)+
  scale_x_continuous(limits = c(0,60),breaks=c(0,10,20,30,40,50,60))+
  labs(x="Distribución posterior de la intención de voto para cada candidato",
       y=NULL,
       title = "Pronosticar el pasado: El Ajiaco aplicado a la primera vuelta de 2018",
       subtitle = paste0("Estimación con base en 18 encuestas disponibles antes de la 28 Mayo 2018"),
       caption="*Incluye otros candidatos, voto en blanco e indecisos\nFuente: www.recetas-electorales.com")+
  theme(legend.position = "bottom")+
  theme_ridges()

probabilidades_2018 <- function(candidato,votos) {
  
  ajiaco_2018_fit %>% 
    as.matrix() %>%
    tibble::as_tibble() %>% 
    dplyr::select(starts_with(paste0("pred[",candidato,"]"))) %>%
    dplyr::select(pred=1) %>%  
    dplyr::group_by(pred) %>%
    dplyr::mutate(n=n()) %>%
    dplyr::distinct(pred, .keep_all = TRUE) %>%
    dplyr::arrange(desc(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(p=n/sum(n)) %>%
    dplyr::filter(pred>=votos) %>%
    dplyr::summarise(100*sum(p)) %>%
    as.numeric()
  
}  
# Comparacion candidatos Ajiaco 2022 ####
ajiaco_fit %>% 
  as.matrix() %>%
  tibble::as_tibble() %>% 
  dplyr::select(`pred[2]`,`pred[4]`) %>%
  dplyr::mutate(fico_rodolfo = ifelse(`pred[2]`>`pred[4]`,1,0)) %>% 
  dplyr::count(fico_rodolfo) %>%
  dplyr::mutate(p = 100*(n/sum(n))) 

ajiaco_fit %>% 
  as.matrix() %>%
  tibble::as_tibble() %>% 
  dplyr::select(`pred[2]`,`pred[4]`) %>%
  dplyr::mutate(rodolfo_fico = ifelse(`pred[2]`<`pred[4]`,1,0)) %>% 
  dplyr::count(rodolfo_fico) %>%
  dplyr::mutate(p = 100*(n/sum(n))) 
