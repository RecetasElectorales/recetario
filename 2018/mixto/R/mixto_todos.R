# Packages ####
library(rstan)
library(rstanarm)
library(posterior)
library(tidyverse)
library(bayesplot)
library(lubridate)
library(rethinking)
library(brms)
options(mc.cores = parallel::detectCores())

# Data ####
encuestas_simple_2018 <- readr::read_csv("https://raw.githubusercontent.com/nelsonamayad/Elecciones-presidenciales-2018/master/Elecciones%202018/encuestas2018.csv") %>%
  # Seleccionar candidatos que encabezan las encuestas
  dplyr::select(n,fecha,encuestadora,ivan_duque,gustavo_petro,sergio_fajardo,german_vargas_lleras, humberto_delacalle, m_error=margen_error, muestra_int_voto,tipo) %>%
  # Pivotear los datos
  tidyr::pivot_longer(cols = c("ivan_duque","gustavo_petro","sergio_fajardo","german_vargas_lleras", "humberto_delacalle"), 
                      names_to = "candidato", values_to = "int_voto") %>% 
  # Seleccionar solo las encuestas hechas en 2018 antes de la primea vuelta
  dplyr::filter(between(as.Date(fecha, tz="GMT"),as.Date('2018-03-11', tz="GMT"),as.Date('2018-05-19', tz="GMT"))) %>% 
  # Crear algunas variables
  dplyr::mutate(e_max = int_voto + m_error,
                e_min = int_voto - m_error,
                fecha = as.Date(fecha),
                candidato = factor(candidato, levels=c("ivan_duque","gustavo_petro","sergio_fajardo","german_vargas_lleras","humberto_delacalle")),
                enc = factor(encuestadora),
                encuestadora=as.numeric(enc)) %>%
  #Crear variable duracion:
  dplyr::mutate(dd = as.Date(as.character(today()), format="%Y-%m-%d") - as.Date(as.character(fecha), format="%Y-%m-%d")) %>%
  dplyr::mutate(dd = as.numeric(dd)) %>% 
  dplyr::mutate(dd = 100*(dd/sum(dd))) %>%
  dplyr::mutate(tipo=ifelse(tipo=="Presencial",1,0)) 


## Crear data frames por candidato: ####
id_2018_simple <- encuestas_simple_2018 %>% dplyr::filter(candidato=="ivan_duque", !is.na(int_voto))  
gp_2018_simple <- encuestas_simple_2018 %>% dplyr::filter(candidato=="gustavo_petro", !is.na(int_voto))    
sf_2018_simple <- encuestas_simple_2018 %>% dplyr::filter(candidato=="sergio_fajardo", !is.na(int_voto))   
gvl_2018_simple <- encuestas_simple_2018 %>% dplyr::filter(candidato=="german_vargas_lleras", !is.na(int_voto))  
hdlc_2018_simple <- encuestas_simple_2018 %>% dplyr::filter(candidato=="humberto_delacalle", !is.na(int_voto))  

# RStan - Modelos ####
mixto_2018_fajardo_fit <- stan(file='modelos-2018/mixto-2018/mixto_fajardo.stan',
                    data=list(
                      N=18,
                      N_encuestadora=7,
                      int_voto=sf_2018_simple$int_voto,
                      encuestadora=sf_2018_simple$encuestadora,
                      muestra_int_voto=sf_2018_simple$muestra_int_voto,
                      m_error=sf_2018_simple$m_error,
                      dd=sf_2018_simple$dd,
                      tipo=sf_2018_simple$tipo),
                    control=list(adapt_delta=0.95),
                    iter=4000,
                    chains=4)

mixto_2018_petro_fit <- stan(file='modelos-2018/mixto-2018/mixto_petro.stan',
                       data=list(
                         N=18,
                         N_encuestadora=7,
                         int_voto=gp_2018_simple$int_voto,
                         encuestadora=gp_2018_simple$encuestadora,
                         muestra_int_voto=gp_2018_simple$muestra_int_voto,
                         m_error=gp_2018_simple$m_error,
                         dd=gp_2018_simple$dd,
                         tipo=gp_2018_simple$tipo),
                       control=list(adapt_delta=0.95),
                       iter=4000,
                       chains=4)


mixto_2018_duque_fit <- stan(file='modelos-2018/mixto-2018/mixto_duque.stan',
                     data=list(
                       N=18,
                       N_encuestadora=7,
                       int_voto=id_2018_simple$int_voto,
                       encuestadora=id_2018_simple$encuestadora,
                       muestra_int_voto=id_2018_simple$muestra_int_voto,
                       m_error=id_2018_simple$m_error,
                       dd=id_2018_simple$dd,
                       tipo=id_2018_simple$tipo),
                     control=list(adapt_delta=0.95),
                     iter=4000,
                     chains=4)

mixto_2018_vargaslleras_fit <- stan(file='modelos-2018/mixto-2018/mixto_vargaslleras.stan',
                     data=list(
                       N=18,
                       N_encuestadora=7,
                       int_voto=gvl_2018_simple$int_voto,
                       encuestadora=gvl_2018_simple$encuestadora,
                       muestra_int_voto=gvl_2018_simple$muestra_int_voto,
                       m_error=gvl_2018_simple$m_error,
                       dd=gvl_2018_simple$dd,
                       tipo=gvl_2018_simple$tipo),
                     control=list(adapt_delta=0.95),
                     iter=4000,
                     chains=4)

mixto_2018_delacalle_fit <- stan(file='modelos-2018/mixto-2018/mixto_delacalle.stan',
                     data=list(
                       N=18,
                       N_encuestadora=7,
                       int_voto=hdlc_2018_simple$int_voto,
                       encuestadora=hdlc_2018_simple$encuestadora,
                       muestra_int_voto=hdlc_2018_simple$muestra_int_voto,
                       m_error=hdlc_2018_simple$m_error,
                       dd=hdlc_2018_simple$dd,
                       tipo=hdlc_2018_simple$tipo),
                     control=list(adapt_delta=0.95),
                     iter=4000,
                     chains=4)


# RStan - Samples from posterior ####
mixto_2018_resultados <- list()
mixto_2018_resultados[["mixto_2018_fajardo_fit"]] <- mixto_2018_fajardo_fit %>% 
  as.matrix() %>%
  as_tibble() %>% 
  dplyr::select(starts_with("m[")) %>%
  tidyr::pivot_longer(cols = starts_with("m["), names_to = "m")


mixto_2018_resultados[["mixto_2018_duque_fit"]] <- mixto_2018_duque_fit %>% 
  as.matrix() %>%
  as_tibble() %>% 
  dplyr::select(starts_with("m[")) %>%
  tidyr::pivot_longer(cols = starts_with("m["), names_to = "m") 

mixto_2018_resultados[["mixto_2018_vargaslleras_fit"]] <- mixto_2018_vargaslleras_fit %>% 
  as.matrix() %>%
  as_tibble() %>% 
  dplyr::select(starts_with("m[")) %>%
  tidyr::pivot_longer(cols = starts_with("m["), names_to = "m") 

mixto_2018_resultados[["mixto_2018_petro_fit"]] <- mixto_2018_petro_fit %>% 
  as.matrix() %>%
  as_tibble() %>% 
  dplyr::select(starts_with("m[")) %>%
  tidyr::pivot_longer(cols = starts_with("m["), names_to = "m")


mixto_2018_resultados[["mixto_2018_delacalle_fit"]] <- mixto_2018_delacalle_fit %>% 
  as.matrix() %>%
  as_tibble() %>% 
  dplyr::select(starts_with("m[")) %>%
  tidyr::pivot_longer(cols = starts_with("m["), names_to = "m") 

# Replication plato simple resultados ####
mixto_2018_resultados %>%
  purrr::imap(~mutate(.x, candidato = .y)) %>%
  purrr::map_df(bind_rows) %>%
  dplyr::left_join(tribble(~modelo,~nombre,
                           "mixto_2018_duque_fit","Ivan Duque",
                           "mixto_2018_petro_fit","Gustavo Petro",
                           "mixto_2018_fajardo_fit","Sergio Fajardo",
                           "mixto_2018_delacalle_fit","Humberto de la Calle",
                           "mixto_2018_vargaslleras_fit","Germán Vargas Lleras"),
                   by=c("candidato"="modelo")) %>%
  #Predicciones
  dplyr::group_by(nombre) %>%
  dplyr::summarise(m_all = mean(value),
                   p10 = quantile(value,0.1),
                   p90 = quantile(value,0.9)) %>% 
  dplyr::mutate(plato = "Mixto") %>%
  #Simple
  dplyr::bind_rows(
    simple_2018_resultados %>%
      purrr::imap(~mutate(.x, candidato = .y)) %>%
      purrr::map_df(bind_rows) %>%
      dplyr::left_join(tribble(~modelo,~nombre,
                               "simple_2018_duque_fit","Ivan Duque",
                               "simple_2018_petro_fit","Gustavo Petro",
                               "simple_2018_fajardo_fit","Sergio Fajardo",
                               "simple_2018_delacalle_fit","Humberto de la Calle",
                               "simple_2018_vargaslleras_fit","German Vargas Lleras"),
                       by=c("candidato"="modelo")) %>%
      #Predicciones
      dplyr::group_by(nombre) %>%
      dplyr::summarise(m_all = mean(value),
                       p10 = quantile(value,0.1),
                       p90 = quantile(value,0.9)) %>%
      dplyr::mutate(plato = "Simple") 
  ) %>%
  #Grafico
  ggplot(aes(x=nombre %>% reorder(m_all),color=nombre))+
  geom_point(aes(y=m_all), shape=10, size=5)+
  geom_text(aes(y=m_all, label=m_all %>% round(digits=1)),vjust=-1)+
  geom_linerange(aes(ymin=p10,ymax=p90))+
  geom_hline(yintercept=seq(10,50,10),linetype="dashed",color="grey60")+
  coord_flip()+
  facet_wrap(~plato)+
  theme(legend.position="none",
        panel.background=element_rect(fill="white", color="white"))+
  labs(x=NULL,y="Predicciones e intervalo de credibilidad 90%")+
  scale_color_manual(values=c("Ivan Duque"="orangered",
                              "Gustavo Petro"="gold2",
                              "Sergio Fajardo"="green4",
                              "Humberto de la Calle"="red2",
                              "German Vargas Lleras"="red4"))


# Export CSV ####
mixto_2018_resultados %>%
  purrr::imap(~mutate(.x, candidato = .y)) %>%
  purrr::map_df(bind_rows) %>%
  dplyr::left_join(tribble(~modelo,~nombre,
                           "mixto_2018_duque_fit","Ivan Duque",
                           "mixto_2018_petro_fit","Gustavo Petro",
                           "mixto_2018_fajardo_fit","Sergio Fajardo",
                           "mixto_2018_delacalle_fit","Humberto de la Calle",
                           "mixto_2018_vargaslleras_fit","German Vargas Lleras"),
                   by=c("candidato"="modelo")) %>%
  readr::write_csv("modelos-2018/mixto-2018/mixto_2018_resultados.csv")