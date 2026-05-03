# Packages ####
library(rstan)
library(rstanarm)
library(posterior)
library(tidyverse)
library(bayesplot)
library(lubridate)
library(rethinking)
library(brms)

# Calentao datos ####
calentao_data <- readr::read_csv("https://raw.githubusercontent.com/nelsonamayad/Elecciones-presidenciales-2022/main/Encuestas%202022/encuestas_2022.csv") %>% 
  dplyr::select(n,fecha,muestra,encuestadora,merror=margen_error,tipo,
                gustavo_petro,federico_gutierrez,sergio_fajardo,rodolfo_hernandez,ingrid_betancourt) %>%
  dplyr::filter(between(lubridate::as_date(fecha),
                        as_date("2022-03-13"),
                        as_date("2022-05-28"))) %>%
  tidyr::pivot_longer(cols=contains("_"),names_to = "candidato", values_to = "int_voto") %>% 
  dplyr::mutate(nombres = case_when(candidato=="gustavo_petro" ~ "Gustavo Petro",
                                    candidato=="federico_gutierrez" ~ "Federico Gutierrez",
                                    candidato=="sergio_fajardo" ~ "Sergio Fajardo",
                                    candidato=="rodolfo_hernandez" ~ "Rodolfo Hernandez",
                                    candidato=="ingrid_betancourt" ~ "Ingrid Betancourt")) %>%  
  # Crear algunas variables
  dplyr::mutate(e_max = int_voto + merror,
                e_min = int_voto - merror,
                fecha = lubridate::as_date(fecha),
                candidato = factor(candidato, levels=c("gustavo_petro","federico_gutierrez","sergio_fajardo","rodolfo_hernandez","ingrid_betancourt")),
                enc = factor(encuestadora),
                encuestadora=as.numeric(enc)) %>%
  #Crear variable duracion:
  dplyr::mutate(dd = lubridate::as_date(as.character(today()), format="%Y-%m-%d") - lubridate::as_date(as.character(fecha), format="%Y-%m-%d")) %>%
  dplyr::mutate(dd = as.numeric(dd)) %>% 
  dplyr::mutate(tipo_1=ifelse(tipo=="presencial",1,0),
                tipo_2=ifelse(tipo=="telefonico y presencial",1,0),
                tipo_3=ifelse(tipo=="digital",1,0))

## Data frames por candidato ####
gp_calentao <- calentao_data %>% dplyr::filter(candidato=="gustavo_petro", !is.na(int_voto))  
fg_calentao <- calentao_data %>% dplyr::filter(candidato=="federico_gutierrez", !is.na(int_voto))  
sf_calentao <- calentao_data %>% dplyr::filter(candidato=="sergio_fajardo", !is.na(int_voto))  
rh_calentao <- calentao_data %>% dplyr::filter(candidato=="rodolfo_hernandez", !is.na(int_voto))  
ib_calentao <- calentao_data %>% dplyr::filter(candidato=="ingrid_betancourt", !is.na(int_voto))  
encuestas_disponibles <- calentao_data %>% dplyr::group_by(candidato) %>% dplyr::tally() %>% dplyr::distinct(n) %>% as.numeric()
encuestadoras_disponibles <- calentao_data %>% dplyr::distinct(encuestadora) %>% dplyr::tally() %>% as.numeric()

# Rethinking - Modelos mixtos en ulam ####
## Gustavo Petro - Mixto ulam ####
gp_mixto_ulam <- ulam(
  alist(
    # Model
    int_voto ~ normal( m , s ) ,
    m <- a_encuestadora[encuestadora]+ 
      b1_encuestadora[encuestadora]*muestra + 
      b2_encuestadora[encuestadora]*merror + 
      b3_encuestadora[encuestadora]*dd+  
      b4_encuestadora[encuestadora]*tipo_1 + 
      b5_encuestadora[encuestadora]*tipo_2 + 
      b6_encuestadora[encuestadora]*tipo_3,
    
    # Priors
    c(a_encuestadora,b1_encuestadora,b2_encuestadora,b3_encuestadora,b4_encuestadora,b5_encuestadora,b6_encuestadora)[encuestadora] ~ multi_normal(c(a,b1,b2,b3,b4,b5,b6),Rho,s_encuestadora),
    a ~  dnorm(38,4),
    b1 ~ dnorm(0,10),
    b2 ~ dnorm(0,10),
    b3 ~ dnorm(0,10),
    b4 ~ dnorm(0,10),
    b5 ~ dnorm(0,10),
    b6 ~ dnorm(0,10),
    s_encuestadora ~ dcauchy(0,5),
    s ~ dcauchy(0,5),
    Rho ~ lkj_corr(2)
  ), 
  data=list(N=encuestas_disponibles,
            N_encuestadora=encuestadoras_disponibles,
            int_voto=gp_calentao$int_voto,
            encuestadora=gp_calentao$encuestadora,
            muestra=gp_calentao$muestra,
            merror=gp_calentao$merror,
            dd=gp_calentao$dd,
            tipo_1=gp_calentao$tipo_1,
            tipo_2=gp_calentao$tipo_2,
            tipo_3=gp_calentao$tipo_3
  ),
  chains=4, 
  cores = 4,
  iter = 4000
)
## Federico Gutierrez - Mixto ulam ####
fg_mixto_ulam <- ulam(
  alist(
    # Model
    int_voto ~ normal( m , s ) ,
    m <- a_encuestadora[encuestadora]+ 
      b1_encuestadora[encuestadora]*muestra + 
      b2_encuestadora[encuestadora]*merror + 
      b3_encuestadora[encuestadora]*dd+  
      b4_encuestadora[encuestadora]*tipo_1 + 
      b5_encuestadora[encuestadora]*tipo_2 + 
      b6_encuestadora[encuestadora]*tipo_3,
    
    # Priors
    c(a_encuestadora,b1_encuestadora,b2_encuestadora,b3_encuestadora,b4_encuestadora,b5_encuestadora,b6_encuestadora)[encuestadora] ~ multi_normal(c(a,b1,b2,b3,b4,b5,b6),Rho,s_encuestadora),
    a ~  dnorm(25,4),
    b1 ~ dnorm(0,10),
    b2 ~ dnorm(0,10),
    b3 ~ dnorm(0,10),
    b4 ~ dnorm(0,10),
    b5 ~ dnorm(0,10),
    b6 ~ dnorm(0,10),
    s_encuestadora ~ dcauchy(0,5),
    s ~ dcauchy(0,5),
    Rho ~ lkj_corr(2)
  ), 
  data=list(N=encuestas_disponibles,
            N_encuestadora=encuestadoras_disponibles,
            int_voto=fg_calentao$int_voto,
            encuestadora=fg_calentao$encuestadora,
            muestra=fg_calentao$muestra,
            merror=fg_calentao$merror,
            dd=fg_calentao$dd,
            tipo_1=fg_calentao$tipo_1,
            tipo_2=fg_calentao$tipo_2,
            tipo_3=fg_calentao$tipo_3
  ),
  chains=4, 
  cores = 4,
  iter = 4000
)
## Sergio Fajardo - Mixto ulam ####
sf_mixto_ulam <- ulam(
  alist(
    # Model
    int_voto ~ normal( m , s ) ,
    m <- a_encuestadora[encuestadora]+ 
      b1_encuestadora[encuestadora]*muestra + 
      b2_encuestadora[encuestadora]*merror + 
      b3_encuestadora[encuestadora]*dd+  
      b4_encuestadora[encuestadora]*tipo_1 + 
      b5_encuestadora[encuestadora]*tipo_2 + 
      b6_encuestadora[encuestadora]*tipo_3,
    
    # Priors
    c(a_encuestadora,b1_encuestadora,b2_encuestadora,b3_encuestadora,b4_encuestadora,b5_encuestadora,b6_encuestadora)[encuestadora] ~ multi_normal(c(a,b1,b2,b3,b4,b5,b6),Rho,s_encuestadora),
    a ~  dnorm(7,2),
    b1 ~ dnorm(0,10),
    b2 ~ dnorm(0,10),
    b3 ~ dnorm(0,10),
    b4 ~ dnorm(0,10),
    b5 ~ dnorm(0,10),
    b6 ~ dnorm(0,10),
    s_encuestadora ~ dcauchy(0,5),
    s ~ dcauchy(0,5),
    Rho ~ lkj_corr(2)
  ), 
  data=list(N=encuestas_disponibles,
            N_encuestadora=encuestadoras_disponibles,
            int_voto=sf_calentao$int_voto,
            encuestadora=sf_calentao$encuestadora,
            muestra=sf_calentao$muestra,
            merror=sf_calentao$merror,
            dd=sf_calentao$dd,
            tipo_1=sf_calentao$tipo_1,
            tipo_2=sf_calentao$tipo_2,
            tipo_3=sf_calentao$tipo_3
  ),
  chains=4, 
  cores = 4,
  iter = 4000
)

## Rodolfo Hernandez - Mixto ulam ####
rh_mixto_ulam <- ulam(
  alist(
    # Model
    int_voto ~ normal( m , s ) ,
    m <- a_encuestadora[encuestadora]+ 
      b1_encuestadora[encuestadora]*muestra + 
      b2_encuestadora[encuestadora]*merror + 
      b3_encuestadora[encuestadora]*dd+  
      b4_encuestadora[encuestadora]*tipo_1 + 
      b5_encuestadora[encuestadora]*tipo_2 + 
      b6_encuestadora[encuestadora]*tipo_3,
    
    # Priors
    c(a_encuestadora,b1_encuestadora,b2_encuestadora,b3_encuestadora,b4_encuestadora,b5_encuestadora,b6_encuestadora)[encuestadora] ~ multi_normal(c(a,b1,b2,b3,b4,b5,b6),Rho,s_encuestadora),
    a ~  dnorm(14,4),
    b1 ~ dnorm(0,10),
    b2 ~ dnorm(0,10),
    b3 ~ dnorm(0,10),
    b4 ~ dnorm(0,10),
    b5 ~ dnorm(0,10),
    b6 ~ dnorm(0,10),
    s_encuestadora ~ dcauchy(0,5),
    s ~ dcauchy(0,5),
    Rho ~ lkj_corr(2)
  ), 
  data=list(N=encuestas_disponibles,
            N_encuestadora=encuestadoras_disponibles,
            int_voto=rh_calentao$int_voto,
            encuestadora=rh_calentao$encuestadora,
            muestra=rh_calentao$muestra,
            merror=rh_calentao$merror,
            dd=rh_calentao$dd,
            tipo_1=rh_calentao$tipo_1,
            tipo_2=rh_calentao$tipo_2,
            tipo_3=rh_calentao$tipo_3
  ),
  chains=4, 
  cores = 4,
  iter = 4000
)
## Ingrid Betancourt - Mixto ulam BLOCKED ####
# ib_mixto_ulam <- ulam(
#   alist(
#     # Model
#     int_voto ~ normal( m , s ) ,
#     m <- a_encuestadora[encuestadora]+ 
#       b1_encuestadora[encuestadora]*muestra + 
#       b2_encuestadora[encuestadora]*merror + 
#       b3_encuestadora[encuestadora]*dd+  
#       b4_encuestadora[encuestadora]*tipo_1 + 
#       b5_encuestadora[encuestadora]*tipo_2 + 
#       b6_encuestadora[encuestadora]*tipo_3,
#     
#     # Priors
#     c(a_encuestadora,b1_encuestadora,b2_encuestadora,b3_encuestadora,b4_encuestadora,b5_encuestadora,b6_encuestadora)[encuestadora] ~ multi_normal(c(a,b1,b2,b3,b4,b5,b6),Rho,s_encuestadora),
#     a ~  dnorm(1,1),
#     b1 ~ dnorm(0,10),
#     b2 ~ dnorm(0,10),
#     b3 ~ dnorm(0,10),
#     b4 ~ dnorm(0,10),
#     b5 ~ dnorm(0,10),
#     b6 ~ dnorm(0,10),
#     s_encuestadora ~ dcauchy(0,5),
#     s ~ dcauchy(0,5),
#     Rho ~ lkj_corr(2)
#   ), 
#   data=list(N=encuestas_disponibles,
#             N_encuestadora=encuestadoras_disponibles,
#             int_voto=ib_calentao$int_voto,
#             encuestadora=ib_calentao$encuestadora,
#             muestra=ib_calentao$muestra,
#             merror=ib_calentao$merror,
#             dd=ib_calentao$dd,
#             tipo_1=ib_calentao$tipo_1,
#             tipo_2=ib_calentao$tipo_2,
#             tipo_3=ib_calentao$tipo_3
#   ),
#   chains=4, 
#   cores = 4,
#   iter = 4000
# )


# Mixto en Stan ####
## Gustavo Petro - Mixto en Stan ####
gp_mixto_stan_fit <- stan(
                      model = rethinking::stancode(gp_mixto_ulam),
                      data=list(N=encuestas_disponibles,
                                N_encuestadora=encuestadoras_disponibles,
                                int_voto=gp_calentao$int_voto,
                                encuestadora=gp_calentao$encuestadora,
                                muestra=gp_calentao$muestra,
                                merror=gp_calentao$merror,
                                dd=gp_calentao$dd,
                                tipo_1=gp_calentao$tipo_1,
                                tipo_2=gp_calentao$tipo_2,
                                tipo_3=gp_calentao$tipo_3),
                      control=list(adapt_delta=0.95),
                      iter=4000,
                      chains=4,
                      cores=4)

## Federico Gutierrez - Mixto en Stan ####
fg_mixto_stan_fit <- stan(
  model = rethinking::stancode(fg_mixto_ulam),
  data=list(N=encuestas_disponibles,
            N_encuestadora=encuestadoras_disponibles,
            int_voto=fg_calentao$int_voto,
            encuestadora=fg_calentao$encuestadora,
            muestra=fg_calentao$muestra,
            merror=fg_calentao$merror,
            dd=fg_calentao$dd,
            tipo_1=fg_calentao$tipo_1,
            tipo_2=fg_calentao$tipo_2,
            tipo_3=fg_calentao$tipo_3),
  control=list(adapt_delta=0.95),
  iter=4000,
  chains=4,
  cores=4)

## Sergio Fajardo - Mixto en Stan ####
sf_mixto_stan_fit <- stan(
  model = rethinking::stancode(sf_mixto_ulam),
  data=list(N=encuestas_disponibles,
            N_encuestadora=encuestas_disponibles,
            int_voto=sf_calentao$int_voto,
            encuestadora=sf_calentao$encuestadora,
            muestra=sf_calentao$muestra,
            merror=sf_calentao$merror,
            dd=sf_calentao$dd,
            tipo_1=sf_calentao$tipo_1,
            tipo_2=sf_calentao$tipo_2,
            tipo_3=sf_calentao$tipo_3),
  control=list(adapt_delta=0.95),
  iter=4000,
  chains=4,
  cores=4)

## Rodolfo Hernandez - Mixto en Stan ####
rh_mixto_stan_fit <- stan(
  model = rethinking::stancode(rh_mixto_ulam),
  data=list(N=encuestas_disponibles,
            N_encuestadora=encuestadoras_disponibles,
            int_voto=rh_calentao$int_voto,
            encuestadora=rh_calentao$encuestadora,
            muestra=rh_calentao$muestra,
            merror=rh_calentao$merror,
            dd=rh_calentao$dd,
            tipo_1=rh_calentao$tipo_1,
            tipo_2=rh_calentao$tipo_2,
            tipo_3=rh_calentao$tipo_3),
  control=list(adapt_delta=0.95),
  iter=4000,
  chains=4,
  cores=4)

## Ingrid Betancourt - Mixto en Stan BLOCKED ####
# ib_mixto_stan_fit <- stan(
#   model = rethinking::stancode(ib_mixto_ulam),
#   data=list(N=encuestadoras_disponibles,
#             N_encuestadora=encuestadoras_disponibles,
#             int_voto=ib_calentao$int_voto,
#             encuestadora=ib_calentao$encuestadora,
#             muestra=ib_calentao$muestra,
#             merror=ib_calentao$merror,
#             dd=ib_calentao$dd,
#             tipo_1=ib_calentao$tipo_1,
#             tipo_2=ib_calentao$tipo_2,
#             tipo_3=ib_calentao$tipo_3),
#   control=list(adapt_delta=0.95),
#   iter=4000,
#   chains=4,
#   cores=4)

# Modelos simples - Stan ####
{gp_model_simple <- "
data{
    int<lower=1> N;
    int<lower=1> N_encuestadora;
    real int_voto[N];
    int encuestadora[N];
    real merror[N];
    real tipo_1[N];
    real tipo_2[N];
    real tipo_3[N];
    int muestra[N];
    real dd[N];
}
parameters{
    real a1;
    vector[N_encuestadora] a_;
    real a_enc;
    real<lower=0> s_enc;
    real a2;
    real a3;
    real a4;
    real a5;
    real a6;
    real a7;
    real<lower=0> s;
}
model{
    vector[N] m;
    s ~ cauchy( 0 , 5 );
    a7 ~ normal( 0 , 10 );
    a6 ~ normal( 0 , 10 );
    a5 ~ normal( 0 , 10 );
    a4 ~ normal( 0 , 10 );
    a3 ~ normal( 0 , 10 );
    a2 ~ normal( 0 , 10 );
    s_enc ~ cauchy( 0 , 5 );
    a_enc ~ normal( 0 , 10 );
    a_ ~ normal( a_enc , s_enc );
    a1 ~ normal( 38 , 4 ); 
    for ( i in 1:N ) {
        m[i] = a1 + a_[encuestadora[i]] + a2 * muestra[i] + a3 * merror[i] +  a4 * dd[i] + a5 * tipo_1[i] + a6 * tipo_2[i] + a7 * tipo_3[i];
    }
    int_voto ~ normal( m , s );
}
generated quantities{
    vector[N] m;
    real dev;
    dev = 0;
    for ( i in 1:N ) {
        m[i] = a1 + a_[encuestadora[i]] + a2 * muestra[i] + a3 * merror[i] +  a4 * dd[i] + a5 * tipo_1[i] + a6 * tipo_2[i] + a7 * tipo_3[i];
    }
    dev = dev + (-2)*normal_lpdf( int_voto | m , s );
}

"
}
{fg_model_simple <- "
data{
    int<lower=1> N;
    int<lower=1> N_encuestadora;
    real int_voto[N];
    int encuestadora[N];
    real merror[N];
    real tipo_1[N];
    real tipo_2[N];
    real tipo_3[N];
    int muestra[N];
    real dd[N];
}
parameters{
    real a1;
    vector[N_encuestadora] a_;
    real a_enc;
    real<lower=0> s_enc;
    real a2;
    real a3;
    real a4;
    real a5;
    real a6;
    real a7;
    real<lower=0> s;
}
model{
    vector[N] m;
    s ~ cauchy( 0 , 5 );
    a7 ~ normal( 0 , 10 );
    a6 ~ normal( 0 , 10 );
    a5 ~ normal( 0 , 10 );
    a4 ~ normal( 0 , 10 );
    a3 ~ normal( 0 , 10 );
    a2 ~ normal( 0 , 10 );
    s_enc ~ cauchy( 0 , 5 );
    a_enc ~ normal( 0 , 10 );
    a_ ~ normal( a_enc , s_enc );
    a1 ~ normal( 25 , 4 ); 
    for ( i in 1:N ) {
        m[i] = a1 + a_[encuestadora[i]] + a2 * muestra[i] + a3 * merror[i] +  a4 * dd[i] + a5 * tipo_1[i] + a6 * tipo_2[i] + a7 * tipo_3[i];
    }
    int_voto ~ normal( m , s );
}
generated quantities{
    vector[N] m;
    real dev;
    dev = 0;
    for ( i in 1:N ) {
        m[i] = a1 + a_[encuestadora[i]] + a2 * muestra[i] + a3 * merror[i] +  a4 * dd[i] + a5 * tipo_1[i] + a6 * tipo_2[i] + a7 * tipo_3[i];
    }
    dev = dev + (-2)*normal_lpdf( int_voto | m , s );
}

"
}
{sf_model_simple <- "
data{
    int<lower=1> N;
    int<lower=1> N_encuestadora;
    real int_voto[N];
    int encuestadora[N];
    real merror[N];
    real tipo_1[N];
    real tipo_2[N];
    real tipo_3[N];
    int muestra[N];
    real dd[N];
}
parameters{
    real a1;
    vector[N_encuestadora] a_;
    real a_enc;
    real<lower=0> s_enc;
    real a2;
    real a3;
    real a4;
    real a5;
    real a6;
    real a7;
    real<lower=0> s;
}
model{
    vector[N] m;
    s ~ cauchy( 0 , 5 );
    a7 ~ normal( 0 , 10 );
    a6 ~ normal( 0 , 10 );
    a5 ~ normal( 0 , 10 );
    a4 ~ normal( 0 , 10 );
    a3 ~ normal( 0 , 10 );
    a2 ~ normal( 0 , 10 );
    s_enc ~ cauchy( 0 , 5 );
    a_enc ~ normal( 0 , 10 );
    a_ ~ normal( a_enc , s_enc );
    a1 ~ normal( 7 , 2 ); 
    for ( i in 1:N ) {
        m[i] = a1 + a_[encuestadora[i]] + a2 * muestra[i] + a3 * merror[i] +  a4 * dd[i] + a5 * tipo_1[i] + a6 * tipo_2[i] + a7 * tipo_3[i];
    }
    int_voto ~ normal( m , s );
}
generated quantities{
    vector[N] m;
    real dev;
    dev = 0;
    for ( i in 1:N ) {
        m[i] = a1 + a_[encuestadora[i]] + a2 * muestra[i] + a3 * merror[i] +  a4 * dd[i] + a5 * tipo_1[i] + a6 * tipo_2[i] + a7 * tipo_3[i];
    }
    dev = dev + (-2)*normal_lpdf( int_voto | m , s );
}

"
}
{rh_model_simple <- "
data{
    int<lower=1> N;
    int<lower=1> N_encuestadora;
    real int_voto[N];
    int encuestadora[N];
    real merror[N];
    real tipo_1[N];
    real tipo_2[N];
    real tipo_3[N];
    int muestra[N];
    real dd[N];
}
parameters{
    real a1;
    vector[N_encuestadora] a_;
    real a_enc;
    real<lower=0> s_enc;
    real a2;
    real a3;
    real a4;
    real a5;
    real a6;
    real a7;
    real<lower=0> s;
}
model{
    vector[N] m;
    s ~ cauchy( 0 , 5 );
    a7 ~ normal( 0 , 10 );
    a6 ~ normal( 0 , 10 );
    a5 ~ normal( 0 , 10 );
    a4 ~ normal( 0 , 10 );
    a3 ~ normal( 0 , 10 );
    a2 ~ normal( 0 , 10 );
    s_enc ~ cauchy( 0 , 5 );
    a_enc ~ normal( 0 , 10 );
    a_ ~ normal( a_enc , s_enc );
    a1 ~ normal( 14 , 4 ); 
    for ( i in 1:N ) {
        m[i] = a1 + a_[encuestadora[i]] + a2 * muestra[i] + a3 * merror[i] +  a4 * dd[i] + a5 * tipo_1[i] + a6 * tipo_2[i] + a7 * tipo_3[i];
    }
    int_voto ~ normal( m , s );
}
generated quantities{
    vector[N] m;
    real dev;
    dev = 0;
    for ( i in 1:N ) {
        m[i] = a1 + a_[encuestadora[i]] + a2 * muestra[i] + a3 * merror[i] +  a4 * dd[i] + a5 * tipo_1[i] + a6 * tipo_2[i] + a7 * tipo_3[i];
    }
    dev = dev + (-2)*normal_lpdf( int_voto | m , s );
}

"
}
{ib_model_simple <- "
data{
    int<lower=1> N;
    int<lower=1> N_encuestadora;
    real int_voto[N];
    int encuestadora[N];
    real merror[N];
    real tipo_1[N];
    real tipo_2[N];
    real tipo_3[N];
    int muestra[N];
    real dd[N];
}
parameters{
    real a1;
    vector[N_encuestadora] a_;
    real a_enc;
    real<lower=0> s_enc;
    real a2;
    real a3;
    real a4;
    real a5;
    real a6;
    real a7;
    real<lower=0> s;
}
model{
    vector[N] m;
    s ~ cauchy( 0 , 5 );
    a7 ~ normal( 0 , 10 );
    a6 ~ normal( 0 , 10 );
    a5 ~ normal( 0 , 10 );
    a4 ~ normal( 0 , 10 );
    a3 ~ normal( 0 , 10 );
    a2 ~ normal( 0 , 10 );
    s_enc ~ cauchy( 0 , 5 );
    a_enc ~ normal( 0 , 10 );
    a_ ~ normal( a_enc , s_enc );
    a1 ~ normal( 1 , 1 ); 
    for ( i in 1:N ) {
        m[i] = a1 + a_[encuestadora[i]] + a2 * muestra[i] + a3 * merror[i] +  a4 * dd[i] + a5 * tipo_1[i] + a6 * tipo_2[i] + a7 * tipo_3[i];
    }
    int_voto ~ normal( m , s );
}
generated quantities{
    vector[N] m;
    real dev;
    dev = 0;
    for ( i in 1:N ) {
        m[i] = a1 + a_[encuestadora[i]] + a2 * muestra[i] + a3 * merror[i] +  a4 * dd[i] + a5 * tipo_1[i] + a6 * tipo_2[i] + a7 * tipo_3[i];
    }
    dev = dev + (-2)*normal_lpdf( int_voto | m , s );
}

"
}
## Gustavo Petro - Simple Stan ####
gp_simple_fit <- stan(model = gp_model_simple,
                     data=list(N=encuestas_disponibles,
                               N_encuestadora=encuestadoras_disponibles,
                               int_voto=gp_calentao$int_voto,
                               encuestadora=gp_calentao$encuestadora,
                               muestra=gp_calentao$muestra,
                               merror=gp_calentao$merror,
                               dd=gp_calentao$dd,
                               tipo_1=gp_calentao$tipo_1,
                               tipo_2=gp_calentao$tipo_2,
                               tipo_3=gp_calentao$tipo_3),
                     control=list(adapt_delta=0.95),
                     iter=4000,
                     chains=4,
                     cores=4)

## Fico Gutierrez - Simple Stan ####
fg_simple_fit <- stan(model = fg_model_simple,
                      data=list(N=encuestas_disponibles,
                                N_encuestadora=encuestadoras_disponibles,
                                int_voto=fg_calentao$int_voto,
                                encuestadora=fg_calentao$encuestadora,
                                muestra=fg_calentao$muestra,
                                merror=fg_calentao$merror,
                                dd=fg_calentao$dd,
                                tipo_1=fg_calentao$tipo_1,
                                tipo_2=fg_calentao$tipo_2,
                                tipo_3=fg_calentao$tipo_3),
                      control=list(adapt_delta=0.95),
                      iter=4000,
                      chains=4,
                      cores=4)
## Sergio Fajardo - Simple Stan ####
sf_simple_fit <- stan(model = sf_model_simple,
                      data=list(N=encuestadoras_disponibles,
                                N_encuestadora=encuestadoras_disponibles,
                                int_voto=sf_calentao$int_voto,
                                encuestadora=sf_calentao$encuestadora,
                                muestra=sf_calentao$muestra,
                                merror=sf_calentao$merror,
                                dd=sf_calentao$dd,
                                tipo_1=sf_calentao$tipo_1,
                                tipo_2=sf_calentao$tipo_2,
                                tipo_3=sf_calentao$tipo_3),
                      control=list(adapt_delta=0.95),
                      iter=4000,
                      chains=4,
                      cores=4)

## Rodolfo Hernandez - Simple Stan ####
rh_simple_fit <- stan(model = rh_model_simple,
                      data=list(N=encuestas_disponibles,
                                N_encuestadora=encuestadoras_disponibles,
                                int_voto=rh_calentao$int_voto,
                                encuestadora=rh_calentao$encuestadora,
                                muestra=rh_calentao$muestra,
                                merror=rh_calentao$merror,
                                dd=rh_calentao$dd,
                                tipo_1=rh_calentao$tipo_1,
                                tipo_2=rh_calentao$tipo_2,
                                tipo_3=rh_calentao$tipo_3),
                      control=list(adapt_delta=0.95),
                      iter=4000,
                      chains=4,
                      cores=4)

## Ingrid Betancourt - Simple Stan BLOCKED ####
# ib_simple_fit <- stan(model = ib_model_simple,
#                       data=list(N=encuestas_disponibles,
#                                 N_encuestadora=encuestadoras_disponibles,
#                                 int_voto=ib_calentao$int_voto,
#                                 encuestadora=ib_calentao$encuestadora,
#                                 muestra=ib_calentao$muestra,
#                                 merror=ib_calentao$merror,
#                                 dd=ib_calentao$dd,
#                                 tipo_1=ib_calentao$tipo_1,
#                                 tipo_2=ib_calentao$tipo_2,
#                                 tipo_3=ib_calentao$tipo_3),
#                       control=list(adapt_delta=0.95),
#                       iter=4000,
#                       chains=4,
#                       cores=4)

# Resultados - Simple ####
calentao_simple_resultados <- list()

calentao_simple_resultados[["gp_simple_fit"]] <- gp_simple_fit %>% 
  as.matrix() %>%
  as_tibble() %>% 
  dplyr::select(starts_with("m[")) %>%
  tidyr::pivot_longer(cols = starts_with("m["), names_to = "m") 

calentao_simple_resultados[["fg_simple_fit"]] <- fg_simple_fit %>% 
  as.matrix() %>%
  as_tibble() %>% 
  dplyr::select(starts_with("m[")) %>%
  tidyr::pivot_longer(cols = starts_with("m["), names_to = "m") 

calentao_simple_resultados[["sf_simple_fit"]] <- sf_simple_fit %>% 
  as.matrix() %>%
  as_tibble() %>% 
  dplyr::select(starts_with("m[")) %>%
  tidyr::pivot_longer(cols = starts_with("m["), names_to = "m") 

calentao_simple_resultados[["rh_simple_fit"]] <- rh_simple_fit %>% 
  as.matrix() %>%
  as_tibble() %>% 
  dplyr::select(starts_with("m[")) %>%
  tidyr::pivot_longer(cols = starts_with("m["), names_to = "m") 


# Resultados - Mixto ####

calentao_mixto_resultados <- list()

calentao_mixto_resultados[["gp_mixto_fit"]] <- rethinking::link_ulam(gp_mixto_ulam) %>% 
  as_tibble() %>% 
  tidyr::pivot_longer(cols = everything(), names_to = "int_voto_pred")

calentao_mixto_resultados[["fg_mixto_fit"]] <-  rethinking::link_ulam(fg_mixto_ulam)  %>% 
  as_tibble() %>% 
  tidyr::pivot_longer(cols = everything(), names_to = "int_voto_pred")

calentao_mixto_resultados[["sf_mixto_fit"]] <-  rethinking::link_ulam(sf_mixto_ulam)  %>% 
  as_tibble() %>% 
  tidyr::pivot_longer(cols = everything(), names_to = "int_voto_pred")

calentao_mixto_resultados[["rh_mixto_fit"]] <-  rethinking::link_ulam(rh_mixto_ulam)  %>% 
  as_tibble() %>% 
  tidyr::pivot_longer(cols = everything(), names_to = "int_voto_pred")


# Calentao de resultados Mixto y Simple ####
calentao_simple_resultados %>%
  purrr::imap(~mutate(.x, candidato = .y)) %>%
  purrr::map_df(bind_rows) %>%
  dplyr::left_join(tribble(~modelo,~nombre,
                           "gp_simple_fit","Gustavo Petro",
                           "fg_simple_fit","Federico Gutierrez",
                           "sf_simple_fit","Sergio Fajardo",
                           "rh_simple_fit","Rodolgo Hernandez"),
                   by=c("candidato"="modelo")) %>%
  #Predicciones
  dplyr::group_by(nombre) %>%
  dplyr::summarise(m_all = mean(value),
                   p10 = quantile(value,0.1),
                   p90 = quantile(value,0.9)) %>% 
  dplyr::mutate(plato = "Simple 2022") %>%
  #Mixto
  dplyr::bind_rows(
    calentao_mixto_resultados %>%
      purrr::imap(~mutate(.x, candidato = .y)) %>%
      purrr::map_df(bind_rows) %>%
      dplyr::left_join(tribble(~modelo,~nombre,
                               "gp_mixto_fit","Gustavo Petro",
                               "fg_mixto_fit","Federico Gutierrez",
                               "sf_mixto_fit","Sergio Fajardo",
                               "rh_mixto_fit","Rodolgo Hernandez"),
                       by=c("candidato"="modelo")) %>%
      #Predicciones
      dplyr::group_by(nombre) %>%
      dplyr::summarise(m_all = mean(value),
                       p10 = quantile(value,0.1),
                       p90 = quantile(value,0.9)) %>%
      dplyr::mutate(plato = "Mixto 2022") 
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
  labs(x=NULL,y="Predicciones e intervalo de credibilidad 90%")



## Compare simple model Gustavo Petro map2stan ####
gp_simple_5 <- map2stan(
  alist(
    # Model
    int_voto ~ normal( m , s ) ,
    m <- a0 + a1*muestra + a2*merror + a3*dd+  a4*tipo_1 + a5*tipo_2 + a6*tipo_3,
    # Priors
    a0 ~  dnorm(34,2),
    a1 ~ dnorm(0,10),
    a2 ~ dnorm(0,10),
    a3 ~ dnorm(0,10),
    a4 ~ dnorm(0,10),
    a5 ~ dnorm(0,10),
    a6 ~ dnorm(0,10),
    s ~ dexp(1)  
  ), 
  data=list(N=disponibles,
            N_encuestadora=encuestadoras_disponibles,
            int_voto=gp_calentao$int_voto,
            encuestadora=gp_calentao$encuestadora,
            muestra=gp_calentao$muestra,
            merror=gp_calentao$merror,
            dd=gp_calentao$dd,
            tipo_1=gp_calentao$tipo_1,
            tipo_2=gp_calentao$tipo_2,
            tipo_3=gp_calentao$tipo_3
  ),
  chains=4, 
  cores = 4,
  iter = 4000
)


# Export CSV ####
calentao_simple_resultados %>%
  purrr::imap(~mutate(.x, modelo = .y)) %>%
  purrr::map_df(bind_rows) %>%
  dplyr::select(int_voto_pred=m, everything()) %>%
  dplyr::mutate(plato = "Simple 2022") %>%
  dplyr::bind_rows(
    calentao_mixto_resultados %>%
      purrr::imap(~mutate(.x, modelo = .y)) %>%
      purrr::map_df(bind_rows) %>%
      dplyr::mutate(plato = "Mixto 2022")
  ) %>%
  readr::write_csv("modelos-2022/calentao/calentao-2022_resultados.csv")