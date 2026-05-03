# Rethinking - Modelos en ulam ####
ulam_mixto_duque <- ulam(
  alist(
    # Model
    int_voto ~ normal( m , s ) ,
    m <- a_encuestadora[encuestadora]+ b1_encuestadora[encuestadora]*muestra_int_voto + b2_encuestadora[encuestadora]*m_error + b3_encuestadora[encuestadora]*dd +  b4_encuestadora[encuestadora]*tipo,
    # Priors
    c(a_encuestadora,b1_encuestadora,b2_encuestadora,b3_encuestadora,b4_encuestadora)[encuestadora] ~ multi_normal(c(a,b1,b2,b3,b4),Rho,s_encuestadora),
    a ~  dnorm(38,4),
    b1 ~ dnorm(0,10),
    b2 ~ dnorm(0,10),
    b3 ~ dnorm(0,10),
    b4 ~ dnorm(0,10),
    s_encuestadora ~ dcauchy(0,5),
    s ~ dcauchy(0,5),
    Rho ~ lkj_corr(2)
  ), 
  data = list(int_voto = id_2018_simple$int_voto,
              encuestadora = id_2018_simple$encuestadora,
              muestra_int_voto = id_2018_simple$muestra_int_voto,
              m_error = id_2018_simple$m_error,
              dd = id_2018_simple$dd,
              tipo = id_2018_simple$tipo
  ),
  chains=4, 
  iter = 4000
)

# Rethinking - Modelos en map2stan ####
id.a <- map2stan(
  alist(
    #model
    int_voto ~ dnorm(m,s),
    m <- a1 + a_[encuestadora]+ a2*muestra_int_voto + a3*m_error + a4*dd + a5*tipo,
    #priors
    a1 ~ dnorm(38, 3),   
    a_[encuestadora] ~ dnorm(a_enc, s_enc),   #Non-regularized
    a_enc ~ dnorm(0, 10),    #Non-regularized
    s_enc ~ dcauchy(0, 5),   #Non-regularized
    a2 ~ dnorm(0, 10),    #Non-regularized
    a3 ~ dnorm(0, 10),    #Non-regularized
    a4 ~ dnorm(0, 10),    #Non-regularized
    a5 ~ dnorm(0,10),    #Non-regularized 
    s ~ dcauchy(0, 5)     #Non-regularized
  ),
  data=id2018.mar,
  control=list(adapt_delta=0.96),
  iter=4000, warmup=1000, chains=4, cores=2)

precis(ulam_mixto_duque,depth=2)

coeftab(ulam_mixto_duque)
link(ulam_mixto_duque)
extract.samples(ulam_mixto_duque)

# brms - Modelos ####
brms_mixto_duque <-  brm(data = id_mixto, 
                         family = gaussian,
                         int_voto ~ 1 + encuestadora + (1 + muestra_int_voto + m_error + dd + tipo | encuestadora),
                         prior = c(prior(normal(0, 10), class = Intercept),
                                   prior(normal(0, 10), class = b, group = encuestadora),
                                   prior(dcauchy(0,5), class = s),
                                   prior(dcauchy(0,5), class = s_encuestadora),
                                   prior(lkj(2), class = cor, group = encuestadora)),
                         iter = 4000, warmup = 1000, chains = 4, cores = 4,
                         seed = 867530,
                         file = NULL)

brm(data = d, 
    family = gaussian,
    wait ~ 1 + afternoon + (1 + afternoon | cafe),
    prior = c(prior(normal(5, 2), class = Intercept),
              prior(normal(-1, 0.5), class = b),
              prior(exponential(1), class = sd),
              prior(exponential(1), class = sigma),
              prior(lkj(2), class = cor)),
    iter = 2000, warmup = 1000, chains = 4, cores = 4,
    seed = 867530,
    file = "fits/b14.01")

ulam(
  alist(
    wait ~ normal( mu , sigma ),
    mu <- a_cafe[cafe] + b_cafe[cafe]*afternoon,
    c(a_cafe,b_cafe)[cafe] ~ multi_normal( c(a,b) , Rho , sigma_cafe ),
    a ~ normal(5,2),
    b ~ normal(-1,0.5),
    sigma_cafe ~ exponential(1),
    sigma ~ exponential(1),
    Rho ~ lkj_corr(2)
  ) , data=d , chains=4 , cores=4 )
