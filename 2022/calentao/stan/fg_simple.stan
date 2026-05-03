data {
  int<lower=1> N;
  int<lower=1> N_encuestadora;

  vector[N] int_voto;

  array[N] int<lower=1, upper=N_encuestadora> encuestadora;

  vector[N] merror;
  vector[N] tipo_1;
  vector[N] tipo_2;
  vector[N] tipo_3;
  vector[N] dd;

  array[N] int muestra;  // keep as int if it is truly integer; otherwise use vector[N]
}

parameters {
  real a1;

  real a_enc;
  real<lower=0> s_enc;
  vector[N_encuestadora] a_;

  real a2;
  real a3;
  real a4;
  real a5;
  real a6;
  real a7;

  real<lower=0> s;
}

transformed parameters {
  vector[N] m;

  m = a1
      + a_[encuestadora]
      + a2 * to_vector(muestra)
      + a3 * merror
      + a4 * dd
      + a5 * tipo_1
      + a6 * tipo_2
      + a7 * tipo_3;
}

model {
  // priors
  s ~ cauchy(0, 5);

  a1 ~ normal(25, 4);

  a_enc ~ normal(0, 10);
  s_enc ~ cauchy(0, 5);
  a_ ~ normal(a_enc, s_enc);

  a2 ~ normal(0, 10);
  a3 ~ normal(0, 10);
  a4 ~ normal(0, 10);
  a5 ~ normal(0, 10);
  a6 ~ normal(0, 10);
  a7 ~ normal(0, 10);

  // likelihood
  int_voto ~ normal(m, s);
}

generated quantities {
  real dev;
  dev = -2 * normal_lpdf(int_voto | m, s);
}
