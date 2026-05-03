data {
  int<lower=1> J;                     // encuestas
  int<lower=2> K;                     // candidatos
  array[J] int<lower=0> n;            // tamaño muestral
  array[J, K] int<lower=0> Y;         // conteos Y[j, k]
}

parameters {
  simplex[K] p;                   // preferencias pooled
  real<lower=0> kappa;            // concentración DM
}

model {
  // priors
  p ~ dirichlet(rep_vector(1.0, K));    // relativamente débil
  kappa ~ lognormal(log(200), 0.7);    // centrado en 200

  // likelihood
  for (j in 1:J) {
    Y[j] ~ dirichlet_multinomial(kappa * p);
  }
}

generated quantities {
  vector[K] alpha = kappa * to_vector(p);
  array[J, K] int Y_rep;              // conteos
  for (j in 1:J) {
    Y_rep[j] = dirichlet_multinomial_rng(alpha, n[j]);
  }
}
