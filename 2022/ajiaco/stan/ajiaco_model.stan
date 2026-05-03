data {
  int<lower=1> ajiaco_n_encuestas;                 // numero total de “trials” (p.ej., encuestas agregadas)
  array[5] int<lower=0> ajiaco_priors;             // conteos por candidato (deben sumar ajiaco_n_encuestas)
}

parameters {
  simplex[5] theta;                                // proporciones (suman 1)
}

model {
  theta ~ dirichlet(rep_vector(2.0, 5));           // prior Dirichlet
  ajiaco_priors ~ multinomial(theta);              // likelihood
}

generated quantities {
  array[5] int pred = multinomial_rng(theta, ajiaco_n_encuestas);
}
