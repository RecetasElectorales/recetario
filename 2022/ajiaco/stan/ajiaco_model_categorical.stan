data {
  int<lower = 1> ajiaco_n_encuestas; //Numero de encuestas
  int<lower = 1, upper = 6> ajiaco_priors[ajiaco_n_encuestas];
}
parameters {
  simplex[6] theta;
}
model {
  target += dirichlet_lpdf(theta | rep_vector(2, 6)); // Prior Dirichlet
  
for(n in 1:ajiaco_n_encuestas)
    target += categorical_lpmf(ajiaco_priors[n] | theta);
}
generated quantities{
  
  int pred[ajiaco_n_encuestas];
  
  for(n in 1:ajiaco_n_encuestas)
  
    pred[n] = categorical_rng(theta);
}
