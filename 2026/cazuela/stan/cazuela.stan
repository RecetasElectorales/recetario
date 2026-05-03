data {
  int<lower=1> J;                     // encuestas
  int<lower=2> K;                     // candidatos
  int<lower=1> H;                     // encuestadoras
  array[J] int<lower=0> n;           // tamaño muestral
  array[J, K] int<lower=0> Y;        // conteos Y[j, k]
  vector[J] time;                     // covariable temporal (centrada)
  array[J] int<lower=1, upper=H> house; // índice de encuestadora
}

parameters {
  vector[K - 1] beta0_raw;           // intercepto (log-ratio vs candidato K)
  vector[K - 1] beta_time_raw;       // pendiente temporal (log-ratio vs K)

  // Efectos aleatorios de casa: multinivel con covarianza LKJ
  matrix[K - 1, H] z_delta;          // efectos estandarizados (para non-centered param)
  vector<lower=0>[K - 1] sigma_delta; // sd por candidato
  cholesky_factor_corr[K - 1] L_Omega; // factor Cholesky de correlación

  real<lower=0> kappa;                // concentración DM
}

transformed parameters {
  // Reconstruir efectos de casa: delta[,h] = diag(sigma) * L * z[,h]
  matrix[K - 1, H] delta;
  delta = diag_pre_multiply(sigma_delta, L_Omega) * z_delta;

  // Construir log-ratios completos (K-1 libres + referencia = 0)
  matrix[J, K] eta;
  for (j in 1:J) {
    for (k in 1:(K - 1)) {
      eta[j, k] = beta0_raw[k] + beta_time_raw[k] * time[j] + delta[k, house[j]];
    }
    eta[j, K] = 0;  // candidato de referencia
  }

  // Softmax -> simplex por encuesta
  array[J] simplex[K] p;
  for (j in 1:J) {
    p[j] = softmax(eta[j]');
  }
}

model {
  // Priors efectos fijos
  beta0_raw ~ normal(0, 2);
  beta_time_raw ~ normal(0, 1);

  // Priors multinivel para efectos de casa
  sigma_delta ~ normal(0, 1);        // half-normal en cada sd
  L_Omega ~ lkj_corr_cholesky(2);    // prior LKJ(2) - favorece correlaciones moderadas
  to_vector(z_delta) ~ std_normal();  // non-centered parametrization

  // Prior concentración
  kappa ~ lognormal(log(200), 0.7);

  // Likelihood
  for (j in 1:J) {
    Y[j] ~ dirichlet_multinomial(kappa * to_vector(p[j]));
  }
}

generated quantities {
  array[J, K] int Y_rep;
  for (j in 1:J) {
    Y_rep[j] = dirichlet_multinomial_rng(kappa * to_vector(p[j]), n[j]);
  }

  // Matriz de correlación de efectos de casa
  corr_matrix[K - 1] Omega;
  Omega = multiply_lower_tri_self_transpose(L_Omega);
}
