data{
    int<lower=1> N;
    int<lower=1> N_encuestadora;
    real int_voto[N];
    int encuestadora[N];
    real muestra_int_voto[N];
    real m_error[N];
    real dd[N];
    real tipo[N];
}
parameters{
    vector[N_encuestadora] b4_encuestadora;
    vector[N_encuestadora] b3_encuestadora;
    vector[N_encuestadora] b2_encuestadora;
    vector[N_encuestadora] b1_encuestadora;
    vector[N_encuestadora] a_encuestadora;
    real a;
    real b1;
    real b2;
    real b3;
    real b4;
    vector<lower=0>[5] s_encuestadora;
    real<lower=0> s;
    corr_matrix[5] Rho;
}
transformed parameters{
    vector[5] v_a_encuestadorab1_encuestadorab2_encuestadorab3_encuestadorab4_encuestadora[N_encuestadora];
    vector[5] Mu_ab1b2b3b4;
    cov_matrix[5] SRS_s_encuestadoraRho;
    for ( j in 1:N_encuestadora ) {
        v_a_encuestadorab1_encuestadorab2_encuestadorab3_encuestadorab4_encuestadora[j,1] = a_encuestadora[j];
        v_a_encuestadorab1_encuestadorab2_encuestadorab3_encuestadorab4_encuestadora[j,2] = b1_encuestadora[j];
        v_a_encuestadorab1_encuestadorab2_encuestadorab3_encuestadorab4_encuestadora[j,3] = b2_encuestadora[j];
        v_a_encuestadorab1_encuestadorab2_encuestadorab3_encuestadorab4_encuestadora[j,4] = b3_encuestadora[j];
        v_a_encuestadorab1_encuestadorab2_encuestadorab3_encuestadorab4_encuestadora[j,5] = b4_encuestadora[j];
    }
    for ( j in 1:5 ) {
        Mu_ab1b2b3b4[1] = a;
        Mu_ab1b2b3b4[2] = b1;
        Mu_ab1b2b3b4[3] = b2;
        Mu_ab1b2b3b4[4] = b3;
        Mu_ab1b2b3b4[5] = b4;
    }
    SRS_s_encuestadoraRho = quad_form_diag(Rho,s_encuestadora);
}
model{
    vector[N] m;
    Rho ~ lkj_corr( 2 );
    s ~ cauchy( 0 , 5 );
    s_encuestadora ~ cauchy( 0 , 5 );
    b4 ~ normal( 0 , 10 );
    b3 ~ normal( 0 , 10 );
    b2 ~ normal( 0 , 10 );
    b1 ~ normal( 0 , 10 );
    a ~ normal( 7 , 2 );
    v_a_encuestadorab1_encuestadorab2_encuestadorab3_encuestadorab4_encuestadora ~ multi_normal( Mu_ab1b2b3b4 , SRS_s_encuestadoraRho );
    for ( i in 1:N ) {
        m[i] = a_encuestadora[encuestadora[i]] + b1_encuestadora[encuestadora[i]] *      muestra_int_voto[i] + b2_encuestadora[encuestadora[i]] * m_error[i] +      b3_encuestadora[encuestadora[i]] * dd[i] + b4_encuestadora[encuestadora[i]] *      tipo[i];
    }
    int_voto ~ normal( m , s );
}
generated quantities{
    vector[N] m;
    real dev;
    dev = 0;
    for ( i in 1:N ) {
        m[i] = a_encuestadora[encuestadora[i]] + b1_encuestadora[encuestadora[i]] *      muestra_int_voto[i] + b2_encuestadora[encuestadora[i]] * m_error[i] +      b3_encuestadora[encuestadora[i]] * dd[i] + b4_encuestadora[encuestadora[i]] *      tipo[i];
    }
    dev = dev + (-2)*normal_lpdf( int_voto | m , s );
}
