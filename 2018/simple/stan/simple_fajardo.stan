data{
    int<lower=1> N;
    int<lower=1> N_encuestadora;
    real int_voto[N];
    int encuestadora[N];
    real m_error[N];
    real tipo[N];
    int muestra_int_voto[N];
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
    real<lower=0> s;
}
model{
    vector[N] m;
    s ~ cauchy( 0 , 5 );
    a5 ~ normal( 0 , 10 );
    a4 ~ normal( 0 , 10 );
    a3 ~ normal( 0 , 10 );
    a2 ~ normal( 0 , 10 );
    s_enc ~ cauchy( 0 , 5 );
    a_enc ~ normal( 0 , 10 );
    a_ ~ normal( a_enc , s_enc );
    a1 ~ normal( 13 , 3 ); 
    for ( i in 1:N ) {
        m[i] = a1 + a_[encuestadora[i]] + a2 * muestra_int_voto[i] + a3 * m_error[i] +      a4 * dd[i] + a5 * tipo[i];
    }
    int_voto ~ normal( m , s );
}
generated quantities{
    vector[N] m;
    real dev;
    dev = 0;
    for ( i in 1:N ) {
        m[i] = a1 + a_[encuestadora[i]] + a2 * muestra_int_voto[i] + a3 * m_error[i] +      a4 * dd[i] + a5 * tipo[i];
    }
    dev = dev + (-2)*normal_lpdf( int_voto | m , s );
}
