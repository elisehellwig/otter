library(rstan)


otrlm <- '
data{
    int<lower=1> N; //number of data points
    int year[N]; //independent variable, year
    real pop[N]; //dependent variable, otter population

}
parameters{
    vector[2] beta; // intercept and slope
    real <lower=0> sigma; //error
}  
model {
    real mu;
    //likelihood
    for (i in 1:N) {
        mu = beta[1] + beta[2] * year[i];
        pop[i] ~ normal(mu, sigma);
    }
}
'
saveRDS(otrlm, file.path(datapath, 'fixedlinear.RDS'))


otrlmm <- '
data{
    int<lower=1> N; //number of data points
    int P; //number of locations
    real pop[N]; //dependent variable, otter population
    int <lower=1, upper=P> loc[N]; //location id

}
parameters{
    real beta;
    real <lower=0> sigma;
    vector[P] p; //location intercepts 
    real <lower=0> sigma_p;
}  
model {
    real mu;
    //priors
    p ~ normal(0, sigma_p) ; 
    //likelihood
    for (i in 1:N) {
        mu = beta + p[loc[i]];
        pop[i] ~ normal(mu, sigma);
    }
}
'
saveRDS(otrlmm, file.path(datapath, 'varying1location.RDS'))




otrGlm <- '
data{
    int<lower=1> N; //number of data points
    int<lower=0> pop[N]; //dependent variable, initial population
}
parameters{ 
    vector[] beta;
}  
model {

    real mu[N];
    real lambda[N];
    for (i in 1:N) {
        mu[i] = beta[1];
        lambda[i] = exp(mu[i]);
    }

    //priors
    beta ~ normal(0,1);

    //likelihood
    alpha ~ poisson(lambda);
}
'
saveRDS(otrGlm, file.path(datapath, 'fixedPOIS.RDS'))


otrGlmm2 <- '
data{
    int<lower=1> N; //number of data points
    int P; //number of locations
    int pop[N]; //dependent variable, otter population
    real year[N]; //predictor variable, year
    int <lower=1, upper=P> loc[N]; //location id
}
parameters{
    vector[2] beta;
    vector<lower=0>[2] sigma_u;
    cholesky_factor_corr[2] L_u;
    matrix[2,P] z_u;
}  
transformed parameters {
    matrix[2,P] u;
    u = diag_pre_multiply(sigma_u, L_u) * z_u;  //loc random effects

}
model {
    real mu;
    real lambda;
    //priors
    L_u ~ lkj_corr_cholesky(2.0);
    to_vector(z_u) ~ normal(0,1);
    beta ~ normal(0,1);
    //likelihood
    for (i in 1:N) {
        mu = beta[1] + u[1,loc[i]] + (beta[2] + u[2, loc[i]]) * year[i];
        lambda = exp(mu);
        pop[i] ~ poisson(lambda);
    }
}
'
saveRDS(otrGlmm, file.path(datapath, 'varying2locationPOIS.RDS'))




### parameter analysis #########
