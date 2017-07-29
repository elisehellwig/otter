library(rstan)
datapath <- '/Users/echellwig/Drive/OtherPeople/otterData'

##Ordinary linear regression with no grouping

linear <- '
data{
    int<lower=1> N; //number of data points
    real<lower=0> pop[N]; //dependent variable, otter population
    real year[N]; //predictor variable, year
}
parameters{
    vector[2] beta;
    real <lower=0> sigma;
}  
model {
    real mu;
    for (i in 1:N) {
        mu = beta[1] + beta[2] * year[i];
        pop[i] ~ normal(mu, sigma);
    }
}
'



### varying FX on location
otrlmm <- '
data{
    int<lower=1> N; //number of data points
    int P; //number of locations
    real<lower=0> pop[N]; //dependent variable, otter population
    real year[N]; //predictor variable, year
    int <lower=1, upper=P> loc[N]; //location id

}
parameters{
    vector[2] beta;
    real <lower=0> sigma;
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
    //priors
    L_u ~ lkj_corr_cholesky(2.0);
    to_vector(z_u) ~ normal(0,1);
    //likelihood
    for (i in 1:N) {
        mu = beta[1] + u[1,loc[i]] + (beta[2] + u[2, loc[i]]) * year[i];
        pop[i] ~ normal(mu, sigma);
    }
}
'



### varying FX on region vs. habitat?





