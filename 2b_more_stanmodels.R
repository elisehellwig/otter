
library(rstan)
datapath <- '/Users/echellwig/Drive/OtherPeople/otterData'


DPfixed <- '
data{
    int<lower=1> N; //number of data points
    real declineP[N]; //dependent variable, likelihood of decline
    real Latitude[N]; //predictor variable, latitude
}
parameters{
    vector[2] beta;
    real <lower=0> sigma;
}  
model {
    real mu;
    beta[1] ~ normal(0, 10);
    beta[2] ~ normal(0, 2);
    for (i in 1:N) {
        mu = beta[1] + beta[2] * Latitude[i];
        declineP[i] ~ normal(mu, sigma);
    }
}
'
saveRDS(DPfixed, file.path(datapath, 'models/DPfixedFX.RDS'))

BETAfixedintercept <- '
data{
    int<lower=1> N; //number of data points
    real BETA[N]; //dependent variable, rate of change of pop
}
parameters{
    real beta;
    real <lower=0> sigma;
}  
model {
    real mu;
    beta ~ normal(0, 10);
    for (i in 1:N) {
        mu = beta;
        BETA[i] ~ normal(mu, sigma);
    }
}
generated quantities {
    vector[N] loglik;
    for (i in 1:N) {
        loglik[i] = normal_lpdf(BETA[i] | beta, sigma);
    }
}
'
saveRDS(BETAfixedintercept, file.path(datapath, 'models/BETAIntercept.RDS'))


BETAfixed <- '
data{
    int<lower=1> N; //number of data points
    real BETA[N]; //dependent variable, rate of change of pop
    real Latitude[N]; //predictor variable, latitude
}
parameters{
    vector[2] beta;
    real <lower=0> sigma;
}  
model {
    real mu;
    beta[1] ~ normal(0, 10);
    beta[2] ~ normal(0, 2);
    for (i in 1:N) {
        mu = beta[1] + beta[2] * Latitude[i];
        BETA[i] ~ normal(mu, sigma);
    }
}
generated quantities {
    vector[N] loglik;
    for (i in 1:N) {
        loglik[i] = normal_lpdf(BETA[i] | beta[1] + beta[2] * Latitude[i], sigma);
    }
}
'
saveRDS(BETAfixed, file.path(datapath, 'models/BETAfixedFX.RDS'))


alphafixed <- '
data{
    int<lower=1> N; //number of data points
    real alpha[N]; //dependent variable, initial population
    real Latitude[N]; //predictor variable, latitude
}
parameters{
    vector[2] beta;
    real <lower=0> sigma;
}  
model {
    real mu;
    beta[1] ~ normal(0, 10);
    beta[2] ~ normal(0, 2);
    for (i in 1:N) {
        mu = beta[1] + beta[2] * Latitude[i];
        alpha[i] ~ normal(mu, sigma);
    }
}
generated quantities {
    vector[N] loglik;
    for (i in 1:N) {
        loglik[i] = normal_lpdf(alpha[i] | beta[1] + beta[2] * Latitude[i], sigma);
    }
}
'
saveRDS(alphafixed, file.path(datapath, 'models/alphafixedFX.RDS'))


alphafixedintercept <- '
data{
    int<lower=1> N; //number of data points
    real alpha[N]; //dependent variable, initial population
}
parameters{
    real beta;
    real <lower=0> sigma;
}  
model {
    real mu;
    beta ~ normal(0, 10);
    for (i in 1:N) {
        mu = beta;
        alpha[i] ~ normal(mu, sigma);
    }
}
generated quantities {
    vector[N] loglik;
    for (i in 1:N) {
        loglik[i] = normal_lpdf(alpha[i] | beta, sigma);
    }
}
'
saveRDS(alphafixedintercept, file.path(datapath, 'models/alphaIntercept.RDS'))


DPbetareg <- '
data{
    int<lower=1> N; //number of data points
    int<lower=1> K; //number of predictors
    vector<lower=0, upper=1>[N] declineP; //response, likelihood of decline
    matrix[N,K] X; // predictor matrix
}
parameters{
    vector[K] beta;
    real<lower=0> phi;
}  
model {
    //Model calculations
    vector[N] Xbeta; //linear predictor
    vector[N] mu; //transformed linear predictor
    vector[N] A; // parameter for beta dist
    vector[N] B; //parameter for beta dist

    Xbeta = X * beta;
    for (i in 1:N) {
        mu[i] = inv_logit(Xbeta[i]);
    }

    A = mu * phi;
    B = (1.0 - mu) * phi;


    //priors
    beta ~ normal(0,1);
    phi ~ cauchy(0,5);

    //likelihood
    declineP ~ beta(A,B);
}
generated quantities {
    //Model calculations
    vector[N] Xbeta; //linear predictor
    vector[N] mu; //transformed linear predictor
    vector[N] A; // parameter for beta dist
    vector[N] B; //parameter for beta dist
    vector[N] log_lik;

    Xbeta = X * beta;
    for (i in 1:N) {
        mu[i] = inv_logit(Xbeta[i]);
    }

    A = mu * phi;
    B = (1.0 - mu) * phi;
    for (i in 1:N) {
        log_lik[i] = beta_lpdf(declineP[i] | A, B);
    }
}
'
saveRDS(DPbetareg, file.path(datapath, 'models/DPbetaregFX.RDS'))


DPbetaInt <- '
data{
    int<lower=1> N; //number of data points
    int<lower=1> K; //number of predictors
    vector<lower=0, upper=1>[N] declineP; //response, likelihood of decline
    matrix[N,K] X; // predictor matrix
}
parameters{
    vector[K] beta;
    real<lower=0> phi;
}  
model {
    //Model calculations
    vector[N] Xbeta; //linear predictor
    vector[N] mu; //transformed linear predictor
    vector[N] A; // parameter for beta dist
    vector[N] B; //parameter for beta dist

    Xbeta = X * beta;
    for (i in 1:N) {
        mu[i] = inv_logit(Xbeta[i]);
    }

    A = mu * phi;
    B = (1.0 - mu) * phi;

    //priors
    beta ~ normal(0,1);
    phi ~ cauchy(0,5);

    //likelihood
    declineP ~ beta(A,B);
}
generated quantities {

        //Model calculations
    vector[N] Xbeta; //linear predictor
    vector[N] mu; //transformed linear predictor
    vector[N] A; // parameter for beta dist
    vector[N] B; //parameter for beta dist
    vector[N] log_lik;


    Xbeta = X * beta;
    for (i in 1:N) {
        mu[i] = inv_logit(Xbeta[i]);
    }

    A = mu * phi;
    B = (1.0 - mu) * phi;
    for (i in 1:N) {
        log_lik[i] = beta_lpdf(declineP[i] | A[i], B[i]);
    }
}
'
saveRDS(DPbetaInt, file.path(datapath, 'models/DPbetaIntercept.RDS'))


