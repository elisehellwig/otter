library(rstan)
datapath <- '/Users/echellwig/Drive/OtherPeople/otterData'

##Ordinary linear regression with no grouping

linear <- '
data{
    int<lower=1> N; //number of data points
    real pop[N]; //dependent variable, otter population
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
saveRDS(linear, file.path(datapath, 'models/linear.RDS'))



linearsplit <- '
data{
    int<lower=1> N; //number of data points
    real<lower=0> pop[N]; //dependent variable, otter population
    real year[N]; //predictor variable, year
    //dummy variables for locations
    int <lower=0, upper=1> Bass[N];
    int <lower=0, upper=1> Bolinas[N] ;
    int <lower=0, upper=1> Drakes[N] ;
    int <lower=0, upper=1> Estero[N] ;
    int <lower=0, upper=1> Giacomini[N] ;
    int <lower=0, upper=1> LasGallinas[N] ;
    int <lower=0, upper=1> Madera[N] ;
    int <lower=0, upper=1> Muir[N] ;
    int <lower=0, upper=1> NTB[N];
    int <lower=0, upper=1> Peters[N] ;
    int <lower=0, upper=1> Rodeo[N] ;
    int <lower=0, upper=1> Tennessee[N] ;
}
parameters{
    vector[14] beta;
    real <lower=0> sigma;
}  
model {
    real mu;
    for (i in 1:N) {
        mu = beta[1] + year[i]*(beta[2] + beta[3] + beta[4] + beta[5] + beta[6] + beta[7] + beta[8] + beta[9] + beta[10] + beta[11] + beta[12] + beta[13] + beta[14]);
        pop[i] ~ normal(mu, sigma);
    }
}
'


otrlmm <- '
data{
    int<lower=1> N; //number of data points
    int P; //number of locations
    real pop[N]; //dependent variable, otter population
    real year[N]; //predictor variable, year
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
saveRDS(otrlmm, file.path(datapath, 'models/varying1location.RDS'))




### varying FX for intercept and slope on location
otrlmm2 <- '
data{
    int<lower=1> N; //number of data points
    int P; //number of locations
    real pop[N]; //dependent variable, otter population
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
saveRDS(otrlmm, file.path(datapath, 'models/varying2location.RDS'))


#############################################################
########################### pups ##############################


linearpup <- '
data{
    int<lower=1> N; //number of data points
    real pups[N]; //dependent variable, otter population
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
        pups[i] ~ normal(mu, sigma);
    }
}
'
saveRDS(linearpup, file.path(datapath, 'models/puplinear.RDS'))



puprlmm <- '
data{
    int<lower=1> N; //number of data points
    int P; //number of locations
    real pups[N]; //dependent variable, otter population
    real year[N]; //predictor variable, year
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
        pups[i] ~ normal(mu, sigma);
    }
}
'
saveRDS(puprlmm, file.path(datapath, 'models/puprvarying1location.RDS'))


### varying FX for intercept and slope on location
puprlmm2 <- '
data{
    int<lower=1> N; //number of data points
    int P; //number of locations
    real pups[N]; //dependent variable, otter population
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
    pups[i] ~ normal(mu, sigma);
    }
}
'
saveRDS(puprlmm2, file.path(datapath, 'models/puprvarying2location.RDS'))




