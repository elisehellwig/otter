datapath <- '/Users/echellwig/Drive/OtherPeople/otterData'
options(stringsAsFactors = FALSE)
options(mc.cores = parallel::detectCores())
library(rstan)

otr <- read.csv(file.path(datapath, 'otterclean.csv'))


##########################################################



otrstan <- '
data{
    int<lower=1> N; //number of data points
    real<lower=0> pop[N]; //dependent variable, otter population
    real year[N]; //predictor variable, year
    int P; //number of locations
    int loc[P] <lower=1, upper=L>; //location id
}
parameters{
    vector[2] beta;
    real <lower=0> sigma;
    vector<lower=0>[2] sigma_U;
    cholesky_factor_corr[2] L_p;
    matrix[2,P] z_p;
}  
transformed parameters {
    matrix[2,P] p;
    p <- diag_pre_multiply(sigma_p, L_p) * z_p;  //loc random effects
}
model {
    real mu;
    //priors
    L_p ~ lkj_corr_cholesky(2.0);
    to_vector(z_p) ~ normal(0,1);
    //likelihood
    for (i in 1:N) {
        mu <- beta[1] + p[1,loc[i]] + (beta[2] + i[2, loc[i]]) * so[i];
        pop[i] ~ normal(mu, sigma);
    }
}
'