locpost <- function(sfit, location) {
    #sfit is a fitted stan model
    #location is a number from 1-13
    varyingpars <- paste0('z_u[', 1:2, ',',location,']')
    
    fixedpars <- c('beta[1]','beta[2]')
    
    beta0samples <- extract(sfit, pars=c(fixedpars[1], varyingpars[1]))
    beta1samples <- extract(sfit, pars=c(fixedpars[2], varyingpars[2]))
    
    
    beta0post <-  beta0samples[[1]] + beta0samples[[2]]
    attributes(beta0post) <- NULL
    
    
    beta1post <-  beta1samples[[1]] + beta1samples[[2]]
    attributes(beta1post) <- NULL
    
    return(data.frame(b0=beta0post, b1=beta1post))
}

declinerisk <- function(sfit, location, postfun) {
    
    postdf <- do.call(postfun, list(sfit, location)) 
    
    dprob <- mean(postdf$b1<0)
    
    return(dprob)
    
}


extractStanPars <- function(stanmodel, parameter, column, stat=NA, 
                            columnnames=NA) {
    require(rstan)
    
    samples <- as.data.frame(extract(stanmodel, pars=parameter))
    searchpattern <- paste0(parameter, '.', column, '.')
    cols <- grep(searchpattern, names(samples))
    
    selectedsamples <- samples[,cols]
    
    if (!is.na(columnnames[1])) {
        names(selectedsamples) <- columnnames
    }
    
    
    if (is.na(stat)) {
        parestimates <- selectedsamples
    } else {
        parestimates <- data.frame(location=columnnames,
                                   estimate=apply(selectedsamples, 2, stat))
    }
   
    
    return(parestimates)
}





