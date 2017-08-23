extractpar <- function(stanmodel, parameter, location=NA, rows=c(1,2)) {
    
    require(rstan)
    pd <- stanmodel@par_dims[[parameter]]
    
    if (length(pd)==0) {
        parvec <- parameter
    } else if (length(pd)==1) {
        
        if (is.na(rows)) {
            rows <- 1:pd
        }
        
        if (is.na(location)) {
            parvec <- paste0(parameter, '[', rows, ']') 
        } else {
            parvec <- paste0(parameter, '[', location, ']') 
        }
        
    } else if (length(pd)==2) {
        
        if (length(setdiff(rows, 1:pd[1]) > 0)) {
            stop('You can only extract as many varying parameters as there are in the model')
        }
        
        if (is.na(location)) {
            irows <- rep(rows, each=pd[2])
            icols <- rep(1:pd[2], length(rows))
            parvec <- paste0(parameter, '[', irows, ',', icols, ']')
        } else {
            
            parvec <- paste0(parameter, '[', rows, ',', location, ']')
        }
        
    } else {
        stop('parameter dimension cannot be negative.')
    }
        
    
    ps <- extract(stanmodel, par=parvec)[[1]]
    
    return(ps)
}


extractStanPars <- function(stanmodel, parameter, column, stat=NA, 
                            columnnames=NA, location=NA) {
    require(rstan)
    
   

    
    
    return(parestimates)
}


locpost <- function(sfit, location=NA, blockid=NA, response='samples') {
    #sfit is a fitted stan model
    #location is a number from 1-14
    
    sid <- sfit@par_dims[['beta']]
    
    if (is.na(blockid)) {
        beta0pars <- 'beta'
    } else {
        beta0pars <- c('beta', blockid)
    }
    
    #print(beta0pars)
    
    beta0samples <- sapply(beta0pars, function(p) {
      extractpar(sfit, p, location, 1)  
    })
    
    #print(str(beta0samples))
    
    beta0 <- apply(beta0samples, 1, sum)
    
    
    beta1samples <- sapply(beta0pars, function(p) {
        extractpar(sfit, p, location, 2)  
    })
    
    beta1 <- apply(beta1samples, 1, sum)
    
    if (response=='samples') {
        return(data.frame(b0=beta0, b1=beta1))
    } else if (response=='function'){
        
        postfun <- function(x, fun) {
            ysamp <- beta0 + x*beta1
            y <- do.call(fun, list(ysamp))
            return(y)
        }
        
        return(postfun)
    }
    
    
}

declinerisk <- function(sfit, location, postfun) {
    
    postdf <- do.call(postfun, list(sfit, location)) 
    
    dprob <- mean(postdf$b1<0)
    
    return(dprob)
    
}


