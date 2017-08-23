extractpar <- function(stanmodel, parameter, column, stat=NA, 
                       columnnames=NA, location=NA, rows=c(1,2)) {
    
    require(rstan)
    pd <- sfit@par_dims[[parameter]]
    
    if (length(pd)==0) {
        parvec <- parameter
    } else if (length(pd)==1) {
        
        if (is.na(location)) {
            parvec <- paste0(parameter, '[', 1:pd, ']') 
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
        
    
    ps <- extract(stanmodel, par=parvec)
    
    return(ps)
}


extractStanPars <- function(stanmodel, parameter, column, stat=NA, 
                            columnnames=NA, location=NA) {
    require(rstan)
    
   

    
    
    return(parestimates)
}

postxy <- function(samples) {
    
}




locpost <- function(sfit, location=NA, cholesky=FALSE, par_name='beta', 
                    blockid=NA) {
    #sfit is a fitted stan model
    #location is a number from 1-14
    
    pd <- sfit@par_dims
    
    if (is.na(blockid)) {
        beta0samples <- extract(sfit, pars=c(fixedpars[1], varyingpars[1]))
            
    } else {
        if (is.na(location)) {
            poi <- c(par_name, blockid)
    
        } else {
            
            if (pd[[blockid]][1]==1) {
                poi <- c('beta', paste0(blockid,'[', location, ']'))
                
            } else if (pd[[blockid]][1]==2) {
                poi <- c('beta', paste0(blockid,'[', 1:2, ',' , location, ']'))
            } else {
                stop('There can only be 1 or 2 varying parameters at this time')
            }
            
        }
            
    }
    
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


