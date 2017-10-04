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
    attributes(ps) <- NULL
    
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
    
    if (is.na(blockid)) {
        beta0samples <- extractpar(sfit, 'beta', rows=1)
        beta1samples <- extractpar(sfit, 'beta', rows=2)
        
        
    } else {

        beta0samp <- data.frame(f=extractpar(sfit, 'beta', rows=1),
                                v=extractpar(sfit, blockid, location, rows=1))
        beta1samp <- data.frame(f=extractpar(sfit, 'beta', rows=2),
                                v=extractpar(sfit, blockid, location, rows=2))
    }
    
    #print(beta0pars)
    
    #print(str(beta0samples))
    
    beta0 <- apply(beta0samp, 1, sum)
    beta1 <- apply(beta1samp, 1, sum)
    
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

same <- function(x) {
    return(x)
}



predictpop <- function(x, funlist, sumfun, locID=1, yearID=2, decimalpts=1) {
    
    newvar <- sapply(1:nrow(x), function(n) {
        i <- df[n,locID]
        j <- df[n,yearID]
        round(funlist[[i]](j, sumfun), decimalpts)
    })
    
    attributes(newvar)$out.attrs <- NULL
    
    
    return(newvar)
}


convertbinary <- function(vector) {
    vals <- as.character(unique(vector))
    df <- sapply(vals, function(v) ifelse(vector==v, 1, 0))
    return(df)
}

createformula <- function(data, responsevar, predictorstart=NA, 
                          predictorend=NA){
    
    if (is.na(predictorend)) {
        if (is.na(predictorstart)) {
            responseindex <- which(names(data)==reponsevar)
            allindices <- 1:dim(data)[2]
            varindices <- allindices[allindices!=responseindex]
            
        } else {
            varindices <- predictorstart:dim(data)[2]
        }
        
    } else {
        varindices <- predictorstart:predictorend
    }
    
    fmlachr <- paste(responsevar,'~', paste(names(data)[varindices], 
                              collapse=' + '))
    fmla <- as.formula(fmlachr)
    
    return(fmla)
}








