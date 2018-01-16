extractpar <- function(stanmodel, parameter, location=NA, rows=c(1,2)) {
    
    require(rstan)
    pd <- stanmodel@par_dims[[parameter]]
    
    if (length(pd)==0) {
        parvec <- parameter
    } else if (length(pd)==1) {
        
        if (is.na(rows[1])) {
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

createformula <- function(data, responsevar, predictornames=NA,
                          predictorstart=NA, predictorend=NA){
    
    if (!is.na(predictornames[1])) {
        varnames <- predictornames
        
    } else if (is.na(predictorend)) {
        if (is.na(predictorstart)) {
            responseindex <- which(names(data)==reponsevar)
            allindices <- 1:dim(data)[2]
            varindices <- allindices[allindices!=responseindex]
            
        } else {
            varindices <- predictorstart:dim(data)[2]
        }
        
    } else {
        varindices <- predictorstart:predictorend
        varnames <- names(data)[varindices]
    }
    
    fmlachr <- paste(responsevar,'~', paste(varnames, collapse=' + '))
    fmla <- as.formula(fmlachr)
    
    return(fmla)
}


extractrf <- function(x, response, predictors, element='R2') {
    require(randomForest)
    
    if ('randomForest' %in% class(x)) {
        rfmod <- x
    } else if (is.data.frame(x)) {
        if (is.numeric(predictors)) {
            fmla <- createformula(x, response, predictorstart = predictors[1],
                                  predictorend=predictors[2])
        } else if (is.character(predictors)) {
            fmla <- createformula(x, response, predictornames=predictors)
        } else {
            stop('Predictors must be either of the class numeric or character.')
        }
        

        rfmod <- randomForest(fmla, x)
    } else {
        stop('x must either be a randomForest object or a data.frame')
    }
    
    
    
    if (element=='R2') {
        value <- mean(rfmod$rsq)
    } else if (element=='rmse') {
        value <- sqrt(mean(rfmod$mse))
    } else if (element=='model') {
        value <- rfmod
    } else if (element=='importance_raw'){
        value <- importance(rfmod)
    } else if (element=='importance_normal') {
        rawimp <- importance(rfmod)
        value <- rawimp/max(rawimp)
    }
    
    return(value)
}

processFixedMod <- function(model, predictor, response, pname='lat', 
                            rname='alpha') {
    
    postpars <- extract(model)
    
    post <- sapply(1:2, function(column) postpars$beta[,column])
    
    pars <- apply(post, 2, mean)
    fitvals <- pars[1] + predictor*pars[2]
    resids <- response - fitvals
    
    df <- data.frame(predictor=predictor,
                     response=response,
                     fit=fitvals,
                     res=resids)
    
    names(df) <- c(pname, rname, paste0(rname,'fit'), paste0(rname,'res'))
    
   return(df)
    
}

autocor <- function(spdata, var, sp_list, permutations, res=FALSE,
                    alt='greater', return='all') {
    require(spdep)
    
    if (res) {
        var <- paste0(var,'res')
    }
    
    numvec <- spdata@data[,var]
    
    mI <- moran.mc(numvec, sp_list, permutations, alternative=alt)
    
    if (return=='p-value') {
        value <- mI$p.value
    } else if (return=='statistic') {
        value <- mI$statistic
        attributes(value) <- NULL
    } else if (return=='all') {
        value <- mI
    } else {
        stop('return must be either p-value, statistic or all.')
    }
    
    return(value)
}

modfit <- function(stanmod) {
    ab <- get_posterior_mean(stanmod)[1:2]
    attributes(ab) <- NULL
    
    fitfun <- function(x) {
        y <- ab[1] + ab[2]*x
        return(y)
    }
    
    return(fitfun)
} 
    

formulastring <- function(mod, yvar='y', xvar='x', sf=2, greek=FALSE) {
    
    require(rstan)
    
    AB <- get_posterior_mean(mod)[1:2]
    attributes(AB) <- NULL
    
    a <- round(AB[1], sf)
    b <- round(AB[2], sf)
    
    if (greek) {
        xvar <- paste0('(', xvar,')')
        fmla <- paste(yvar, '==', b, xvar, '+', a)
    } else {
        fmla <- paste0(yvar, ' = ', b, xvar, ' + ', a)
    }
    
    return(fmla)
    
}


