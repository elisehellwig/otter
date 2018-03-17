rmsd <- function(x, y, na.rm=FALSE) {

    rmsd <- sqrt(sum((x-y)^2)/length(x)) #calculating the RMSD
    
    return(rmsd)
}



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


extractrf <- function(x, response, predictors, element='R2', imp=FALSE) {
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
        

        rfmod <- randomForest(fmla, x, importance=imp)
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

modfit <- function(stanmod, betareg=FALSE) {
    abc <- get_posterior_mean(stanmod)[1:3]
    attributes(abc) <- NULL
    
    
    if (betareg) {
        
        fitfun <- function(x) {
            mat <- matrix(c(rep(1, length(x)), x), ncol=2)
            mc <- mat %*% abc[1:2]
            mu <- inv_logit(mc)
            
            return(mu)
        }
        
        
    } else {
        ab <- abc[1:2]
        fitfun <- function(x) {
            y <- ab[1] + ab[2]*x
            return(y)
        }
    }
    
    return(fitfun)
} 

BRpostlink <- function(dat, coefs, phi, noPredictor=FALSE, returnMU=FALSE) {
    
    if (noPredictor) {
        mc <- inv_logit(coefs)
        
    } else {
        mat <- matrix(c(rep(1, length(dat)), dat), ncol=2)
        mc <- mat %*% coefs
    }
    
    mu <- inv_logit(mc)
    
    if (returnMU) {
        return(mu)
    } else {
        A <- mu*phi
        B <- (1-mu)*phi
        
        fits <- beta(A, B)
        return(fits) 
    }
    
    
}

processFixedMod <- function(model, predictor, response, pname='lat', 
                            rname='alpha', rmse=FALSE, betareg=FALSE) {
    
    #print(predictor)
    postpars <- get_posterior_mean(model)
    postpars <- unname(postpars)
    
    
    if (is.na(predictor[1])) {
        parIDs <- 1
        
    } else {
        parIDs <- 1:2
        
    }
    
    if (betareg) {
        parIDs <- c(parIDs, max(parIDs)+1)
    }
    
    pars <- postpars[parIDs]
    
    if (betareg & (!is.na(predictor[1]))) {
        fitvals <- BRpostlink(predictor, pars[1:2], pars[3], returnMU = TRUE)
        
    } else if (betareg & is.na(predictor[1])) {
        fits <- BRpostlink(NA, pars[1], pars[2], noPredictor = TRUE,
                           returnMU = TRUE)
        fitvals <- rep(fits, length(response))
        
    } else {
        if (length(pars)>1) {
            fitvals <- pars[1] + predictor*pars[2]
        } else {
            fitvals <- rep(pars[1], length(response))
        }
    }
    
    
    
    resids <- response - fitvals
    
    
    
    if (rmse) {
        error <- rmsd(fitvals, response)
        return(error)
        
    } else {
        df <- data.frame(predictor=predictor,
                         response=response,
                         fit=fitvals,
                         res=resids)
        names(df) <- c(pname, rname, paste0(rname,'fit'), paste0(rname,'res'))
        
        return(df)
    }
    
   
    
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


formulastring <- function(mod, yvar='y', xvar='x', sf=2, greek=FALSE, 
                          beta=FALSE) {
    
    require(rstan)
    
    AB <- get_posterior_mean(mod)[1:2]
    attributes(AB) <- NULL
    
    a <- round(AB[1], sf)
    b <- round(AB[2], sf)
    
    if (beta) {
        xvar <- paste0('(', xvar,')')
        bneg <- b*(-1)
            
        if (a<0) {
            mufmla <- paste0(bneg, xvar, '+', abs(a))
        } else {
            mufmla <- paste0(bneg, xvar, '-', abs(a))
        }
        
        fmla <- paste0(yvar, '==', 'frac(1, 1+e^{',
                       mufmla, '})')
    } else {
        if (greek) {
            xvar <- paste0('(', xvar,')')
            fmla <- paste(yvar, '==', b, xvar, '+', a)
        } else {
            fmla <- paste0(yvar, ' = ', b, xvar, ' + ', a)
        }
        
    }
    
    return(fmla)
    
    
}



rsquared <- function(obs, fit) {
    
    sstot <- sum((obs-mean(obs))^2)
    ssres <- sum((fit-obs)^2)
    
    r2 <- 1-(ssres/sstot)
    
    return(r2)
    
}

crossval <- function(stanmodel, predictor, response, data, k, 
                     nchain=1, nwarm=5000, nsamp=25000, delt=0.999, 
                     treedepth=15, palt = 'Latitude', noPredictor=FALSE,
                     betareg=FALSE) {
    
    rmseV <- rep(NA, k)

    if (is.na(palt)) {
        palt <- predictor
    }
    
    for (i in 1:k) {
        train <- data[data$fold!=i, ]
        test <- data[data$fold==i, ]
        
        if (betareg & noPredictor) {
            Latmat <- matrix(rep(1, nrow(train)), ncol=1)
            dlist <- list(N=nrow(train), K=ncol(Latmat),
                          declineP=train$declineP, X=Latmat)
            
        } else if (betareg & (!noPredictor)) {
            Latmat <- matrix(c(rep(1, nrow(train)), train$latitude), ncol=2)
            dlist <- list(N=nrow(train), K=ncol(Latmat), 
                          declineP=train$declineP, X=Latmat)
            
        } else {
            dlist <- list(train[, predictor], train[,response], nrow(train))
            names(dlist) <- c(palt, response, 'N')
        }
        
        
    
        
        mod <- stan(model_code = stanmodel, data=dlist, iter=nsamp,
                          warmup = nwarm, chains=nchain,
                          control=list(adapt_delta = delt,
                                       max_treedepth=treedepth))
        
        #print(mod)
        
        if (noPredictor) {
            
            if (betareg) {
                rmseV[i] <- processFixedMod(mod, NA, test[,response],
                                            pname=predictor, rname=response, 
                                            rmse=TRUE, betareg=TRUE)
            } else {
                rmseV[i] <- processFixedMod(mod, NA, test[,response],
                                            pname=predictor, rname=response, 
                                            rmse=TRUE)
            }
            
            
        } else {
            
            if (betareg) {
                rmseV[i] <- processFixedMod(mod, test[, predictor], 
                                            test[,response], pname=predictor,
                                            rname=response, rmse=TRUE,
                                            betareg=TRUE)
                
            } else {
                rmseV[i] <- processFixedMod(mod, test[, predictor], 
                                            test[,response], pname=predictor,
                                            rname=response, rmse=TRUE)
            }
            
        }
        
        
    }
    
    return(mean(rmseV))
    
}





