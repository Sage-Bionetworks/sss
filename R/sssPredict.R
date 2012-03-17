## PREDICT METHOD FOR MODELS OUTPUT FROM sss
#####


## THIS ONE IS FOR INTERNAL TEST - DISPATCHES THE SAME FOR ALL RESULT TYPES
setMethod(
  f = "predict",
  signature = "sssResult",
  definition = function(object, ...){
    
    args <- list(...)
    
    if(any(names(args) == "")){
      warning("'newdata' is the only optional argument allowed - all others were ignored")
    }
    
    if( !any(names(args) == "newdata") ){
      
      return(list(trainPredictionSummary = object@trainPredictionSummary,
                  testPredictionSummary = object@testPredictionSummary))
      
    } else{
      
      newdata <- args[["newdata"]]
      if( !is.matrix(newdata) ){
        stop("newdata must be a matrix")
      }
      
      if( any(is.na(newdata)) ){
        stop("Missing values not allowed in newdata matrix")
      }
      
      if( is.null(colnames(object@model@data)) ){
        warning("original data does not have column names - predictions on newdata must have columns in same order as original data for predictions to be accurate.")
      }
      if( is.null(colnames(newdata)) ){
        warning("newdata does not have column names - must be in same order as original data for predictions to be accurate.")
      }
      
      myPred <- .sssPredict(object, newdata)
      
      ## RETURN THE WEIGHTED AVERAGE OF THE PREDICTIONS
      if( length(myPred) != 0L ){
        testPredictionSummary <- as.numeric(sapply(myPred, as.numeric) %*% matrix(object@standScore))
        return(testPredictionSummary)
      } else{
        return(numeric())
      }
      
    }
    
  }
)

setMethod(
  f = ".sssPredict",
  signature = c("sssResult", "matrix"),
  definition = function(object, newdata){
    switch(class(object@model),
           sssBinaryModel = .sssBinaryPredict(object, newdata),
           sssLinearModel = .sssLinearPredict(object, newdata),
           sssSurvivalModel = .sssSurvivalPredict(object, newdata))
  }
)

#####
## LINEAR MODELS
#####
setMethod(
  f = ".sssLinearPredict",
  signature = c("sssResult", "matrix"),
  definition = function(object, newdata){
    
    thesePreds <- colnames(object@model@data)
    
    standX <- apply(newdata, 2, function(x){
      (x-mean(x))/sd(x)
    })
    trY <- object@model@response[object@model@training==1]
    meanY <- mean(trY)
    sdY <- sd(trY)
    
    myPred <- lapply(as.list(1:length(object@nBestFits$indices)), function(i){
      if( object@nBestFits$p[[i]] == 0 ){
        b <- 0
        A <- 0
      } else{
        if( !is.null(thesePreds) ){
          idp <- thesePreds[object@nBestFits$indices[[i]]]
        } else{
          idp <- object@nBestFits$indices[[i]]
        }
        if( !all(idp %in% colnames(newdata)) )
          stop("all predictors specified in the model are not included in newdata")
        
        b <- matrix(object@nBestFits$pmean[[i]], ncol=1)
        A <- standX[, idp]
      }
      pred <- as.numeric(meanY + (A %*% b)*rep(sdY, nrow(newdata)))
      if( !is.null(rownames(newdata)) )
        names(pred) <- rownames(newdata)

      pred
    })
    
    return(myPred)
  }
)


#####
## BINARY MODELS
#####
setMethod(
  f = ".sssBinaryPredict",
  signature = c("sssResult", "matrix"),
  definition = function(object, newdata){
    
    thesePreds <- colnames(object@model@data)
    
    standX <- apply(newdata, 2, function(x){
      (x-mean(x))/sd(x)
    })
    
    myEst <- lapply(as.list(1:length(object@nBestFits$indices)), function(i){
      b <- matrix(object@nBestFits$pmode[[i]], ncol=1)
      A <- matrix(0, nrow=dim(newdata)[1], ncol=nrow(b))
      A[, 1] <- 1
      
      if( object@nBestFits$p[[i]] > 0 ){
        if( !is.null(thesePreds) ){
          idp <- thesePreds[object@nBestFits$indices[[i]]]
        } else{
          idp <- object@nBestFits$indices[[i]]
        }
        if( !all(idp %in% colnames(newdata)) )
          stop("all predictors specified in the model are not included in newdata")
        
        A[, -1] <- standX[, idp]
      }
      est <- as.numeric(A %*% b)
      if( !is.null(rownames(newdata)) )
        names(est) <- rownames(newdata)

      est
    })
    
    myPred <- lapply(myEst, function(x){
      1 / (1+exp(-1*x))
    })
    
    return(myPred)
  }
)



#####
## SURVIVAL MODELS
#####
setMethod(
  f = ".sssSurvivalPredict",
  signature = c("sssResult", "matrix"),
  definition = function(object, newdata){
    
    thesePreds <- colnames(object@model@data)
    
    standX <- apply(newdata, 2, function(x){
      (x-mean(x))/sd(x)
    })
    
    myPred <- lapply(as.list(1:length(object@nBestFits$indices)), function(i){
      b <- matrix(object@nBestFits$pmode[[i]], ncol=1)
      A <- matrix(0, nrow=dim(newdata)[1], ncol=nrow(b))
      A[, 1] <- 1
      if( object@nBestFits$p[[i]] > 0 ){
        if( !is.null(thesePreds) ){
          idp <- thesePreds[object@nBestFits$indices[[i]]]
        } else{
          idp <- object@nBestFits$indices[[i]]
        }
        if( !all(idp %in% colnames(newdata)) )
          stop("all predictors specified in the model are not included in newdata")
        
        A[, -1] <- standX[, idp]
      }
      tmpPred <- as.numeric(A %*% b)
      mu <- exp(-1*tmpPred)
      pred <- (mu*log(2))^(1/object@nBestFits$pmeanalpha[[i]])
      if( !is.null(rownames(newdata)) )
        names(pred) <- rownames(newdata)

      pred
    })
    
    return(myPred)
  }
)

