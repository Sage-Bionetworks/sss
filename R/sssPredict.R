## PREDICT METHOD FOR MODELS OUTPUT FROM sss
#####


## THIS ONE IS FOR INTERNAL TEST - DISPATCHES THE SAME FOR ALL RESULT TYPES
setMethod(
  f = "predict",
  signature = "sssResult",
  definition = function(object, ...){
    
    args <- list(...)
    if( any(names(args) != "newdata") ){
      warning("all named arguments other than newdata are ignored")
    }
    
    #####
    ## DISPATCH DEPENDING ON WHETHER newdata IS PASSED IN ...
    #####
    if( any(names(args) == "newdata") ){
      
      newdata <- args[["newdata"]]
      if( !is.matrix(newdata) ){
        stop("newdata must be a matrix")
      }
      if( is.null(colnames(object@sssModel@data)) ){
        warning("original data does not have column names - predictions on newdata must have columns in same order as original data for predictions to be accurate.")
      }
      if( is.null(colnames(newdata)) ){
        warning("newdata does not have column names - must be in same order as original data for predictions to be accurate.")
      }
      
      myPred <- .sssPredict(object, newdata)
      
    } else{
      
      ## IF EXTERNAL USER CALLS PREDICT, JUST PULL FROM THE RESULT OBJECT
      if( exists(deparse(substitute(object@wAvePredTest))) ){
        return(object@wAvePredTest)
      } else{
        
        ## OTHERWISE INTERNAL WE CHECK TO SEE IF THERE IS A TEST SET HELD ASIDE
        if( length(object@sssModel@training) == 0L ){
          warning("training argument not passed - no testing set to make predictions on")
          return(numeric())
        }
        if( all(object@sssModel@training == 1) ){
          warning("all values of training are 1 - not testing set to make predictions on")
          return(numeric())
        }
        
        ## RUN PREDICTIONS ON THE TEST SET
        #testX <- object@sssModel@data[object@sssModel@training==0, ]
        #myPred <- .sssPredict(object, testX)
        
        ## THIS SHOULD ALWAYS BE POPULATED IN THIS SCENARIO
        myPred <- object@sssModelNbest$predTest
      }
            
    }
    
    ## RETURN THE WEIGHTED AVERAGE OF THE PREDICTIONS
    if( length(myPred) != 0L ){
      wAvePredTest <- as.numeric(sapply(myPred, as.numeric) %*% matrix(object@standScore))
      return(wAvePredTest)
    } else{
      return(numeric())
    }
    
  }
)


#####
## LINEAR MODELS
#####
setMethod(
  f = ".sssPredict",
  signature = c("sssLinearResult", "matrix"),
  definition = function(object, newdata){
    
    thesePreds <- colnames(object@sssModel@data)
    
    standX <- apply(newdata, 2, function(x){
      (x-mean(x))/sd(x)
    })
    trY <- object@sssModel@response[object@sssModel@training==1]
    meanY <- mean(trY)
    sdY <- sd(trY)
    
    myPred <- lapply(as.list(1:length(object@sssModelNbest$indices)), function(i){
      if( object@sssModelNbest$p[[i]] == 0 ){
        b <- 0
        A <- 0
      } else{
        if( !is.null(thesePreds) ){
          idp <- thesePreds[object@sssModelNbest$indices[[i]]]
        } else{
          idp <- object@sssModelNbest$indices[[i]]
        }
        if( !all(idp %in% colnames(newdata)) )
          stop("all predictors specified in the model are not included in newdata")
        
        b <- matrix(object@sssModelNbest$pmean[[i]], ncol=1)
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
  f = ".sssPredict",
  signature = c("sssBinaryResult", "matrix"),
  definition = function(object, newdata){
    
    thesePreds <- colnames(object@sssModel@data)
    
    standX <- apply(newdata, 2, function(x){
      (x-mean(x))/sd(x)
    })
    
    myEst <- lapply(as.list(1:length(object@sssModelNbest$indices)), function(i){
      b <- matrix(object@sssModelNbest$pmode[[i]], ncol=1)
      A <- matrix(0, nrow=dim(newdata)[1], ncol=nrow(b))
      A[, 1] <- 1
      
      if( object@sssModelNbest$p[[i]] > 0 ){
        if( !is.null(thesePreds) ){
          idp <- thesePreds[object@sssModelNbest$indices[[i]]]
        } else{
          idp <- object@sssModelNbest$indices[[i]]
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
  f = ".sssPredict",
  signature = c("sssSurvivalResult", "matrix"),
  definition = function(object, newdata){
    
    thesePreds <- colnames(object@sssModel@data)
    
    standX <- apply(newdata, 2, function(x){
      (x-mean(x))/sd(x)
    })
    
    myPred <- lapply(as.list(1:length(object@sssModelNbest$indices)), function(i){
      b <- matrix(object@sssModelNbest$pmode[[i]], ncol=1)
      A <- matrix(0, nrow=dim(newdata)[1], ncol=nrow(b))
      A[, 1] <- 1
      if( object@sssModelNbest$p[[i]] > 0 ){
        if( !is.null(thesePreds) ){
          idp <- thesePreds[object@sssModelNbest$indices[[i]]]
        } else{
          idp <- object@sssModelNbest$indices[[i]]
        }
        if( !all(idp %in% colnames(newdata)) )
          stop("all predictors specified in the model are not included in newdata")
        
        A[, -1] <- standX[, idp]
      }
      tmpPred <- as.numeric(A %*% b)
      mu <- exp(-1*tmpPred)
      pred <- (mu*log(2))^(1/object@sssModelNbest$pmeanalpha[[i]])
      if( !is.null(rownames(newdata)) )
        names(pred) <- rownames(newdata)

      pred
    })
    
    return(myPred)
  }
)

