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
      if( exists(deparse(substitute(object@sssModelNbest$predTest))) ){
        myPred <- object@sssModelNbest$predTest
      } else{
        
        ## OTHERWISE INTERNAL WE CHECK TO SEE IF THERE IS A TEST SET HELD ASIDE
        if( length(object@sssModel@training) == 0L ){
          return(list(pred=NA, pFit=NA))
        }
        if( all(object@sssModel@training == 1) ){
          return(list(pred=NA, pFit=NA))
        }
        
        ## RUN PREDICTIONS ON THE TEST SET
        testX <- object@sssModel@data[object@sssModel@training==0, ]
        myPred <- .sssPredict(object, testX)
      }
            
    }
    return(myPred)
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
        A <- newdata[, idp]
      }
      pred <- meanY + (A %*% b)*rep(sdY, nrow(newdata))
      names(pred) <- rownames(newdata)
      return(pred)
    })
    
    return(list(pred=myPred))
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
    
    myPred <- lapply(as.list(1:length(object@sssModelNbest$indices)), function(i){
      b <- matrix(object@sssModelNbest$pmode[[i]], ncol=1)
      A <- matrix(0, nrow=dim(newdata)[1], ncol=length(b))
      A[, 1] <- 1
      
      if( object@sssModelNbest$p[[i]] > 0 ){
        if( !is.null(thesePreds) ){
          idp <- thesePreds[object@sssModelNbest$indices[[i]]]
        } else{
          idp <- object@sssModelNbest$indices[[i]]
        }
        if( !all(idp %in% colnames(newdata)) )
          stop("all predictors specified in the model are not included in newdata")
        
        A[, -1] <- newdata[, idp]
      }
      pred <- A %*% b
      names(pred) <- rownames(newdata)
      pFit <- 1 / (1+exp(-1*pred))
      names(pFit) <- rownames(newdata)
      return(pred)
    })
    myPFit <- lapply(myPred, function(x){
      1 / (1+exp(-1*x))
    })
    
    return(list(pred=myPred, pFit=myPFit))
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
    
    myPred <- lapply(as.list(1:length(object@sssModelNbest$indices)), function(i){
      b <- matrix(object@sssModelNbest$pmode[[i]], ncol=1)
      A <- matrix(0, nrow=dim(newdata)[1], ncol=length(b))
      A[, 1] <- 1
      if( object@sssModelNbest$p[[i]] > 0 ){
        if( !is.null(thesePreds) ){
          idp <- thesePreds[object@sssModelNbest$indices[[i]]]
        } else{
          idp <- object@sssModelNbest$indices[[i]]
        }
        if( !all(idp %in% colnames(newdata)) )
          stop("all predictors specified in the model are not included in newdata")
        
        A[, -1] <- newdata[, idp]
      }
      pred <- A %*% b
      names(pred) <- rownames(newdata)
      return(pred)
    })
    
    return(list(pred=myPred))
  }
)

