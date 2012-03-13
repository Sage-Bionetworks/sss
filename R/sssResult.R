## METHOD TO READ IN SUMMARY OUTPUT FROM SSS FOR DIFFERENT MODEL TYPES
#####


setMethod(
  f = ".readSummary",
  signature = "sssLinearModel",
  definition = function(object){
    
    outSum <- read.table(object@setupSpec@summaryfile, header=F, sep=" ", na.strings=c("NA", "nan", "na", "NaN"), as.is=T)
    pmax <- sqrt(ncol(outSum) - 3) - 1
    
    ## SPECIFY OUTPUT AS DESCRIBED BY HANS ET AL
    p <- as.list(as.numeric(outSum[, 1]))
    score <- as.list(as.numeric(outSum[, 2]))
    indices <- lapply(as.list(1:dim(outSum)[1]), function(i){ x <- as.numeric(outSum[i, 3:(p[[i]] + 2)])
                                                              x[!is.na(x)] })
    pmean <- lapply(as.list(1:dim(outSum)[1]), function(i){
      if(p[[i]]>0){
        x <- as.numeric(outSum[i, (p[[i]] + 3):(2*p[[i]] + 2)])
        x[!is.na(x)]
      } else{
        numeric()
      }
    })
    pvar <- lapply(as.list(1:dim(outSum)[1]), function(i){
      if(p[[i]]>0){
        x <- as.numeric(outSum[i, (2*p[[i]] + 3):(p[[i]]*p[[i]] + 2*p[[i]] + 2)])
        x[!is.na(x)]
      } else{
        numeric()
      }
    })
    residsd <- as.list(as.numeric(outSum[, pmax*pmax + 2*pmax + 3]))
    postdf <- as.list(as.numeric(outSum[, pmax*pmax + 2*pmax + 4]))
    
    ## CREATE MORE MEANINGFUL OUTPUT METRICS
    pm <- lapply(score, function(x){
      exp(x-max(unlist(score)))
    })
    standScore <- lapply(pm, function(x){
      x / sum(unlist(pm))
    })
    pmp <- rep(0, dim(object@data)[2])
    for(i in 1:length(p)){
      if(p[[i]]>0){
        pmp[indices[[i]]] <- pmp[indices[[i]]] + standScore[[i]]
      }
    }
    names(pmp) <- colnames(object@data)
    
    myRes <- new("sssLinearResult",
                 sssModel = object,
                 sssFitNBest = list(p = p,
                                      score = score,
                                      indices = indices,
                                      pmean = pmean,
                                      pvar = pvar,
                                      residsd = residsd,
                                      postdf = postdf),
                 standScore = unlist(standScore),
                 postMargProb = sort(pmp, decreasing=T))
    if( length(object@training) == 0L | all(object@training == 1) ){
      myRes@sssFitNBest$predTest <- list()
    } else{
      myRes@sssFitNBest$predTest <- .sssPredict(myRes, object@data[object@training==0, ])
    }
    myRes@wAvePredTest <- predict(myRes)
    
    return(myRes)
  }
)



setMethod(
  f = ".readSummary",
  signature = "sssBinaryModel",
  definition = function(object){
    
    outSum <- read.table(object@setupSpec@summaryfile, header=F, sep=" ", na.strings=c("NA", "nan", "na", "NaN"), as.is=T)
    #pmax <- sqrt(ncol(outSum)) - 2
    
    ## SPECIFY OUTPUT AS DESCRIBED BY HANS ET AL
    p <- as.list(as.numeric(outSum[, 1]))
    score <- as.list(as.numeric(outSum[, 2]))
    indices <- lapply(as.list(1:dim(outSum)[1]), function(i){
      if(p[[i]]>0){
        x <- as.numeric(outSum[i, 3:(p[[i]] + 2)])
        x[!is.na(x)]
      } else{
        numeric()
      }
    })
    pmode <- lapply(as.list(1:dim(outSum)[1]), function(i){
      x <- as.numeric(outSum[i, (p[[i]] + 3):(2*p[[i]] + 3)])
      x[!is.na(x)]
    })
    pvar <- lapply(as.list(1:dim(outSum)[1]), function(i){
      x <- as.numeric(outSum[i, (2*p[[i]] + 4):(p[[i]]*p[[i]] + 4*p[[i]] + 4)])
      x[!is.na(x)]
    })
    
    ## CREATE MORE MEANINGFUL OUTPUT METRICS
    pm <- lapply(score, function(x){
      exp(x-max(unlist(score)))
    })
    standScore <- lapply(pm, function(x){
      x / sum(unlist(pm))
    })
    pmp <- rep(0, dim(object@data)[2])
    for(i in 1:length(p)){
      if(length(indices[[i]]) != 0L){
        pmp[indices[[i]]] <- pmp[indices[[i]]] + standScore[[i]]
      }
    }
    names(pmp) <- colnames(object@data)
    
    myRes <- new("sssBinaryResult",
                 sssModel = object,
                 sssFitNBest = list(p = p,
                                      score = score,
                                      indices = indices,
                                      pmode = pmode,
                                      pvar = pvar),
                 standScore = unlist(standScore),
                 postMargProb = sort(pmp, decreasing=T))
    if( length(object@training) == 0L | all(object@training == 1) ){
      myRes@sssFitNBest$predTest <- list()
    } else{
      myRes@sssFitNBest$predTest <- .sssPredict(myRes, object@data[object@training==0, ])
    }
    myRes@wAvePredTest <- predict(myRes)
    
    return(myRes)
  }
)



setMethod(
  f = ".readSummary",
  signature = "sssSurvivalModel",
  definition = function(object){
    
    outSum <- read.table(object@setupSpec@summaryfile, header=F, sep=" ", na.strings=c("NA", "nan", "na", "NaN"), as.is=T)
    #pmax <- sqrt(ncol(outSum) + 1) - 3
    
    ## SPECIFY OUTPUT AS DESCRIBED BY HANS ET AL
    p <- as.list(as.numeric(outSum[, 1]))
    score <- as.list(as.numeric(outSum[, 2]))
    indices <- lapply(1:dim(outSum)[1], function(i){
      if(p[[i]]>0){
        x <- as.numeric(outSum[i, 3:(p[[i]] + 2)])
        x[!is.na(x)]
      } else{
        numeric()
      }
    })
    pmeanalpha <- lapply(1:dim(outSum)[1], function(i){
      x <- as.numeric(outSum[i, (p[[i]] + 3)])
      x[!is.na(x)]
    })
    pmode <- lapply(1:dim(outSum)[1], function(i){
      x <- as.numeric(outSum[i, (p[[i]] + 4):(2*p[[i]] + 4)])
      x[!is.na(x)]
    })
    pvar <- lapply(1:dim(outSum)[1], function(i){
      x <- as.numeric(outSum[i, (2*p[[i]] + 5):(p[[i]]*p[[i]] + 6*p[[i]] + 8)])
      x[!is.na(x)]
    })
    
    ## CREATE MORE MEANINGFUL OUTPUT METRICS
    pm <- lapply(score, function(x){
      exp(x-max(unlist(score)))
    })
    standScore <- lapply(pm, function(x){
      x / sum(unlist(pm))
    })
    pmp <- rep(0, dim(object@data)[2])
    for(i in 1:length(p)){
      if(p[[i]]>0){
        pmp[indices[[i]]] <- pmp[indices[[i]]] + standScore[[i]]
      }
    }
    names(pmp) <- colnames(object@data)
    
    myRes <- new("sssSurvivalResult",
                 sssModel = object,
                 sssFitNBest = list(p = p,
                                      score = score,
                                      indices = indices,
                                      pmeanalpha = pmeanalpha,
                                      pmode = pmode,
                                      pvar = pvar),
                 standScore = unlist(standScore),
                 postMargProb = sort(pmp, decreasing=T))
    if( length(object@training) == 0L | all(object@training == 1) ){
      myRes@sssFitNBest$predTest <- list()
    } else{
      myRes@sssFitNBest$predTest <- .sssPredict(myRes, object@data[object@training==0, ])
    }
    myRes@wAvePredTest <- predict(myRes)
    
    return(myRes)
  }
)



