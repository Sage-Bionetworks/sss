## METHOD TO READ IN SUMMARY OUTPUT FROM SSS FOR DIFFERENT MODEL TYPES
#####
setMethod(
  f = ".readRes",
  signature = "sssModel",
  definition = function(object){
    
    nBestFits <- .readSummary(object)
    
    ## CREATE MORE MEANINGFUL OUTPUT METRICS
    pm <- lapply(nBestFits$score, function(x){
      exp(x-max(unlist(nBestFits$score)))
    })
    standScore <- lapply(pm, function(x){
      x / sum(unlist(pm))
    })
    pmp <- rep(0, dim(object@data)[2])
    for(i in 1:length(nBestFits$p)){
      if(nBestFits$p[[i]]>0){
        pmp[nBestFits$indices[[i]]] <- pmp[nBestFits$indices[[i]]] + standScore[[i]]
      }
    }
    names(pmp) <- colnames(object@data)
    
    myRes <- new("sssResult",
                 model = object,
                 nBestFits = nBestFits,
                 standScore = unlist(standScore),
                 postMargProb = sort(pmp, decreasing=T))
    
    ## FILL IN THE NBEST PREDICTIONS
    myRes@nBestFits$trainPrediction <- .sssPredict(myRes, object@data[object@training==1, ])
    myRes@trainPredictionSummary <- as.numeric(sapply(myRes@nBestFits$trainPrediction, as.numeric) %*% matrix(myRes@standScore))
    if( any(object@training==0) ){
      myRes@nBestFits$testPrediction <- .sssPredict(myRes, object@data[object@training==0, ])
      myRes@testPredictionSummary <- as.numeric(sapply(myRes@nBestFits$testPrediction, as.numeric) %*% matrix(myRes@standScore))
    } else{
      myRes@nBestFits$testPrediction <- list()
      myRes@testPredictionSummary <- numeric()
    }
    
    return(myRes)
    
  }
)

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
    
    return(list(p = p,
                score = score,
                indices = indices,
                pmean = pmean,
                pvar = pvar,
                residsd = residsd,
                postdf = postdf))
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
    
    return(list(p = p,
                score = score,
                indices = indices,
                pmode = pmode,
                pvar = pvar))
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
    
    return(list(p = p,
                score = score,
                indices = indices,
                pmeanalpha = pmeanalpha,
                pmode = pmode,
                pvar = pvar))
  }
)



