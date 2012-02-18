## METHOD TO READ IN SUMMARY OUTPUT FROM SSS FOR DIFFERENT MODEL TYPES
#####


setMethod(
  f = "readSummary",
  signature = "sssLinearModel",
  definition = function(object){
    
    outSum <- read.table(object@setupSpec[["SUMMARYFILE"]], header=F, sep=" ", as.is=T)
    pmax <- sqrt(ncol(outSum) - 3) - 1
    
    ## SPECIFY OUTPUT AS DESCRIBED BY HANS ET AL
    p <- as.list(as.numeric(outSum[, 1]))
    score <- as.list(as.numeric(outSum[, 2]))
    indices <- lapply(1:dim(outSum)[1], function(i){ x <- as.numeric(outSum[i, 3:(pmax + 2)])
                                                     x[!is.na(x)] })
    pmean <- lapply(1:dim(outSum)[1], function(i){ x <- as.numeric(outSum[i, (pmax + 3):(2*pmax + 2)])
                                                    x[!is.na(x)] })
    pvar <- lapply(1:dim(outSum)[1], function(i){ x <- as.numeric(outSum[i, (2*pmax + 3):(pmax*pmax + 2*pmax + 2)])
                                                  x[!is.na(x)] })
    residsd <- as.list(as.numeric(outSum[, pmax*pmax + 2*pmax + 3]))
    postdf <- as.list(as.numeric(outSum[, pmax*pmax + 2*pmax + 4]))
    
    new("sssLinearResult",
        p = p,
        score = score,
        indices = indices,
        pmean = pmean,
        pvar = pvar,
        residsd = residsd,
        postdf = postdf)
  }
)



setMethod(
  f = "readSummary",
  signature = "sssBinaryModel",
  definition = function(object){
    
    outSum <- read.table(object@setupSpec[["SUMMARYFILE"]], header=F, sep=" ", as.is=T)
    pmax <- sqrt(ncol(outSum)) - 2
    
    ## SPECIFY OUTPUT AS DESCRIBED BY HANS ET AL
    p <- as.list(as.numeric(outSum[, 1]))
    score <- as.list(as.numeric(outSum[, 2]))
    indices <- lapply(1:dim(outSum)[1], function(i){ x <- as.numeric(outSum[i, 3:(pmax + 2)])
                                                     x[!is.na(x)] })
    pmode <- lapply(1:dim(outSum)[1], function(i){ x <- as.numeric(outSum[i, (pmax + 3):(2*pmax + 3)])
                                                    x[!is.na(x)] })
    pvar <- lapply(1:dim(outSum)[1], function(i){ x <- as.numeric(outSum[i, (2*pmax + 4):(pmax*pmax + 4*pmax + 4)])
                                                  x[!is.na(x)] })
    
    new("sssBinaryResult",
        p = p,
        score = score,
        indices = indices,
        pmode = pmode,
        pvar = pvar)
  }
)



setMethod(
  f = "readSummary",
  signature = "sssSurvivalModel",
  definition = function(object){
    
    outSum <- read.table(object@setupSpec[["SUMMARYFILE"]], header=F, sep=" ", as.is=T)
    pmax <- sqrt(ncol(outSum) + 1) - 3
    
    ## SPECIFY OUTPUT AS DESCRIBED BY HANS ET AL
    p <- as.list(as.numeric(outSum[, 1]))
    score <- as.list(as.numeric(outSum[, 2]))
    indices <- lapply(1:dim(outSum)[1], function(i){ x <- as.numeric(outSum[i, 3:(pmax + 2)])
                                                     x[!is.na(x)] })
    pmeanalpha <- as.list(as.numeric(outSum[, pmax + 3]))
    pmode <- lapply(1:dim(outSum)[1], function(i){ x <- as.numeric(outSum[i, (pmax + 4):(2*pmax + 4)])
                                                   x[!is.na(x)] })
    pvar <- lapply(1:dim(outSum)[1], function(i){ x <- as.numeric(outSum[i, (2*pmax + 5):(pmax*pmax + 6*pmax + 8)])
                                                  x[!is.na(x)] })
    
    new("sssSurvivalResult",
        p = p,
        score = score,
        indices = indices,
        pmeanalpha = pmeanalpha,
        pmode = pmode,
        pvar = pvar)
  }
)
