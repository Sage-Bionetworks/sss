## METHOD TO READ IN SUMMARY OUTPUT FROM SSS FOR DIFFERENT MODEL TYPES
#####


setMethod(
  f = ".readSummary",
  signature = "sssLinearModel",
  definition = function(object){
    
    outSum <- read.table(object@setupSpec@summaryfile, header=F, sep=" ", na.strings=c("NA", "nan", "na", "NaN"), as.is=T)
    pmax <- sqrt(ncol(outSum) - 3) - 1
    
    ## SPECIFY OUTPUT AS DESCRIBED BY HANS ET AL
    models <- lapply(as.list(1:dim(outSum)[1]), function(i){
      p <- as.numeric(outSum[i, 1])
      score <- as.numeric(outSum[i, 2])
      indices <- as.numeric(outSum[i, 3:(p + 2)])
      indices <- indices[!is.na(indices)]
      pmean <- as.numeric(outSum[i, (p + 3):(2*p + 2)])
    })
    p <- as.list(as.numeric(outSum[, 1]))
    score <- as.list(as.numeric(outSum[, 2]))
    indices <- lapply(as.list(1:dim(outSum)[1]), function(i){ x <- as.numeric(outSum[i, 3:(p[[i]] + 2)])
                                                              x[!is.na(x)] })
    pmean <- lapply(as.list(1:dim(outSum)[1]), function(i){ x <- as.numeric(outSum[i, (p[[i]] + 3):(2*p[[i]] + 2)])
                                                            x[!is.na(x)] })
    pvar <- lapply(as.list(1:dim(outSum)[1]), function(i){ x <- as.numeric(outSum[i, (2*p[[i]] + 3):(p[[i]]*p[[i]] + 2*p[[i]] + 2)])
                                                           x[!is.na(x)] })
    residsd <- as.list(as.numeric(outSum[, pmax*pmax + 2*pmax + 3]))
    postdf <- as.list(as.numeric(outSum[, pmax*pmax + 2*pmax + 4]))
    
    ## CREATE MORE MEANINGFUL OUTPUT METRICS
    pm <- lapply(score, function(x){
      exp(x-max(unlist(score)))
    })
    standScore <- lapply(pm, function(x){
      x / sum(unlist(pm))
    })
    
    myRes <- new("sssLinearResult",
                 sssModel = object,
                 sssModelNbest = list(p = p,
                                      score = score,
                                      indices = indices,
                                      pmean = pmean,
                                      pvar = pvar,
                                      residsd = residsd,
                                      postdf = postdf,
                                      standScore = standScore)
                 )
    tmpPred <- predict(myRes)
    myRes@sssModelNbest$predTest <- tmpPred$pred
    
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
    indices <- lapply(as.list(1:dim(outSum)[1]), function(i){ x <- as.numeric(outSum[i, 3:(p[[i]] + 2)])
                                                     x[!is.na(x)] })
    pmode <- lapply(as.list(1:dim(outSum)[1]), function(i){ x <- as.numeric(outSum[i, (p[[i]] + 3):(2*p[[i]] + 3)])
                                                    x[!is.na(x)] })
    pvar <- lapply(as.list(1:dim(outSum)[1]), function(i){ x <- as.numeric(outSum[i, (2*p[[i]] + 4):(p[[i]]*p[[i]] + 4*p[[i]] + 4)])
                                                  x[!is.na(x)] })
    
    ## CREATE MORE MEANINGFUL OUTPUT METRICS
    pm <- lapply(score, function(x){
      exp(x-max(unlist(score)))
    })
    standScore <- lapply(pm, function(x){
      x / sum(unlist(pm))
    })

    myRes <- new("sssBinaryResult",
                 sssModel = object,
                 sssModelNbest = list(p = p,
                                      score = score,
                                      indices = indices,
                                      pmode = pmode,
                                      pvar = pvar,
                                      standScore = standScore)
                 )
    tmpPred <- predict(myRes)
    myRes@sssModelNbest$predTest <- tmpPred$pred
    myRes@sssModelNbest$pFitTest <- tmpPred$pFit
    
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
    indices <- lapply(1:dim(outSum)[1], function(i){ x <- as.numeric(outSum[i, 3:(p[[i]] + 2)])
                                                     x[!is.na(x)] })
    pmeanalpha <- lapply(1:dim(outSum)[1], function(i){ x <- as.numeric(outSum[i, (p[[i]] + 3)])
                                                        x[!is.na(x)] })
    pmode <- lapply(1:dim(outSum)[1], function(i){ x <- as.numeric(outSum[i, (p[[i]] + 4):(2*p[[i]] + 4)])
                                                   x[!is.na(x)] })
    pvar <- lapply(1:dim(outSum)[1], function(i){ x <- as.numeric(outSum[i, (2*p[[i]] + 5):(p[[i]]*p[[i]] + 6*p[[i]] + 8)])
                                                  x[!is.na(x)] })
    
    ## CREATE MORE MEANINGFUL OUTPUT METRICS
    pm <- lapply(score, function(x){
      exp(x-max(unlist(score)))
    })
    standScore <- lapply(pm, function(x){
      x / sum(unlist(pm))
    })
    
    myRes <- new("sssSurvivalResult",
                 sssModel = object,
                 sssModelNbest = list(p = p,
                                      score = score,
                                      indices = indices,
                                      pmeanalpha = pmeanalpha,
                                      pmode = pmode,
                                      pvar = pvar,
                                      standScore = standScore)
                 )
    tmpPred <- predict(myRes)
    myRes@sssModelNbest$predTest <- tmpPred$pred
    
    return(myRes)
  }
)



#####
## SET A SHOW METHOD FOR GENERIC sssModel
#####
setMethod(
  f = "show",
  signature = "sssResult",
  definition = function(object){
    cat('An object of class "', class(object), '"\n\n', sep="")
    
    these <- slotNames(object)
    cat("Contains slots (class)\n")
    cat("----------------------\n")
    for(this in these)
      cat("  ", this, " (", class(slot(object, this)), ")\n", sep="")
  }
)

