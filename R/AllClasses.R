## GENERIC CLASS DEFINITIONS
##
## AUTHOR: BRIAN M. BOT
#####


## VIRTUAL CLASS THAT WILL BE EXTENDED BY EACH MODEL TYPE


#' ssModel virtual class and the classes that contain it
#'
#' @alias sssLinearModel,sssBinaryModel,sssSurvivalModel
#' @exportClass
setClass(
  Class = "sssModel",
  
  representation = representation(
    "VIRTUAL",
    data = "data.frame",
    weights = "numeric",
    setupSpec = "list")  
)
setValidity(
  Class = "sssModel",
  function(object){

    if( length(object@weights) != 0L ){
      
      if( nrow(object@data) != length(object@weights) ){
        stop("number of rows in data does not match number of values in weights")
      }
      if( !all(sort(unique(object@weights)) %in% c(0, 1)) ){
        stop("weights must only contain values of 0 and 1")
      }
    }
  }
)

#####
## INDIVIDUAL MODEL TYPES EXTENDING sssModel
#####

## LINEAR MODEL
setClass(
  Class = "sssLinearModel",
  contains = "sssModel",
  
  representation = representation(
    response = "numeric")
)
setValidity(
  "sssLinearModel",
  function(object){
    
    if( nrow(object@data) != length(object@response) ){
      return("number of rows in data does not match number of values in response")
    }
    if( any(is.na(object@response)) ){
      return("NAs not allowed in response vector")
    }
    
    ## IF PASS ABOVE CHECKS THEN RETURN TRUE
    return(TRUE)
  }
)

## BINARY MODEL
setClass(
  Class = "sssBinaryModel",
  contains = "sssModel",
  
  representation = representation(
    response = "numeric")
)
setValidity(
  "sssBinaryModel",
  function(object){
    
    if( nrow(object@data) != length(object@response) ){
      return("number of rows in data does not match number of values in response")
    }
    if( any(is.na(object@response)) ){
      return("NAs not allowed in response vector")
    }
    if( !all(sort(unique(object@response)) %in% c(0, 1)) ){
      stop("response for binary must only contain values of 0 and 1")
    }
    
    ## IF PASS ABOVE CHECKS THEN RETURN TRUE
    return(TRUE)
  }
)

## SURVIVAL MODEL
setClass(
  Class = "sssSurvivalModel",
  contains = "sssModel",
  
  representation = representation(
    timeToEvent = "numeric",
    censor = "numeric")  
)
setValidity(
  "sssSurvivalModel",
  function(object){
    
    if( nrow(object@data) != length(object@censor) ){
      return("number of rows in data does not match number of values in censor")
    }
    if( nrow(object@data) != length(object@timeToEvent) ){
      return("number of rows in data does not match number of values in timeToEvent")
    }
    if( any(is.na(object@censor)) ){
      return("NAs not allowed in censor vector")
    }
    if( !all(sort(unique(object@censor)) %in% c(0, 1)) ){
      return("censor must only contain values of 0 (censor) and 1 (event)")
    }
    
    ## IF PASS ABOVE CHECKS THEN RETURN TRUE
    return(TRUE)
  }
)


#####
## MODEL RESULT CLASSES
#####

## VIRTUAL CLASS THAT WILL BE EXTENDED BY EACH MODEL TYPE
setClass(
  Class = "sssResult",
  
  representation = representation(
    "VIRTUAL",
    sssModel = "sssModel",
    p = "list",
    score = "list",
    indices = "list")  
)

setClass(
  Class = "sssLinearResult",
  contains = "sssResult",
  
  representation = representation(
    sssModel = "sssLinearModel",
    pmean = "list",
    pvar = "list",
    residsd = "list",
    postdf = "list")  
)

setClass(
  Class = "sssBinaryResult",
  contains = "sssResult",
  
  representation = representation(
    sssModel = "sssBinaryModel",
    pmode = "list",
    pvar = "list")  
)

setClass(
  Class = "sssSurvivalResult",
  contains = "sssResult",
  
  representation = representation(
    sssModel = "sssSurvivalModel",
    pmeanalpha = "list",
    pmode = "list",
    pvar = "list")
)


## CREATE A CLASS THAT CONTAINS ALL THE SSS SETUP INFORMATION
setClass(
  Class = "sssSetup",
  
  representation = representation(
    NOBSERVATIONS = "numeric",
    NVARIABLES = "numeric",
    DATAFILE = "character",
    RESPONSEFILE = "character",
    WEIGHTSFILE = "character",
    CENSORFILE = "character",
    modtype = "numeric",
    OUTFILE = "character",
    ITEROUT = "character",
    SUMMARYFILE = "character",
    NOISY = "numeric",
    NORMSTAND = "numeric",
    PSTART = "numeric",
    ONEVAR = "numeric",
    iters = "numeric",
    NBest = "numeric",
    pmax = "numeric",
    innerAnneal1 = "numeric",
    innerAnneal2 = "numeric",
    innerAnneal3 = "numeric",
    outerAnneal = "numeric",
    priormeanp = "numeric"),
  
  prototype = prototype(
    OUTFILE = file.path(tempdir(), "outputModels.txt"),
    ITEROUT = file.path(tempdir(), "iterout.txt"),
    SUMMARYFILE = file.path(tempdir(), "modelsummary.txt"),
    NOISY = 1,
    NORMSTAND = 1,
    PSTART = 2,
    ONEVAR = 1,
    iters = 100,
    NBest = 10,
    pmax = 15,
    innerAnneal1 = 0.6,
    innerAnneal2 = 0.8,
    innerAnneal3 = 1,
    outerAnneal = 0.4,
    priormeanp = 5)
)  

