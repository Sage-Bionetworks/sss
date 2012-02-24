## GENERIC CLASS DEFINITIONS
##
## AUTHOR: BRIAN M. BOT
#####

## CREATE A CLASS THAT CONTAINS ALL THE SSS SETUP INFORMATION
setClass(
  Class = "sssSetup",
  
  representation = representation(
    nobservations = "numeric",
    nvariables = "numeric",
    datafile = "character",
    responsefile = "character",
    weightsfile = "character",
    censorfile = "character",
    modtype = "numeric",
    outfile = "character",
    iterout = "character",
    summaryfile = "character",
    
    priormeanp = "numeric",
    pstart = "numeric",
    pmax = "numeric",
    onevar = "numeric",
    iters = "numeric",
    nbest = "numeric",
    inneranneal1 = "numeric",
    inneranneal2 = "numeric",
    inneranneal3 = "numeric",
    outeranneal = "numeric"),
  
  prototype = prototype(
    datafile = "NA",
    responsefile = "NA",
    weightsfile = "NA",
    censorfile = "NA",
    outfile = file.path(tempdir(), "outfile.txt"),
    iterout = file.path(tempdir(), "iterout.txt"),
    summaryfile = file.path(tempdir(), "summary.txt"),
    
    pstart = 2,
    onevar = 1,
    iters = 100,
    nbest = 10,
    pmax = 15,
    inneranneal1 = 0.6,
    inneranneal2 = 0.8,
    inneranneal3 = 1,
    outeranneal = 0.4,
    priormeanp = 5)
)  


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
    setupSpec = "sssSetup")  
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


