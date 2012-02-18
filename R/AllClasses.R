## GENERIC CLASS DEFINITIONS
##
## AUTHOR: BRIAN M. BOT
#####

## VIRTUAL CLASS THAT WILL BE EXTENDED BY EACH MODEL TYPE
setClass(
  Class = "sssModel",
  
  representation = representation(
    "VIRTUAL",
    modtype = "numeric",
    data = "data.frame",
    weights = "numeric",
    setupSpec = "list"),
  
  prototype = prototype(
    data = data.frame(),
    weights = numeric(),
    setupSpec = list())
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
    response = "numeric"),
  
  prototype = prototype(
    modtype = 1,
    response = numeric())
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
    if( object@modtype != 1 ){
      return("modtype by definition must be 1")
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
    response = "numeric"),
  
  prototype = prototype(
    modtype = 2,
    response = numeric())
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
    if( object@modtype != 2 ){
      return("modtype by definition must be 2")
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
    censor = "numeric"),
  
  prototype = prototype(
    modtype = 3,
    timeToEvent = numeric(),
    censor = numeric())
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
    if( object@modtype != 3 ){
      return("modtype by definition must be 3")
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
    p = "list",
    score = "list",
    indices = "list"),
  
  prototype = prototype(
    p = list(),
    score = list(),
    indices = list())
)

setClass(
  Class = "sssLinearResult",
  contains = "sssResult",
  
  representation = representation(
    pmean = "list",
    pvar = "list",
    residsd = "list",
    postdf = "list"),
  
  prototype = prototype(
    pmean = list(),
    pvar = list(),
    residsd = list(),
    postdf = list())
)

setClass(
  Class = "sssBinaryResult",
  contains = "sssResult",
  
  representation = representation(
    pmode = "list",
    pvar = "list"),
  
  prototype = prototype(
    pmode = list(),
    pvar = list())
)

setClass(
  Class = "sssSurvivalResult",
  contains = "sssResult",
  
  representation = representation(
    pmeanalpha = "list",
    pmode = "list",
    pvar = "list"),
  
  prototype = prototype(
    pmeanalpha = list(),
    pmode = list(),
    pvar = list())
)

