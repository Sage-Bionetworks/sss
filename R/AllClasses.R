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
    iterout = "character",
    outfile = "character",
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
    iterout = "NA",
    outfile = "NA",
    summaryfile = "NA",
    
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
    call = "call",
    data = "matrix",
    training = "numeric",
    setupSpec = "sssSetup")  
)
setValidity(
  Class = "sssModel",
  function(object){

    if( length(object@training) != 0L ){
      
      if( nrow(object@data) != length(object@training) ){
        stop("number of rows in data does not match number of values in training")
      }
      if( !all(sort(unique(object@training)) %in% c(0, 1)) ){
        stop("training must only contain values of 0 and 1")
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
    sssFitNBest = "list",
    standScore = "numeric",
    postMargProb = "numeric",
    wAvePredTest = "numeric")
)

setClass(
  Class = "sssLinearResult",
  contains = "sssResult",
  
  representation = representation(
    sssModel = "sssLinearModel")
)

setClass(
  Class = "sssBinaryResult",
  contains = "sssResult",
  
  representation = representation(
    sssModel = "sssBinaryModel")
)

setClass(
  Class = "sssSurvivalResult",
  contains = "sssResult",
  
  representation = representation(
    sssModel = "sssSurvivalModel")
)



#####
## HERE ARE ALL OF THE SHOW METHODS
#####

## SET A SHOW METHOD FOR GENERIC sssModel
setMethod(
  f = "show",
  signature = "sssResult",
  definition = function(object){
    cat('\nType of model fit: "', class(object@sssModel), '"\n\n', sep="")
    cat("Call: ", deparse(object@sssModel@call), "\n", sep="")
    cat("Number of features searched : ", ncol(object@sssModel@data), "\n", sep="")
    cat("Number of training samples  : ", sum(object@sssModel@training==1), "\n", sep="")
    if( any(object@sssModel@training==0) ){
      cat("Number of testing samples   : ", sum(object@sssModel@training==0), "\n\n", sep="")
      cat("To access the predictions on the held-out testing dataset, call:\n")
      cat("  predict(object)\n", sep="")
      
      ## ADD OTHER INFO ABOUT THE PREDICTIONS
    } else{
      cat("To test this predictive model against a validation set, pass a new feature matrix to:\n")
      cat("  predict(object, newdata=newFeatureMatrix)\n", sep="")
    }
    
    these <- slotNames(object)
    cat("\n----------------------\n")
    cat("Contains slots (class)\n")
    cat("----------------------\n")
    for(this in these){
      cat("  ", this, " (", class(slot(object, this)), ")\n", sep="")
      if( class(slot(object,this)) == "list" ){
        theseL <- names(slot(object, this))
        for(thisL in theseL)
          cat("      ", thisL, "\n", sep="")
      }
    }
    
  }
)


## SET A SHOW METHOD FOR GENERIC sssModel
setMethod(
  f = "show",
  signature = "sssModel",
  definition = function(object){
    cat('An object of class "', class(object), '"\n\n', sep="")
    
    these <- slotNames(object)
    cat("----------------------\n")
    cat("Contains slots (class)\n")
    cat("----------------------\n")
    for(this in these){
      cat("  ", this, " (", class(slot(object, this)), ")\n", sep="")
      if( class(this) == "list" ){
        theseL <- names(object)
        for(thisL in theseL)
          cat("    ", thisL, " (", class(slot(object, this)[[thisL]]), ")\n", sep="")
      }
    }
  }
)

