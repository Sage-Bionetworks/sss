## CONSTRUCTOR FOR MODEL CLASSES
#####

#####
## LINEAR MODELS
#####
setMethod(
  f = "LinearModel",
  signature = c("numeric", "data.frame", "missing", "ANY"),
  definition = function(response, data, weights, ...){
    LinearModel(response, data, numeric(), ...)
  }
)
setMethod(
  f = "LinearModel",
  signature = c("numeric", "data.frame", "numeric", "ANY"),
  definition = function(response, data, weights, ...){
    
    args <- list(...)
    if(any(names(ags) == ""))
      stop("Optional arguments passed for sssSetup must be named")
    
    newMod <- new("sssLinearModel",
                  response = response,
                  data = data,
                  weights = weights,
                  setupSpec = new("sssSetup", ...))
  }
)


#####
## BINARY MODEL
#####
setMethod(
  f = "BinaryModel",
  signature = c("numeric", "data.frame", "missing", "ANY"),
  definition = function(response, data, weights, ...){
    BinaryModel(response, data, numeric(), ...)
  }
)


setMethod(
  f = "BinaryModel",
  signature = c("numeric", "data.frame", "numeric", "ANY"),
  definition = function(response, data, weights, ...){

    args <- list(...)
    if(any(names(ags) == ""))
      stop("Optional arguments passed for sssSetup must be named")
    
    newMod <- new("sssBinaryModel",
                  response = response,
                  data = data,
                  weights = weights,
                  setupSpec = new("sssSetup", ...))
  }
)


#####
## SURVIVAL MODEL
#####
setMethod(
  f = "SurvivalModel",
  signature = c("numeric", "numeric", "data.frame", "missing", "ANY"),
  definition = function(timeToEvent, censor, data, weights, ...){
    SurvivalModel(timeToEvent, censor, data, numeric(), ...)
  }
)

setMethod(
  f = "SurvivalModel",
  signature = c("numeric", "numeric", "data.frame", "numeric", "ANY"),
  definition = function(timeToEvent, censor, data, weights, ...){

    args <- list(...)
    if(any(names(ags) == ""))
      stop("Optional arguments passed for sssSetup must be named")
    
    newMod <- new("sssSurvivalModel",
                  timeToEvent = timeToEvent,
                  censor = censor,
                  data = data,
                  weights = weights,
                  setupSpec = new("sssSetup", ...))
  }
)



#####
## SET A SHOW METHOD FOR GENERIC sssModel
#####
setMethod(
  f = "show",
  signature = "sssModel",
  definition = function(object){
    cat('An object of class "', class(object), '"\n\n', sep="")
    
    these <- slotNames(object)
    cat("Contains slots (class)\n")
    cat("----------------------\n")
    for(this in these)
      cat("  ", this, " (", class(slot(object, this)), ")\n", sep="")
  }
)

