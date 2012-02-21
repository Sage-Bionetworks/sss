## CONSTRUCTOR FOR MODEL CLASSES
#####

#####
## LINEAR MODELS
#####
setMethod(
  f = "LinearModel",
  signature = c("numeric", "data.frame", "missing", "missing"),
  definition = function(response, data, weights, setupSpec){
    LinearModel(response, data, numeric(), list())
  }
)
setMethod(
  f = "LinearModel",
  signature = c("numeric", "data.frame", "numeric", "missing"),
  definition = function(response, data, weights, setupSpec){
    LinearModel(response, data, weights, list())
  }
)
setMethod(
  f = "LinearModel",
  signature = c("numeric", "data.frame", "missing", "list"),
  definition = function(response, data, weights, setupSpec){
    LinearModel(response, data, numeric(), setupSpec)
  }
)

setMethod(
  f = "LinearModel",
  signature = c("numeric", "data.frame", "numeric", "list"),
  definition = function(response, data, weights, setupSpec){
    newMod <- new("sssLinearModel",
                  response = response,
                  data = data,
                  weights = weights,
                  setupSpec = setupSpec)
  }
)


#####
## BINARY MODEL
#####
setMethod(
  f = "BinaryModel",
  signature = c("numeric", "data.frame", "missing", "missing"),
  definition = function(response, data, weights, setupSpec){
    BinaryModel(response, data, numeric(), list())
  }
)
setMethod(
  f = "BinaryModel",
  signature = c("numeric", "data.frame", "numeric", "missing"),
  definition = function(response, data, weights, setupSpec){
    BinaryModel(response, data, weights, list())
  }
)
setMethod(
  f = "BinaryModel",
  signature = c("numeric", "data.frame", "missing", "list"),
  definition = function(response, data, weights, setupSpec){
    BinaryModel(response, data, numeric(), setupSpec)
  }
)


setMethod(
  f = "BinaryModel",
  signature = c("numeric", "data.frame", "numeric", "list"),
  definition = function(response, data, weights, setupSpec){
    newMod <- new("sssBinaryModel",
                  response = response,
                  data = data,
                  weights = weights,
                  setupSpec = setupSpec)
  }
)


#####
## SURVIVAL MODEL
#####
setMethod(
  f = "SurvivalModel",
  signature = c("numeric", "numeric", "data.frame", "missing", "missing"),
  definition = function(timeToEvent, censor, data, weights, setupSpec){
    SurvivalModel(timeToEvent, censor, data, numeric(), list())
  }
)
setMethod(
  f = "SurvivalModel",
  signature = c("numeric", "numeric", "data.frame", "missing", "list"),
  definition = function(timeToEvent, censor, data, weights, setupSpec){
    SurvivalModel(timeToEvent, censor, data, numeric(), setupSpec)
  }
)
setMethod(
  f = "SurvivalModel",
  signature = c("numeric", "numeric", "data.frame", "numeric", "missing"),
  definition = function(timeToEvent, censor, data, weights, setupSpec){
    SurvivalModel(timeToEvent, censor, data, weights, list())
  }
)


setMethod(
  f = "SurvivalModel",
  signature = c("numeric", "numeric", "data.frame", "numeric", "list"),
  definition = function(timeToEvent, censor, data, weights, setupSpec){
    newMod <- new("sssSurvivalModel",
                  timeToEvent = timeToEvent,
                  censor = censor,
                  data = data,
                  weights = weights,
                  setupSpec = setupSpec)
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

