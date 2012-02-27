## GENERIC METHOD DEFINITIONS
##
## AUTHOR: BRIAN M. BOT
#####

setGeneric(
  name = "sss",
  def = function(formula, ...){
    standardGeneric("sss")
  }
)
setGeneric(
  name = ".sssWorker",
  def = function(object){
    standardGeneric(".sssWorker")
  }
)

#####
## SSS DISPATCH CALLS
#####
setGeneric(
  name = ".sssDis",
  def = function(object){
    standardGeneric(".sssDis")
  }
)
setGeneric(
  name = ".sssPlatform",
  def = function(setupLoc){
    standardGeneric(".sssPlatform")
  }
)


#####
## SPECIFIC SSS EXECUTABLE CALLS
#####
setGeneric(
  name = ".mac64sss",
  def = function(setupLoc){
    standardGeneric(".mac64sss")
  }
)
setGeneric(
  name = ".winsss",
  def = function(setupLoc){
    standardGeneric(".winsss")
  }
)
setGeneric(
  name = ".source64sss",
  def = function(setupLoc){
    standardGeneric(".source64sss")
  }
)
setGeneric(
  name = ".source32sss",
  def = function(setupLoc){
    standardGeneric(".source32sss")
  }
)



#####
## METHODS FOR WRITING OUT AND READING IN FILES
#####
setGeneric(
  name = ".writeSetup",
  def = function(setupSpec){
    standardGeneric(".writeSetup")
  }
)
setGeneric(
  name = ".writeWeights",
  def = function(weights){
    standardGeneric(".writeWeights")
  }
)
setGeneric(
  name = ".writeResponse",
  def = function(response){
    standardGeneric(".writeResponse")
  }
)
setGeneric(
  name = ".writeData",
  def = function(data){
    standardGeneric(".writeData")
  }
)
setGeneric(
  name = ".writeCensor",
  def = function(censor){
    standardGeneric(".writeCensor")
  }
)

setGeneric(
  name = ".readSummary",
  def = function(object){
    standardGeneric(".readSummary")
  }
)

#####
## CLASS CONSTRUCTORS
#####
setGeneric(
  name = "LinearModel",
  def = function(call, response, data, weights, setupSpec){
    standardGeneric("LinearModel")
  }
)
setGeneric(
  name = "BinaryModel",
  def = function(call, response, data, weights, setupSpec){
    standardGeneric("BinaryModel")
  }
)
setGeneric(
  name = "SurvivalModel",
  def = function(call, timeToEvent, censor, data, weights, setupSpec){
    standardGeneric("SurvivalModel")
  }
)
