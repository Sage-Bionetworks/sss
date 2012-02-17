## GENERIC METHOD DEFINITIONS
##
## AUTHOR: BRIAN M. BOT
#####

setGeneric(
  name = "sss",
  def = function(object){
    standardGeneric("sss")
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
  name = ".sssWorkhorse",
  def = function(setupLoc){
    standardGeneric(".sssWorkhorse")
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
## METHODS FOR WRITING OUT FILES
#####
setGeneric(
  name = "writeSetup",
  def = function(setupSpec){
    standardGeneric("writeSetup")
  }
)
setGeneric(
  name = "writeWeights",
  def = function(weights){
    standardGeneric("writeWeights")
  }
)
setGeneric(
  name = "writeResponse",
  def = function(response){
    standardGeneric("writeResponse")
  }
)
setGeneric(
  name = "writeData",
  def = function(data){
    standardGeneric("writeData")
  }
)
setGeneric(
  name = "writeCensor",
  def = function(censor){
    standardGeneric("writeCensor")
  }
)

#####
## CLASS CONSTRUCTORS
#####
setGeneric(
  name = "LinearModel",
  def = function(response, data, weights, setupSpec){
    standardGeneric("LinearModel")
  }
)
setGeneric(
  name = "BinaryModel",
  def = function(response, data, weights, setupSpec){
    standardGeneric("BinaryModel")
  }
)
setGeneric(
  name = "SurvivalModel",
  def = function(timeToEvent, censor, data, weights, setupSpec){
    standardGeneric("SurvivalModel")
  }
)
