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
  def = function(object, outputDir){
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
  def = function(training){
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
  name = ".readRes",
  def = function(object){
    standardGeneric(".readRes")
  }
)

setGeneric(
  name = ".readSummary",
  def = function(object){
    standardGeneric(".readSummary")
  }
)

## GENERIC predict IMPORTED FROM STATS
setGeneric(
  name = ".sssPredict",
  def = function(object, newdata){
    standardGeneric(".sssPredict")
  }
)

setGeneric(
  name = ".sssBinaryPredict",
  def = function(object, newdata){
    standardGeneric(".sssBinaryPredict")
  }
)
setGeneric(
  name = ".sssLinearPredict",
  def = function(object, newdata){
    standardGeneric(".sssLinearPredict")
  }
)
setGeneric(
  name = ".sssSurvivalPredict",
  def = function(object, newdata){
    standardGeneric(".sssSurvivalPredict")
  }
)
