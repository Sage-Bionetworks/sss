## METHOD THAT DISPATCHES TO sss BINARY SPECIFIC TO PLATFORM
#####

setMethod(
  f = "sss",
  signature = "sssModel",
  definition = function(object){
    
    ## FILL IN SETUP INFORMATION
    object@setupSpec[["modtype"]] <- object@modtype
    object@setupSpec[["NOBSERVATIONS"]] <- nrows(object@data)
    object@setupSpec[["NVARIABLES"]] <- ncols(object@data)
    object@setupSpec[["DATAFILE"]] <- writeData(object@data)
    if( !is.null(object@weights) ){
      object@setupSpec[["WEIGHTSFILE"]] <- writeWeights(object@weights)
    } else{
      object@setupSpec[["WEIGHTSFILE"]] <- writeWeights(rep(1, nrows(object@data)))
    }
    
    ## PASS ON TO DISPATCH METHOD DIFFERING BY MODEL TYPE TO FILL IN THE REST OF THE SETUP PARAMS
    object <- .sssDis(object)
    
    ## WRITE SETUP FILE
    setupLoc <- writeSetup(object@setupSpec)
    
    ## RUN SSS (ALL THAT IS NEEDED IS THE SETUP FILE)
    .sssWorkhorse(setupLoc)
    
  }
)


### SSS MODEL DISPATCH
setMethod(
  f = ".sssDis",
  signature = "sssLinearModel",
  definition = function(object){
    
    object@setupSpec[["RESPONSEFILE"]] <- writeResponse(object@response)
    
  }
)
setMethod(
  f = ".sssDis",
  signature = "sssBinaryModel",
  definition = function(object){
    
    object@setupSpec[["RESPONSEFILE"]] <- writeResponse(object@response)
    
  }
)
setMethod(
  f = ".sssDis",
  signature = "sssSurvivalModel",
  definition = function(object){
    
    setupSpec[["CENSORFILE"]] <- writeCensor(censor)
    setupSpec[["RESPONSEFILE"]] <- writeResponse(timeToEvent)
    
  }
)



#####
## THIS FUNCTION IS THE REAL WORKHOSE
#####
setMethod(
  f = ".sssWorkhorse",
  signature = "character",
  definition = function(setupLoc){
    
    ## DEFINE WHICH BINARY TO RUN
    ## myPlat IS ONE OF source, mac.binary.leopard, win.binary
    myPlat <- .Platform$pkgType
    ## FOR DIFFERENTIATION BETWEEN 32 AND 64 BIT OS
    myArch <- .Platform$r_arch
    mySwitch <- paste(myPlat, myArch, sep="")
    
    switch(mySwitch,
           mac.binary.leopardx86_64 = .mac64sss(setupLoc),
#           mac.binary.leopardi386   = .mac32sss(setupLoc), ## DOES THIS EXIST?
           win.binary               = .winsss(setupLoc),
           sourcex86_64             = .source64sss(setupLoc),
           sourcei386               = .source32sss(setupLoc))
  }
)


#####
## THESE METHODS ARE FOR PLATFORM SPECIFIC DISPATCH
#####
setMethod(
  f = ".mac64sss",
  signature = c("character"),
  definition = function(setupLoc){
    pathToExec <- file.path(path.package("sss"), "exec")
    system(paste(file.path(pathToExec, "modelsearch"), setupLoc, sep=" "))
  }
)

setMethod(
  f = ".winsss",
  signature = c("character"),
  definition = function(setupLoc){
    pathToExec <- file.path(path.package("sss"), "exec")
    system(paste(file.path(pathToExec, "modelsearch.exe"), setupLoc, sep=" "))
  }
)

setMethod(
  f = ".source64sss",
  signature = c("character"),
  definition = function(setupLoc){
    pathToExec <- file.path(path.package("sss"), "exec")
    system(paste(file.path(pathToExec, "modelsearch64"), setupLoc, sep=" "))
  }
)
setMethod(
  f = ".source32sss",
  signature = c("character"),
  definition = function(setupLoc){
    pathToExec <- file.path(path.package("sss"), "exec")
    system(paste(file.path(pathToExec, "modelsearch32"), setupLoc, sep=" "))
  }
)


