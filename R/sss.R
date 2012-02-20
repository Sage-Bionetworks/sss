## METHOD THAT DISPATCHES TO sss BINARY SPECIFIC TO PLATFORM
#####

setMethod(
  f = "sss",
  signature = "sssModel",
  definition = function(object){
    
    ## FILL IN SETUP INFORMATION
    object@setupSpec[["modtype"]] <- object@modtype
    object@setupSpec[["NOBSERVATIONS"]] <- nrow(object@data)
    object@setupSpec[["NVARIABLES"]] <- ncol(object@data)
    object@setupSpec[["DATAFILE"]] <- writeData(object@data)
    if( !is.null(object@weights) ){
      object@setupSpec[["WEIGHTSFILE"]] <- writeWeights(object@weights)
    } else{
      object@setupSpec[["WEIGHTSFILE"]] <- writeWeights(rep(1, nrow(object@data)))
    }
    
    ## PASS ON TO DISPATCH METHOD DIFFERING BY MODEL TYPE TO FILL IN THE REST OF THE SETUP PARAMS
    object <- .sssDis(object)
    
    ## UPDATE DEFAULT SETUP WITH USER SPECIFIED VALUES - AND WRITE OUT THE FILE
    object@setupSpec <- updateSetup(object@setupSpec)
    setupLoc <- writeSetup(object@setupSpec)
    
    ## RUN SSS (ALL THAT IS NEEDED IS THE LOCATION OF THE SETUP FILE)
    .sssWorkhorse(setupLoc)
    
    ## NOW THAT SSS HAS BEEN CALLED, RETURN SUMMARY OF MODEL RUN
    outSum <- readSummary(object)
    return(outSum)
  }
)


### SSS MODEL DISPATCH
setMethod(
  f = ".sssDis",
  signature = "sssLinearModel",
  definition = function(object){
    
    object@setupSpec[["RESPONSEFILE"]] <- writeResponse(object@response)
    object
    
  }
)
setMethod(
  f = ".sssDis",
  signature = "sssBinaryModel",
  definition = function(object){
    
    object@setupSpec[["RESPONSEFILE"]] <- writeResponse(object@response)
    object
    
  }
)
setMethod(
  f = ".sssDis",
  signature = "sssSurvivalModel",
  definition = function(object){
    
    object@setupSpec[["CENSORFILE"]] <- writeCensor(object@censor)
    object@setupSpec[["RESPONSEFILE"]] <- writeResponse(object@timeToEvent)
    object
    
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
           sourcei386               = .source32sss(setupLoc),
           stop("No sss executable available for this platform\nSee http://www.stat.osu.edu/~hans/sss/ for info on available platforms."))
  }
)


#####
## THESE METHODS ARE FOR PLATFORM SPECIFIC DISPATCH
#####
setMethod(
  f = ".mac64sss",
  signature = c("character"),
  definition = function(setupLoc){
    #    pathToExec <- file.path(path.package("sss"), "exec")
    pathToExec <- file.path("/Users/brian/workspace/gitRepos/sss/inst", "exec")
    system(paste(file.path(pathToExec, "modelsearch"), setupLoc, sep=" "))
    system(paste(file.path(pathToExec, "modelsummary"), setupLoc, sep=" "))
  }
)

setMethod(
  f = ".winsss",
  signature = c("character"),
  definition = function(setupLoc){
    #    pathToExec <- file.path(path.package("sss"), "exec")
    pathToExec <- file.path("/Users/brian/workspace/gitRepos/sss/inst", "exec")
    system(paste(file.path(pathToExec, "modelsearch.exe"), setupLoc, sep=" "))
    system(paste(file.path(pathToExec, "modelsummary.exe"), setupLoc, sep=" "))
  }
)

setMethod(
  f = ".source64sss",
  signature = c("character"),
  definition = function(setupLoc){
    #    pathToExec <- file.path(path.package("sss"), "exec")
    pathToExec <- file.path("/Users/brian/workspace/gitRepos/sss/inst", "exec")
    system(paste(file.path(pathToExec, "modelsearch64"), setupLoc, sep=" "))
    system(paste(file.path(pathToExec, "modelsummary64"), setupLoc, sep=" "))
  }
)
setMethod(
  f = ".source32sss",
  signature = c("character"),
  definition = function(setupLoc){
    #    pathToExec <- file.path(path.package("sss"), "exec")
    pathToExec <- file.path("/Users/brian/workspace/gitRepos/sss/inst", "exec")
    system(paste(file.path(pathToExec, "modelsearch32"), setupLoc, sep=" "))
    system(paste(file.path(pathToExec, "modelsummary32"), setupLoc, sep=" "))
  }
)


