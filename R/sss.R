## MAIN SSS FUNCTION CALL - AND DISPATCH DEPENDING ON TYPE OF MODEL
#####

setMethod(
  f = "sss",
  signature = "formula",
  definition = function(formula, ...){
    
    Call <- match.call()
    
    y <- eval(Call$formula[[2]])
    x <- as.matrix(eval(Call$formula[[3]]))
    
    
    args <- list(...)

    if(any(names(args) == ""))
      stop("Optional arguments passed for sssSetup must be named")
    
    if( any(names(args) == "training") ){
      training <- args[["training"]]
      args[["training"]] <- NULL
    } else{
      training <- numeric()
    }
    
    setupSpec <- new("sssSetup")
    if( length(args) != 0L ){
      for( i in names(args) ){
        slot(setupSpec, i) <- args[[i]]
      }
    }
    
    if( !(class(y) %in% c("numeric", "Surv")) )
      stop("Response must either be a numeric vector or a Surv object")
    if( class(y) == "Surv" ){
      myObj <- new("sssSurvivalModel",
                   call = Call,
                   timeToEvent = y[, 1],
                   censor = y[, 2],
                   data = x,
                   training = training,
                   setupSpec = setupSpec)
    } else{
      if( all(unique(y) %in% c(0, 1)) ){
        myObj <- new("sssBinaryModel",
                     call = Call,
                     response = y,
                     data = x,
                     training = training,
                     setupSpec = setupSpec)
      } else{
        myObj <- new("sssLinearModel",
                     call = Call,
                     response = y,
                     data = x,
                     training = training,
                     setupSpec = setupSpec)
      }
    }
    
    ## RUN THE WORKER THAT WRITES FILES AND RUNS SSS BINARY
    outSum <- .sssWorker(myObj)
    return(outSum)
  }
)

setMethod(
  f = ".sssWorker",
  signature = "sssModel",
  definition = function(object){
    
    ## FILL IN SETUP INFORMATION
    object@setupSpec@nobservations <- nrow(object@data)
    object@setupSpec@nvariables <- ncol(object@data)
    object@setupSpec@datafile <- .writeData(object@data)
    object@setupSpec@iterout <- file.path(tempdir(), "iterout.txt")
    object@setupSpec@outfile <- file.path(tempdir(), "modelout.txt")
    object@setupSpec@summaryfile <- file.path(tempdir(), "modelsummary.txt")
    if( !is.null(object@training) ){
      object@setupSpec@weightsfile <- .writeWeights(object@training)
    } else{
      object@setupSpec@weightsfile <- .writeWeights(rep(1, nrow(object@data)))
    }
    
    ## PASS ON TO DISPATCH METHOD DIFFERING BY MODEL TYPE TO FILL IN THE REST OF THE SETUP PARAMS
    object <- .sssDis(object)
    
    ## UPDATE DEFAULT SETUP WITH USER SPECIFIED VALUES - AND WRITE OUT THE FILE
    setupLoc <- .writeSetup(object@setupSpec)
    
    ## RUN SSS (ALL THAT IS NEEDED IS THE LOCATION OF THE SETUP FILE)
    .sssPlatform(setupLoc)
    
    ## NOW THAT SSS HAS BEEN CALLED, RETURN SUMMARY OF MODEL RUN
    outSum <- .readSummary(object)
    return(outSum)
  }
)


#' SSS model dispatch
#'
#' This function takes an object and sets appropriate values in the setupSpec based on
#' model type such that the sss binary can point to correct locations.
#'
#' @param object previously constructed object of class derived from \code{sssModel}
#'   and passed by master /code{sss} function call
setMethod(
  f = ".sssDis",
  signature = "sssLinearModel",
  definition = function(object){
    
    object@setupSpec@responsefile <- .writeResponse(object@response)
    object@setupSpec@modtype <- 1
    object
    
  }
)
setMethod(
  f = ".sssDis",
  signature = "sssBinaryModel",
  definition = function(object){
    
    object@setupSpec@responsefile <- .writeResponse(object@response)
    object@setupSpec@modtype <- 2
    object
    
  }
)
setMethod(
  f = ".sssDis",
  signature = "sssSurvivalModel",
  definition = function(object){
    
    object@setupSpec@censorfile <- .writeCensor(object@censor)
    object@setupSpec@responsefile <- .writeResponse(object@timeToEvent)
    object@setupSpec@modtype <- 3
    object
    
  }
)


#' SSS platform specific triage
#'
#' Determines if there is a platform specific executable to be run for the user
#'
#' @param setupLoc the location on the local file system of the setup file necessary for
#'   passing to sss executable.
setMethod(
  f = ".sssPlatform",
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
           source                   = .source64sss(setupLoc),
#           sourcex86_64             = .source64sss(setupLoc),
#           sourcei386               = .source32sss(setupLoc),
           stop("No sss executable available for this platform\nSee http://www.stat.osu.edu/~hans/sss/ for info on available platforms."))
  }
)


#' Platform specific sss call
#'
#' Mac-specific executable
#'
#' @param setupLoc the location on the local file system of the setup file necessary for
#'   passing to sss executable.
setMethod(
  f = ".mac64sss",
  signature = c("character"),
  definition = function(setupLoc){
    pathToExec <- file.path(path.package("sss"), "exec")
    #pathToExec <- file.path("/Users/brian/workspace/gitRepos/sss/inst", "exec")
    system(paste(file.path(pathToExec, "modelsearch"), setupLoc, sep=" "))
    system(paste(file.path(pathToExec, "modelsummary"), setupLoc, sep=" "))
  }
)

#' Platform specific sss call
#'
#' Windows-specific executable
#'
#' @param setupLoc the location on the local file system of the setup file necessary for
#'   passing to sss executable.
setMethod(
  f = ".winsss",
  signature = c("character"),
  definition = function(setupLoc){
    pathToExec <- file.path(path.package("sss"), "exec")
    #pathToExec <- file.path("/Users/brian/workspace/gitRepos/sss/inst", "exec")
    system(paste(file.path(pathToExec, "modelsearch.exe"), setupLoc, sep=" "))
    system(paste(file.path(pathToExec, "modelsummary.exe"), setupLoc, sep=" "))
  }
)

#' Platform specific sss call
#'
#' Unix 64-bit specific executable
#'
#' @param setupLoc the location on the local file system of the setup file necessary for
#'   passing to sss executable.
setMethod(
  f = ".source64sss",
  signature = c("character"),
  definition = function(setupLoc){
    pathToExec <- file.path(path.package("sss"), "exec")
    #pathToExec <- file.path("/Users/brian/workspace/gitRepos/sss/inst", "exec")
    system(paste(file.path(pathToExec, "modelsearch64"), setupLoc, sep=" "))
    system(paste(file.path(pathToExec, "modelsummary64"), setupLoc, sep=" "))
  }
)

#' Platform specific sss call
#'
#' Unix 32-bit specific executable
#'
#' @param setupLoc the location on the local file system of the setup file necessary for
#'   passing to sss executable.
setMethod(
  f = ".source32sss",
  signature = c("character"),
  definition = function(setupLoc){
    pathToExec <- file.path(path.package("sss"), "exec")
    #pathToExec <- file.path("/Users/brian/workspace/gitRepos/sss/inst", "exec")
    system(paste(file.path(pathToExec, "modelsearch32"), setupLoc, sep=" "))
    system(paste(file.path(pathToExec, "modelsummary32"), setupLoc, sep=" "))
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
