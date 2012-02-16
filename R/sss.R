## METHOD THAT DISPATCHES TO sss BINARY SPECIFIC TO PLATFORM
#####

setMethod(
  f = "sss",
  signature = c("character", "data.frame", "numeric", "numeric", "numeric", "numeric", "list"),
  definition = function(modtype, data, response, timeToEvent, censor, weights, setupSpec){
    
    ## INITIATE SETUP SPEC IF NOT ALREADY
    if( !exists("setupSpec") ){
      setupSpec <- list()
    }
    
    ## DO modtype SPECIFIC CHECKS AND FILE WRITING
    if( !(tolower(modtype) %in% c("binary", "linear", "survival")) ){
      stop("modtype must be one of: binary, linear, or survival")
    } else{
      
      if( tolower(modtype)=="survival"){ ## SURVIVAL DATA
        if( nrows(data) != length(censor) ){
          stop("number of rows in data does not match number of values in censor")
        }
        if( nrows(data) != length(timeToEvent) ){
          stop("number of rows in data does not match number of values in timeToEvent")
        }
        if( !all(sort(unique(censor)) == c(0,1)) ){
          stop("censor must only contain values of 0 (censor) and 1 (event)")
        }
        ## WRITE OUT THE APPROPRIATE FILES
        setupSpec[["CENSORFILE"]] <- writeCensor(censor)
        setupSpec[["RESPONSEFILE"]] <- writeResponse(timeToEvent)
        
      } else{ ## LINEAR OR BINARY RESPONSE DATA
        
        if( nrows(data) != length(response) ){
          stop("number of rows in data does not match number of values in response")
        }
        if( modtype == "binary" & !all(sort(unique(censor)) == c(0,1)) ){
          stop("response for binary must only contain values of 0 and 1")
        }
        
        ## WRITE OUT APPROPRIATE FILE
        setupSpec[["RESPONSEFILE"]] <- writeResponse(response)
      }
      
    }
    
    ## FILL IN SOME MORE OF THE SETUP FILE
    setupSpec[["NOBSERVATIONS"]] <- nrows(data)
    setupSpec[["NVARIABLES"]] <- ncols(data)
    setupSpec[["modtype"]] <- switch(modtype,
                                     linear   = 1,
                                     binary   = 2,
                                     survival = 3)
    
    ## WRITE DATA FILE
    dataLoc <- writeData(data)
    setupSpec[["DATAFILE"]] <- dataLoc
        
    ## WRITE WEIGHTS FILE, IF NECESSARY
    if(weights != NULL){
      
      if( nrows(data) != length(weights) ){
        stop("number of rows in data does not match number of values in weights")
      }
      if( !all(sort(unique(weights)) == c(0,1)) ){
        stop("weights must only contain values of 0 and 1")
      }
      setupSpec[["WEIGHTSFILE"]] <- writeWeights(weights)
      
    } else{
      setupSpec[["WEIGHTSFILE"]] <- writeWeights(rep(1, nrows(data)))
    }
    
    ## WRITE SETUP FILE
    setupLoc <- writeSetup(setupSpec)
    
    
    
    
    #####
    ## SECTION TO DEFINE WHICH BINARY TO RUN
    #####
    ## myPlat IS ONE OF source, mac.binary.leopard, win.binary
    myPlat <- .Platform$pkgType
    ## FOR DIFFERENTIATION BETWEEN 32 AND 64 BIT OS
    myArch <- .Platform$r_arch
    mySwitch <- paste(myPlat, myArch, sep="")
    
    switch(mySwitch,
           mac.binary.leopardx86_64 = .mac64sss(setupLoc),
           mac.binary.leopardi386   = .mac32sss(setupLoc),
           win.binary               = .winsss(setupLoc),
           sourcex86_64             = .source64sss(setupLoc),
           sourcei386               = .source32sss(setupLoc))
  }
)

setMethod(
  f = ".mac64sss",
  signature = c("character"),
  definition = function(setupLoc){
    
  }
)

setMethod(
  f = ".mac32sss",
  signature = c("character"),
  definition = function(setupLoc){
    
  }
)

setMethod(
  f = ".winsss",
  signature = c("character"),
  definition = function(setupLoc){
    
  }
)

setMethod(
  f = ".source64sss",
  signature = c("character"),
  definition = function(setupLoc){
    
  }
)

setMethod(
  f = ".source32sss",
  signature = c("character"),
  definition = function(setupLoc){
    
  }
)


