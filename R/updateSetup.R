## WRITE OUT THE SETUP FILE FOR INPUT TO SSS BINARY
##
## SPECIFY DEFAULTS IN setupBase
## FUNCTION TAKES IN setupSpec THAT USER PROVIDES
## OUTPUT IS THE FILE THAT IS TO BE WRITTEN (setupFinal LIST IN R)
#####

setMethod(
  f = "updateSetup",
  signature = "list",
  definition = function(setupSpec){
    
    ## HERE IS THE BASE SETUP - TO BE MERGED WITH THE SPECIFICS PASSED BY USER
    setupBase <- list(NOBSERVATIONS = NA,
                      NVARIABLES = NA,
                      DATAFILE = NA,
                      RESPONSEFILE = NA,
                      WEIGHTSFILE = NA,
                      CENSORFILE = NA,
                      modtype = NA,
                      OUTFILE = file.path(tempdir(), "outputModels.txt"),
                      ITEROUT = file.path(tempdir(), "iterout.txt"),
                      SUMMARYFILE = file.path(tempdir(), "modelsummary.txt"),
                      NOISY = 1,
                      NORMSTAND = 1,
                      PSTART = 2,
                      ONEVAR = 1,
                      iters = 100,
                      NBest = 10,
                      pmax = 15,
                      innerAnneal1 = 0.6,
                      innerAnneal2 = 0.8,
                      innerAnneal3 = 1,
                      outerAnneal = 0.4,
                      priormeanp = 5)
    
    if( length(setupSpec) != 0L ){

      theseSpecs <- names(setupSpec)
      
      ## CHECK THE OVERLAP - JUST THROW A WARNING AND KEEP GOING
      oops <- setdiff(theseSpecs, names(setupBase))
      if(length(oops) > 0)
        warning(paste("List members provided, but not used in setup script:  ", paste(oops, collapse=", "), sep=""))
      
      ## UPDATE THE DEFAULTS
      setupFinal <- setupBase
      for(this in theseSpecs){
        setupFinal[[this]] <- setupSpec[[this]]
      }
      
    } else{
      setupFinal <- setupBase
    }
    
    return(setupFinal)
    
    }
)



