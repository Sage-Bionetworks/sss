## WRITE OUT THE SETUP FILE FOR INPUT TO SSS BINARY
##
## SPECIFY DEFAULTS IN setupBase
## FUNCTION TAKES IN setupSpec THAT USER PROVIDES
## OUTPUT IS THE FILE THAT IS TO BE WRITTEN (setupSpec LIST IN R)
#####

setMethod(
  f = "writeSetup",
  signature = "list",
  definition = function(setupSpec){
        
    ## WRITE OUT SETUP FILE
    if( length(names(setupSpec)) != length(unlist(setupSpec)) ){
      stop("Setup file for sss not able to be written - parameters passed must be a singular value")
    }
    
    thisOut <- paste(names(setupSpec), " = ", unlist(setupSpec), sep="")
    
    fileLoc <- file.path(tempdir(), "setup.txt")
    write.table(thisOut, file=fileLoc, sep="\t", quote=F, row.names=F, col.names=F)
    
    return(fileLoc)
  }
)



