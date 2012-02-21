#' Write out the setup file for input to sss binary
#'
#' @param setupSpec a list to be written to file in tempdir()
#' @return the full file path to the file written out
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



