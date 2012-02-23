#' Write out the response file for input to sss binary
#'
#' @param response a numeric vector to be written to file in tempdir()
#' @return the full file path to the file written out
setMethod(
  f = ".writeResponse",
  signature = "numeric",
  definition = function(response){
    
    fileLoc <- file.path(tempdir(), "response.txt")
    write.table(response, file=fileLoc, sep="\t", quote=F, row.names=F, col.names=F)
    
    return(fileLoc)
  }
)



