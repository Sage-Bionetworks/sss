#' Write out the response file for input to sss binary
#'
#' @param response a numeric vector to be written to file in tempdir()
#' @return the full file path to the file written out
setMethod(
  f = ".writeResponse",
  signature = "numeric",
  definition = function(response){
    
    fileLoc <- tempfile(pattern="response", tmpdir=tempdir(), fileext=".txt")
    write.table(response, file=fileLoc, sep="\t", quote=F, row.names=F, col.names=F)
    
    return(fileLoc)
  }
)



