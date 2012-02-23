#' Write out the censor file for input to sss binary
#'
#' @param censor a numeric vector to be written to file in tempdir()
#' @return the full file path to the file written out
setMethod(
  f = ".writeCensor",
  signature = "numeric",
  definition = function(censor){
    
    fileLoc <- file.path(tempdir(), "censor.txt")
    write.table(censor, file=fileLoc, sep="\t", quote=F, row.names=F, col.names=F)
    
    return(fileLoc)
  }
)



