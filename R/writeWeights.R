#' Write out the weights file for input to sss binary
#'
#' @param weights a numeric vector to be written to file in tempdir()
#' @return the full file path to the file written out
setMethod(
  f = "writeWeights",
  signature = "numeric",
  definition = function(weights){
    
    fileLoc <- file.path(tempdir(), "weights.txt")
    write.table(weights, file=fileLoc, sep="\t", quote=F, row.names=F, col.names=F)
    
    return(fileLoc)
  }
)



