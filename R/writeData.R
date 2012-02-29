#' Write out the data file for input to sss binary
#'
#' @param data a data frame to be written to file in tempdir()
#' @return the full file path to the file written out
setMethod(
  f = ".writeData",
  signature = "matrix",
  definition = function(data){
    
    fileLoc <- tempfile(pattern="xdata", tmpdir=tempdir(), fileext=".txt")
    write.table(data, file=fileLoc, sep="\t", quote=F, row.names=F, col.names=F)
    
    return(fileLoc)
  }
)



