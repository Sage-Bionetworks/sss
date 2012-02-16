## WRITE OUT THE WEIGHTS FILE FOR INPUT TO SSS BINARY
#####

setMethod(
  f = "writeWeights",
  signature = "numeric",
  definition = function(weights){
    
    fileLoc <- file.path(tempdir(), "weights.txt")
    fileConn <- file(fileLoc)
    writeLines(weights)
    close(fileConn)
    
    return(fileLoc)
  }
)



