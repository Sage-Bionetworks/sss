## WRITE OUT THE CENSOR FILE FOR INPUT TO SSS BINARY
#####

setMethod(
  f = "writeCensor",
  signature = "numeric",
  definition = function(censor){
    
    fileLoc <- file.path(tempdir(), "censor.txt")
    fileConn <- file(fileLoc)
    writeLines(censor)
    close(fileConn)
    
    return(fileLoc)
  }
)



