## WRITE OUT THE RESPONSE FILE FOR INPUT TO SSS BINARY
#####

setMethod(
  f = "writeResponse",
  signature = "numeric",
  definition = function(response){
    
    fileLoc <- file.path(tempdir(), "response.txt")
    fileConn <- file(fileLoc)
    writeLines(response)
    close(fileConn)
    
    return(fileLoc)
  }
)



