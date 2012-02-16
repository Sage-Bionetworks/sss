## WRITE OUT THE DATA FILE FOR INPUT TO SSS BINARY
#####

setMethod(
  f = "writeData",
  signature = "data.frame",
  definition = function(data){
    
    theseLines <- apply(data, 1, paste, collapse="\t")
    
    fileLoc <- file.path(tempdir(), "data.txt")
    fileConn <- file(fileLoc)
    writeLines(theseLines)
    close(fileConn)
    
    return(fileLoc)
  }
)



