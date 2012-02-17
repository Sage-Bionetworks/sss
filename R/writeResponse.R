## WRITE OUT THE RESPONSE FILE FOR INPUT TO SSS BINARY
#####

setMethod(
  f = "writeResponse",
  signature = "numeric",
  definition = function(response){
    
    fileLoc <- file.path(tempdir(), "response.txt")
    write.table(response, file=fileLoc, sep="\t", quote=F, row.names=F, col.names=F)
    
    return(fileLoc)
  }
)



