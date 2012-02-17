## WRITE OUT THE CENSOR FILE FOR INPUT TO SSS BINARY
#####

setMethod(
  f = "writeCensor",
  signature = "numeric",
  definition = function(censor){
    
    fileLoc <- file.path(tempdir(), "censor.txt")
    write.table(censor, file=fileLoc, sep="\t", quote=F, row.names=F, col.names=F)
    
    return(fileLoc)
  }
)



