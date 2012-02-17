## WRITE OUT THE DATA FILE FOR INPUT TO SSS BINARY
#####

setMethod(
  f = "writeData",
  signature = "data.frame",
  definition = function(data){
    
    fileLoc <- file.path(tempdir(), "data.txt")
    write.table(data, file=fileLoc, sep="\t", quote=F, row.names=F, col.names=F)
    
    return(fileLoc)
  }
)



