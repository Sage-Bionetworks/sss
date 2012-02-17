## WRITE OUT THE WEIGHTS FILE FOR INPUT TO SSS BINARY
#####

setMethod(
  f = "writeWeights",
  signature = "numeric",
  definition = function(weights){
    
    fileLoc <- file.path(tempdir(), "weights.txt")
    write.table(weights, file=fileLoc, sep="\t", quote=F, row.names=F, col.names=F)
    
    return(fileLoc)
  }
)



