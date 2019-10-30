Avar <- function(genes, selection, samples){
  
  gene <- unlist(genes)
  samples <- as.data.frame(samples)
  selection <- as.matrix(selection)
  samples$gene <- row.names(samples)
  ConSam <- dcast(samples, condition ~ gene)
  ConSam <- as.data.frame(ConSam)
  row.names(ConSam) <- ConSam$condition
  ConSam$condition <- NULL
  ConSam <- as.matrix(ConSam)

  AV <- matrix(nrow = length(gene), ncol = nrow(ConSam))
  AV <- as.data.frame(AV, row.names = gene)
  colnames(AV) <- rownames(ConSam)
  
  ST <- matrix(nrow = length(gene), ncol = nrow(ConSam))
  ST <- as.data.frame(ST, row.names = gene)
  colnames(ST) <- rownames(ConSam)
  
  for(g in gene){
    avar <- list()
    stand <- list()
    for(i in row.names(ConSam)){
      l <- ConSam[i,]
      l <- as.vector(l)
      l <- l[!is.na(l)]
      avar[i] <- mean(selection[g,l])
      stand[i] <- sd(selection[g,l])
    }
    exp1 <- as.data.frame(avar)
    rownames(exp1) <- g
    AV[g,] <- exp1
    exp2 <- as.data.frame(stand)
    rownames(exp2) <- g
    ST[g,] <- exp2
  }
  AvCountsLong   <- melt(as.matrix(AV))
  StCountsLong   <- melt(as.matrix(ST))
  
outPut <- list()
outPut$AV <- AvCountsLong
outPut$ST <- StCountsLong

return(outPut)
}