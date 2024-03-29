cindex <- function(clV1=clV1, clV2=clV2, self=FALSE, minSZ=1, method="jaccard") {
  ## (A) Conversions to assure proper data structure 
  ## Allow selection of a minimum cluster size. 
  if(minSZ > 1) {
    CLSZ <- table(clV1)
    clV1 <- clV1[clV1 %in% names(CLSZ[CLSZ >= minSZ])]
    CLSZ <- table(clV2)
    clV2 <- clV2[clV2 %in% names(CLSZ[CLSZ >= minSZ])]
  }
  
  ## Remove non-common labels in clV1 and clV2 
  clV1 <- clV1[names(clV1) %in% names(clV2)]
  clV2 <- clV2[names(clV2) %in% names(clV1)]
  
  ## (B) Compute matrix that provides the common pairs among the two clustering results
  cpairMA <- sapply(unique(clV1), function(x)  sapply(unique(clV2), function(z) sum(names(clV1[clV1==x]) %in% names(clV2[clV2==z]))))
  colnames(cpairMA) <- unique(clV1); rownames(cpairMA) <- unique(clV2)
  olMA <- cpairMA # Copy for export. To debug result, use: sum(names(clV1[clV1==2]) %in% names(clV2[clV2==28]))
  if(self==TRUE) { # Plus cpairMA/2 includes self comparisons and minus cpairMA/2 excludes them.
    cpairMA <- cpairMA^2/2 + cpairMA/2 
  }
  if(self==FALSE) {
    cpairMA <- cpairMA^2/2 - cpairMA/2 
  }
  
  ## (C) Compute Similarity Indices
  if(self==TRUE) { 
    NpairsCL1 <- tapply(names(clV1), clV1, length); NpairsCL1 <- NpairsCL1^2/2 + NpairsCL1/2
    NpairsCL2 <- tapply(names(clV2), clV2, length); NpairsCL2 <- NpairsCL2^2/2 + NpairsCL2/2
  }
  if(self==FALSE) { 
    NpairsCL1 <- tapply(names(clV1), clV1, length); NpairsCL1 <- NpairsCL1^2/2 - NpairsCL1/2
    NpairsCL2 <- tapply(names(clV2), clV2, length); NpairsCL2 <- NpairsCL2^2/2 - NpairsCL2/2
  }
  ja <- sum(cpairMA)
  jb <- sum(NpairsCL1[colnames(cpairMA)] - colSums(cpairMA))
  jc <- sum(NpairsCL2[rownames(cpairMA)] - rowSums(cpairMA))
  ## Return results as list
  if(method=="jaccard") {
    jindex <- ja/(ja+jb+jc)	
    return(list(intersects=olMA, variables=unlist(list(a=ja, b=jb, c=jc)), Jaccard_Index=jindex))
  }
  if(method=="rand") {
    Nitems <- length(names(clV1)) # clV1 and clV2 contain same items (see above)
    if(self==TRUE) {
      jd <- (Nitems^2/2 + Nitems/2) - ja - jb -jc
    }
    if(self==FALSE) {
      jd <- (Nitems^2/2 - Nitems/2) - ja - jb -jc
    }
    rindex <- (ja+jd)/(ja+jb+jc+jd)	
    return(list(intersects=olMA, variables=unlist(list(a=ja, b=jb, c=jc, d=jd)), Rand_Index=rindex))
  }
  if(method=="arand") {
    Nitems <- length(names(clV1)) # clV1 and clV2 contain same items (see above)
    if(self==TRUE) {
      jd <- (Nitems^2/2 + Nitems/2) - ja - jb -jc
    }
    if(self==FALSE) {
      jd <- (Nitems^2/2 - Nitems/2) - ja - jb -jc
    }
    arindex <- (2*(ja*jd - jc*jb)) / ((ja+jb)*(jb+jd)+(ja+jc)*(jc+jd))	
    return(list(intersects=olMA, variables=unlist(list(a=ja, b=jb, c=jc, d=jd)), Adjusted_Rand_Index=arindex))
  }
}
