is_symmetric_positive_definite <- function(mat) {
  if (nrow(mat) != ncol(mat)) {
    return(FALSE)
  }
  
  for (i in 1:nrow(mat)) {
    for (j in 1:ncol(mat)) {
      if (mat[i, j] != mat[j, i]) {
        return(FALSE)
      }
    }
  }
 
  eigen <- eigen(mat)$values
  for (i in 1:length(eigen)) {
    if (eigen[i] <= 0) {
      return(FALSE)
    }
  }
  
  return(TRUE)
}

M <- matrix(c(4, 1, 1, 3), nrow = 2)
result <- is_symmetric_positive_definite(M)
print(result)