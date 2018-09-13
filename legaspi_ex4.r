this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
source("legaspi_ex3.r")

result1 = AugCoeffMatrix(system)
aug_coeff_matrix = result1$augcoeffmatrix
vars = result1$variables

n = len-1

FindPivotRowIndex <- function(pivot, column, a){
  for(i in column:n){
    if(pivot == a[i, column]){
      pivot_row_index = i
      return(pivot_row_index)
    }
  }
}

FindPivotRow <- function(i, a){
  pivot = max(abs(a[i:n,i]))
  pivot_row_index = FindPivotRowIndex(pivot, i, a)
  pivot_row = a[pivot_row_index, ]
  p = list(piv_index = pivot_row_index, piv_row = pivot_row)
  return(p)
}

GetUpperTriangular <- function(a){
  for(i in 1:(n-1)){
    p = FindPivotRow(i, a)
    
    if(a[p$piv_index, i] == 0){
      return(FALSE)
    }
    
    temp = a[i, ]
    a[i, ] = p$piv_row
    a[p$piv_index, ] = temp
    for(j in (i+1):n){
      PE = a[i, i]
      MULT = a[j, i]/PE
      NR = MULT * a[i, ]
      a[j,] = a[j,] - NR
    }
  }
  return(a)
}

GetCoeffs <- function(a){
  b = c()
  column = n+1
  for(row in 1:n){
    b <- c(b, a[row, column])
  }
  return(b)
}

BackwardSubstitution <- function(a){
  x = c()
  b = GetCoeffs(a)
  x[n] = a[n, n+1] / a[n, n]
  
  for(i in (n-1):1){
    sum_ans = sum(a[i, (i+1):n] * x[(i+1):n])
    x[i] = (b[i] - sum_ans) / a[i, i]
  }
  return(x)
}

GaussianElimination <- function(aug_coeff_matrix){ 
  a= GetUpperTriangular(aug_coeff_matrix)
  if(a[1] == FALSE){
    print(NA)
  }else{
    x = BackwardSubstitution(a)
  }
  result = list(solutionSet = x, variables = vars, matrix = a)
  return(result)
}

GetIdentityMatrix <- function(a){
  for(i in 1:n){
    if(i != n){
      p = FindPivotRow(i, a)
      
      if(a[p$piv_index, i] == 0){
        return(FALSE)
      }
      
      temp = a[i, ]
      a[i, ] = p$piv_row
      a[p$piv_index, ] = temp
    }
    a[i, ] = a[i, ] / a[i, i]
    
    for(j in 1:n){
      if(i == j){
        next
      }
      NR = a[j, i] * a[i, ]
      a[j, ] = a[j, ] - NR
    }
  }
  return(a)
}

GaussJordanElimination <- function(a){
  a = GetIdentityMatrix(a)
  if(a[1] == FALSE){
    print(NA)
  }else{
    x = GetCoeffs(a)
  }
  result = list(solutionSet = x, variables = vars, matrix = a)
  return(result)
}

#print(aug_coeff_matrix)
result1 = GaussianElimination(aug_coeff_matrix)
result2 = GaussJordanElimination(aug_coeff_matrix)

print(result1)
print(result2)




