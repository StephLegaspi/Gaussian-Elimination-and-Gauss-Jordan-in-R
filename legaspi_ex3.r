E1 <- function (x1, x2, x3, x4, x5, x7, x8, x9) 4 * x1 + -1 * x2 + 0 * x3 + -1 * x4 + 0 * x5 + 0 * x6 + 0 * x7 + 0 * x8 + 0 * x9 + -80;
E2 <- function (x1, x2, x3, x4, x5, x7, x8, x9) -1 * x1 + 4 * x2 + -1 * x3 + 0 * x4 + -1 * x5 + 0 * x6 + 0 * x7 + 0 * x8 + 0 * x9 + -30;
E3 <- function (x1, x2, x3, x4, x5, x7, x8, x9) 0 * x1 + -1 * x2 + 4 * x3 + 0 * x4 + 0 * x5 + -1 * x6 + 0 * x7 + 0 * x8 + 0 * x9 + -80;
E4 <- function (x1, x2, x3, x4, x5, x7, x8, x9) -1 * x1 + 0 * x2 + 0 * x3 + 4 * x4 + -1 * x5 + 0 * x6 + -1 * x7 + 0 * x8 + 0 * x9 + -50;

E5 <- function (x1, x2, x3, x4, x5, x7, x8, x9) 0 * x1 + -1 * x2 + 0 * x3 + -1 * x4 + 4 * x5 + -1 * x6 + 0 * x7 + -1 * x8 + 0 * x9 + 0;
E6 <- function (x1, x2, x3, x4, x5, x7, x8, x9) 0 * x1 + 0 * x2 + -1 * x3 + 0 * x4 + -1 * x5 + 4 * x6 + 0 * x7 + 0 * x8 + -1 * x9 + -50;
E7 <- function (x1, x2, x3, x4, x5, x7, x8, x9) 0 * x1 + 0 * x2 + 0 * x3 + -1 * x4 + 0 * x5 + 0 * x6 + 4 * x7 + -1 * x8 + 0 * x9 + -120;

E8 <- function (x1, x2, x3, x4, x5, x7, x8, x9) 0 * x1 + 0 * x2 + 0 * x3 + 0 * x4 + -1 * x5 + 0 * x6 + -1 * x7 + 4 * x8 + -1 * x9 + -70;
E9 <- function (x1, x2, x3, x4, x5, x7, x8, x9) 0 * x1 + 0 * x2 + 0 * x3 + 0 * x4 + 0 * x5 + -1 * x6 + 0 * x7 + -1 * x8 + 4 * x9 + -120;


system <- list(E1, E2, E3, E4, E5, E6, E7, E8, E9);

len = length(system) + 1

RowNames <- function(){
  rows = c()
  for(i in 1:length(system)){
    rows <- c(rows, i)
  }
  return(rows);
}

GetUnknownVar <- function(){
  v = Deparse(system, 1, 1)
  v = strsplit(v, " ")[[1]]
  v = v[length(v)]
  v = substr(v, 1, 1)
  return(v);
}

ColNames <- function(){
  v = GetUnknownVar()
  cols = c()
  for(i in 1:len){
    if(i==len){  col_i = "RHS"}
    else{ col_i = paste(v, i, sep = "")}
    cols <- c(cols, col_i)
  }
  return(cols);
}

InitMatrix <- function(){
  row_list = RowNames()
  col_list = ColNames()
  matrix_result = matrix(data=0, nrow=length(system), ncol=length(system)+1, dimnames = list(row_list, col_list))
  return(matrix_result)
}

DeparseEq <- function(system, i){
  equation = paste(deparse(system[i])[2], deparse(system[i])[3], sep="")
  equation = substr(equation, 1, nchar(equation)-1)
  return(equation)
}

Deparse <- function(system, i, j){
  equation = deparse(system[i])[j];
  equation = substr(equation, 1, nchar(equation)-1)
  return(equation)
}


GetLengthOfTerms <- function(system){
  term_count = c()
  
  for(j in 1:length(system)){
    len = 0
    eq = Deparse(system, j, 1)
    eq = strsplit(eq, " ")[[1]]
    eq = eq[length(eq)]
    eq = as.numeric(substr(eq, 2, 2))
    term_count <- c(term_count, eq)
  }
  #print(term_count)
  return(term_count)
}

CheckIfSquareMatrix <- function(system){
  sys_len = length(system)
  term_count = GetLengthOfTerms(system)
  
  for(i in 1:length(term_count)){
    if(sys_len != term_count[i]){ return(FALSE)}
  }
  return(TRUE)
}

SortTerms <- function(splitted_list){
  eq_list = c();
  for(j in 1:length(splitted_list)) {
    if (grepl("x", splitted_list[j])) {
      index = strtoi(substring(splitted_list[j], nchar(splitted_list[j]), nchar(splitted_list[j])))
      eq_list[index] = splitted_list[j];
    }else{ b = splitted_list[j] } 
  }
  eq_list <- c(eq_list, b)
  return(eq_list)
}

NegateRHS <- function(coefficient){
  return(coefficient * -1)
}

GetCoefficients <- function(j, eq_list){
  coeff = as.numeric(strsplit(eq_list[j], "\\*")[[1]][1]);
  if(j == length(eq_list)){
    coeff = NegateRHS(coeff)
  }
  return(coeff)
}


SplitTerms <- function(equation){
  splitted_list = strsplit(equation, " \\+ ")[[1]];
  #print(splitted_list)
  return(splitted_list)
}

Tokenize <- function(system){
  coefficients = c()
  for(i in 1:len){
    equation = DeparseEq(system, i);
    splitted_list = SplitTerms(equation)
    eq_list = SortTerms(splitted_list)
    
    for(j in 1:length(eq_list)){
      coeff = GetCoefficients(j, eq_list)
      coefficients <- c(coefficients, coeff)
    }
    
  }
  return(coefficients)
}

FillMatrix <- function(row, col, coefficients, matrix_result){
  counter = 1;
  for(i in 1:row){
    for(j in 1:col){
      matrix_result[i,j] = coefficients[counter]
      counter = counter+1
    }
  }
  return(matrix_result)
}



GetVariables <- function(){
  vars = ColNames()
  vars = vars[-length(vars)]
  return(vars)
}

AugCoeffMatrix <- function(system){
  is_square = CheckIfSquareMatrix(system)
  if(is_square){
    matrix_result = InitMatrix()
    coefficients = Tokenize(system)
    matrix_result = FillMatrix(length(system), len, coefficients, matrix_result)
    vars = GetVariables()
    result = list(variables = vars, augcoeffmatrix = matrix_result)
  }else{ result = NA }
  return(result)
}


#res = AugCoeffMatrix(system)
#print(res)








