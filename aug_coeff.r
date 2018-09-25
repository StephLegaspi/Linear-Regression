#x = c(50, 50, 50, 70, 70, 70, 80, 80, 80, 90, 90, 90, 100, 100, 100)
#y = c(3.3, 2.8, 2.9, 2.3, 2.6, 2.1, 2.5, 2.9, 2.4, 3.0, 3.1, 2.8, 3.3, 3.5, 3.0)

#len_x = length(x)
#len_y = length(y)

#size = 3

RowNames <- function(size){
  rows = c()
  for(i in 1:(size+1)){
    rows <- c(rows, i)
  }
  return(rows);
}

ColNames <- function(size){
  cols = c()
  for(i in 1:(size+2)){
    if(i==size+2){  col_i = "RHS"}
    else{ col_i = paste("x", i, sep = "")}
    cols <- c(cols, col_i)
  }
  return(cols);
}

GetPowers <- function(size){
  powers = c()
  for(i in 0:(2*size)){
    powers <- c(powers, i)
  }
  
  x_raised = c()
  for(i in 1:(2*size+1)){
    x_raised = c(x_raised, sum(x^powers[i]))
  }
  return(x_raised)
}

GetRHS <- function(size){
  powers = c()
  rhs_list = c()
  for(i in 1:(size+1)){
    powers <- c(powers, i-1)
  }
  
  for(r in 1:(size+1)){
    raised = x^powers[r]
    rhs = sum(raised * y)
    rhs_list <- c(rhs_list, rhs)
  }
  return(rhs_list)
}


CreateAugCoeff <- function(m, x, y, size){
  x_raised = GetPowers(size)
  summation_y = sum(y)
  p = 0
  for(row in 1:(size+1)){
    for(col in 1:(size+2)){
      if(col == size+2){
        rhs = GetRHS(size)
        #rhs = x_raised[p+1] * summation_y
        m[row, col] = rhs[p+1]
      }else{
        m[row, col] = x_raised[col+p]
      }
    }
    p = p + 1
  }
  return(m)
}

GetVars <- function(size){
  v = ColNames(size)
  v = v[-length(v)]
  #print(v)
  return(v)
}

AugCoeffMatrix <- function(x, y, size){
  len_x = length(x)
  len_y = length(y)
  
  if(len_x != len_y){
    return(NA)
  }else{
    m = matrix(data=0, nrow=size+1, ncol=size+2, dimnames = list(RowNames(size), ColNames(size)))
    matrix_result = CreateAugCoeff(m, x, y, size)
    result = list(variables = GetVars(size), augcoeffmatrix = matrix_result)
    return(result)
  }
}

