generate_data = function(n, p){
  temp = matrix(0, n, p)
  for(i in 1:n){
    temp[i,] = rnorm(p, 0, 1)
  }
  
  resp = rnorm(n, 0, 1)
  return(list(covariates = temp, responses = resp))
}
