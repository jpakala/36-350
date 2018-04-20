generate_data = function(n, p){
  temp = matrix(0, n, p)
  for(i in 1:n){
    temp[i,] = rnorm(p, 0, 1)
  }
  
  resp = rnorm(n, 0, 1)
  return(list(covariates = temp, responses = resp))
}

model_select = function(covariances, responses, cutoff){
  m0 = lm(responses ~ covariances)
  lessCutoff = which(summary(m0)$p.value < cutoff)
  
  m1 = lm(responses ~ covariances[,lessCutoff])
  return(summary(m1)$p.value)
}