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
  if(length(lessCutoff) == 0) return(c())
  
  m1 = lm(responses ~ covariances[,lessCutoff])
  return(summary(m1)$p.value)
}

run_simulation = function(n_trials, n, p, cutoff){
  for(i in 1:n_trials){
    x = generate_data(n, p)
    result = model_select(x$covariates, x$responses, cutoff)
    hist(result)
  }
}

run_simulation(1, 100, 10, 0.05)
run_simulation(1, 100, 20, 0.05)
run_simulation(1, 100, 50, 0.05)
run_simulation(1, 1000, 10, 0.05)
run_simulation(1, 1000, 20, 0.05)
run_simulation(1, 1000, 50, 0.05)
run_simulation(1, 10000, 10, 0.05)
run_simulation(1, 10000, 20, 0.05)
run_simulation(1, 10000, 50, 0.05)

make_plot = function(datapath){
  hist(datapath)
}

run_simulation_new = function(n_trials, n, p, cutoff){
  for(i in 1:n_trials){
    x = generate_data(n, p)
    result = model_select(x$covariates, x$responses, cutoff)
    make_plot(result)
  }
}
