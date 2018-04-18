generate_data = function(n, p) {
  mat = matrix(rnorm(n*p), nrow = n, ncol = p)
  vec = rnorm(n)
  return(list("covariates" = mat, "responses" = vec))
}

model_select = function(covariates, responses, cutoff) {
  sum.coef = summary(lm(responses ~ covariates))$coefficients
  p.vals = sum.coef[,"Pr(>|t|)"][-1]
  cov.i = which(p.vals <= cutoff)
  if (length(cov.i) == 0) return(cov.i)
  
  sum.coef2 = summary(lm(responses ~ covariates[, cov.i]))$coefficients
  p.vals2 = sum.coef2[,"Pr(>|t|)"][-1]
  names(p.vals2) = names(p.vals)[cov.i]
  return(p.vals2)
}

run_simulation = function(n_trials, n, p, cutoff) {
  vals = vector()
  for (i in 1:n_trials) {
    gen.dat = generate_data(n, p)
    pvals = model_select(gen.dat$covariates, gen.dat$responses, cutoff)
    if (length(pvals) > 0) {
      for (i in length(pvals))
        vals[length(vals) + i] = pvals[i]
    }
  }
  save(vals, file = "datapath.text")
}

run_simulation(1, 100, 10, 0.05)
run_simulation(1, 1000, 10, 0.05)
run_simulation(1, 10000, 10, 0.05)
run_simulation(1, 100, 20, 0.05)
run_simulation(1, 1000, 20, 0.05)
run_simulation(1, 10000, 20, 0.05)
run_simulation(1, 100, 50, 0.05)
run_simulation(1, 1000, 50, 0.05)
run_simulation(1, 10000, 50, 0.05)

make_plot = function(datapath) {
  load(datapath)
  return(hist(vals))
}