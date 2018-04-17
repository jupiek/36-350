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
  return(p.vals2)
}
