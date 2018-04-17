generate_data = function(n, p) {
  mat = matrix(rnorm(n*p), nrow = n, ncol = p)
  vec = rnorm(n)
  return(list("covariates" = mat, "responses" = vec))
}
