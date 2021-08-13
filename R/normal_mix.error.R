#' @imporFrom sn psn
#' @export

normal_mix.error <-
function(parameters, values, probabilities, weights){
  pmix = exp(parameters[5])/(1+exp(parameters[5]))
  sum(weights * (pmix*pnorm(values, parameters[1], exp(parameters[2]))+
	                 (1-pmix)*pnorm(values, parameters[3], exp(parameters[4]))- probabilities)^2)
}
