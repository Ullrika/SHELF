#' @imporFrom sn psn
#' @export

sn_mix.error <-
function(parameters, values, probabilities, weights){
  pmix = exp(parameters[7])/(1+exp(parameters[7]))
  sum(weights * (pmix*sn::psn(values, parameters[1], exp(parameters[2]),parameters[3])+
	                 (1-pmix)*sn::psn(values, parameters[4], exp(parameters[5]),parameters[6])- probabilities)^2)
}
