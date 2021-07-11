#' @importFrom sn pst
#' @export

st_mix.error <-
function(parameters, values, probabilities, weights){
  pmix = exp(parameters[7])/(1+exp(parameters[7]))
  sum(weights * (pmix*sn::pst(values, parameters[1], exp(parameters[2]),parameters[3],nu=3, method=4)+
	                 (1-pmix)*sn::pst(values, parameters[4], exp(parameters[5]),parameters[6],nu=3, method=4)- probabilities)^2)
}
