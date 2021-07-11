#' @importFrom sn psn
#' @export

sn.error <-
function(parameters, values, probabilities, weights){
	sum(weights * (sn::psn(values, parameters[1], exp(parameters[2]),parameters[3]) - probabilities)^2)
}
