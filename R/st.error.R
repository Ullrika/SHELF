#' @importFrom sn pst
#' @export

st.error <-
function(parameters, values, probabilities, weights){
	sum(weights * (sn::pst(values, parameters[1], exp(parameters[2]),parameters[3],nu=3, method=4) - probabilities)^2)
}
