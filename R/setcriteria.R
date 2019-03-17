#' setCriteria
#'
#' @param minDO minimum Dissolved Oxygen criteria
#' @param maxTemperature maximum Dissolved Oxygen criteria
#'
#' @return
#' @export
#'
#' @examples
setCriteria <- function(minDO, maxTemperature) {

  if(is.numeric(minDO) == FALSE) {
    stop("minDO must be numeric")}
  if(is.numeric(maxTemperature) == FALSE) {
    stop("maxTemperature must be numeric")}
  criteria <- list("DO" = minDO, "Temperature" = maxTemperature)

}
