#' Convert a transect visit count into the number of flowers visited
#'
#' Scales a number of visits recorded along a fixed-area transect to the
#' number of individual flowers that were visited, using the local flower
#' density and the average lifespan of a flower. Each visit is assumed to land
#' on a different flower that is still open, so the number of distinct flowers
#' touched is the visit count rescaled by the number of flowers present per
#' transect area and by how long a single flower remains open.
#'
#' @param V_transect Numeric vector of pollinator visits recorded along the
#'   transect.
#' @param transect_size Numeric value indicating the size of transect in square metre.
#' @param flw_x_m2 Numeric value giving the flower density, i.e. the mean
#'   number of open flowers per square metre.
#' @param lifespan Numeric value giving the mean lifespan of a single flower
#'   in the units used by `V_transect` (usually days).
#'
#' @return Numeric vector of the same length as `V_transect`, giving the number
#'   of flowers visited.
#'
#' @examples
#' V_transect_to_flower(V_transect = 50, flw_x_m2 = 100, lifespan = 8)
#'
#' @export
V_transect_to_flower <- function(V_transect, transect_size, flw_x_m2, lifespan) {
  V_flowers <- V_transect * (transect_size/flw_x_m2) * lifespan
  V_flowers
}