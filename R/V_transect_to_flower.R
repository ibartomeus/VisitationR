#' Convert a transect visit count into the number of flowers visited
#'
#' Scales a number of visits recorded along a fixed-area transect to the
#' number of individual flowers that were visited, using the local flower
#' density and the average lifespan of a flower. The number of flowers
#' touched is the visit count rescaled by the number of flowers present per
#' transect area and by how long a single flower remains open.
#'
#' @param V_transect Numeric vector of pollinator visits recorded along the
#'   transect.
#' @param transect_size Numeric value indicating the size of transect in square metre.
#' @param transect_time Numeric value indicating the number of effective minutes invested on the transect.
#' @param flw_x_m2 Numeric value giving the flower density, i.e. the mean
#'   number of open flowers per square metre.
#' @param lifespan Numeric value giving the mean lifespan of a single flower
#'   in the units used by `V_transect` (usually minutes).
#'
#' @return Numeric vector of the same length as `V_transect`, giving the number
#'   of visits per flowers.
#'
#' @examples
#' V_transect_to_flower(V_transect = 50, transect_size = 100, transect_time = 30, flw_x_m2 = 100, lifespan = 8)
#'
#' @export
V_transect_to_flower <- function(V_transect, transect_size, 
                                 transect_time, flw_x_m2, lifespan) {
  V_per_m2 <- V_transect/transect_size #* (transect_size/flw_x_m2) * lifespan
  V_per_flw <- V_per_m2/flw_x_m2
  V_per_flw_ls <- (V_per_flw/transect_time)*lifespan
  V_per_flw_ls
}
