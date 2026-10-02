#' Plot a saturating visitation curve
#'
#' Draws the visitation curve
#'
#' \deqn{\mathrm{seedset}(x) = \frac{b \cdot a}{100} + \left(b - \frac{b \cdot a}{100}\right) \cdot \left(1 - e^{-c \cdot x}\right)}
#'
#' between `from_` and `to_`, using [graphics::curve()].
#'
#' @param a Percentage of the maximum seed set obtained through autonomous
#'   selfing.
#' @param b Potential (maximum) seed set.
#' @param c Per-visit saturation parameter, usually obtained from
#'   [fit_data()] or [calculate_visits()].
#' @param from_ Lower bound of the x-axis (number of visits). Defaults to `0`.
#' @param to_ Upper bound of the x-axis (number of visits). Defaults to `1`.
#' @param add_ Logical. If `TRUE`, add the curve to the current plot instead of
#'   starting a new one. Defaults to `FALSE`.
#' @param col_ Colour of the curve. Defaults to `2` (red).
#' @param ... Further arguments passed on to [graphics::curve()], e.g. `lwd`,
#'   `lty` or `xlab`.
#'
#' @return Invisibly returns the result of [graphics::curve()], a list with the
#'   coordinates `x` and `y` used to draw the curve. Called for its side effect
#'   of drawing the curve.
#'
#' @seealso [fit_data()], [calculate_visits()].
#'
#' @examples
#' plot_visits(a = 30, b = 200, c = 10, to_ = 20)
#'
#' # Overlay a second population on the same axes
#' plot_visits(a = 30, b = 200, c = 10, to_ = 20, col_ = 2)
#' plot_visits(a = 10, b = 150, c = 3, to_ = 20, col_ = 4, add_ = TRUE, lty = 2)
#'
#' @export
plot_visits <- function(a, b, c, from_ = 0, to_ = 1, add_ = FALSE, col_ = 2, ...) {
  a2 <- (b*a)/100
  b2 <- b-a2
  curve(a2+b2*(1-exp(-c*x)), from = from_, to = to_, add = add_, col = col_,
        ylim = c(0, b), las = 1, ...)
}