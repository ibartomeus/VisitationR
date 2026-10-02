#' VisitationR: Estimate Pollinator Visitation Rates from Seed Set Data
#'
#' VisitationR fits saturating (Michaelis-Menten type) models that relate the
#' number of pollinator visits to the resulting seed set of a plant
#' population. All exported functions assume the same model form:
#'
#' \deqn{\mathrm{seedset} = \frac{b \cdot a}{100} + \left(b - \frac{b \cdot a}{100}\right) \cdot \left(1 - e^{-c \cdot \mathrm{visits}}\right)}
#'
#' where `a` is the percentage of the maximum seed set `b` obtained through
#' autonomous selfing, and `c` is the per-visit saturation parameter that is
#' being estimated.
#'
#' The package can be used in three ways:
#'
#' * to fit `c` directly to empirical `(visitation, seedset)` data with
#'   [fit_data()];
#' * to derive `c` from selfing rate, potential seed set and single-visit
#'   deposition with [calculate_visits()];
#' * to invert the model and predict the visits needed to reach a target seed
#'   set with [calculate_required_visits()].
#'
#' @section Functions:
#' \describe{
#'   \item{[fit_data()]}{Fit the model to empirical data.}
#'   \item{[calculate_visits()], [calculate_visits0()]}{Estimate the
#'     saturation parameter `c`.}
#'   \item{[loss()]}{Per-visit decay rate used to build the simulated
#'     deposition series.}
#'   \item{[calculate_required_visits()]}{Inverse of the model: visits needed
#'     to reach a target seed set.}
#'   \item{[plot_visits()]}{Plot the fitted curve.}
#'   \item{[pseudo_R2()]}{Pseudo R-squared of a fitted model.}
#'   \item{[V_transect_to_flower()]}{Convert transect visits to flowers
#'     visited.}
#' }
#'
#' @keywords internal
#' @aliases VisitationR-package
"_PACKAGE"

## usethis namespace: start
#' @importFrom stats nls nls.control coef residuals
#' @importFrom graphics plot curve
#' @importFrom utils globalVariables
## usethis namespace: end
NULL

# `x` is created by graphics::curve() inside the model formulas.
globalVariables("x")