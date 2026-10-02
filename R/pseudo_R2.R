#' Pseudo R-squared of a fitted visitation model
#'
#' Computes a pseudo R-squared as the proportion of the variance in the
#' observed seed set that is explained by a fitted nonlinear model:
#'
#' \deqn{\mathrm{pseudo\ R^2} = 1 - \frac{\sum_i (y_i - \hat{y}_i)^2}{\sum_i (y_i - \bar{y})^2}}
#'
#' @param model A fitted model object, for example the `nls` object returned by
#'   [fit_data()] with `simplify = "model"`.
#' @param seedset The vector of observed seed set values that was used to fit
#'   `model`.
#'
#' @return A single numeric value, the pseudo R-squared. It is `1` for a
#'   perfect fit, `0` for a fit no better than the mean, and negative when the
#'   model predicts worse than the mean.
#'
#' @seealso [fit_data()].
#'
#' @examples
#' visits <- c(0, 2, 5, 10, 20, 40, 80)
#' seedset <- c(20, 45, 70, 88, 96, 99, 100)
#'
#' model <- fit_data(seedset = seedset, visitation = visits, simplify = "model")
#' pseudo_R2(model = model, seedset = seedset)
#'
#' @export
pseudo_R2 <- function(model, seedset) {
  RSS <- sum(residuals(model)^2)
  TSS <- sum((seedset - mean(seedset))^2)
  pseudo_R2 <- 1 - (RSS / TSS)
  pseudo_R2
}