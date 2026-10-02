#' Fit the saturating visitation model to empirical data
#'
#' Fits the nonlinear model
#'
#' \deqn{\mathrm{seedset} = \frac{b \cdot a}{100} + \left(b - \frac{b \cdot a}{100}\right) \cdot \left(1 - e^{-c \cdot \mathrm{visitation}}\right)}
#'
#' to observed pairs of pollinator visitation rates and resulting seed set,
#' using [stats::nls()]. All three parameters -- `a` (autonomous selfing as a
#' percentage of the potential seed set), `b` (potential/maximum seed set) and
#' `c` (per-visit saturation parameter) -- are estimated simultaneously.
#'
#' @param seedset Numeric vector of observed seed set values (e.g. mean number
#'   of seeds produced per flower, or per fruit).
#' @param visitation Numeric vector of observed pollinator visitation rates.
#'   Must be the same length as `seedset`. The values are interpreted on the
#'   scale on which `c` is to be expressed (usually mean number of visits per
#'   flower).
#' @param a_start,b_start,c_start Starting values for the nonlinear fit. Use
#'   `NULL` (the default) to derive them from the data, which is what you want
#'   in almost all cases: `a_start` from the lowest observed seed set, `b_start`
#'   from slightly above the highest one (the asymptote itself is not a usable
#'   starting point), and `c_start` from the visitation rate at which the curve
#'   reaches half of the range spanned by the data. Supply them explicitly only
#'   if the fit fails to converge.
#' @param simplify Character string controlling the return value. One of:
#'   * `"params"` (default): a named numeric vector of the estimated
#'     coefficients `a`, `b` and `c`.
#'   * `"stats"`: the full coefficient matrix with standard errors, t values
#'     and p-values, as returned by `summary(model)$coefficients`.
#'   * `"model"`: the [stats::nls()] object itself, which can be passed to
#'     [pseudo_R2()] or [plot_visits()].
#'
#' @return Depending on `simplify`: a named numeric vector of coefficients
#'   (`"params"`), a matrix of coefficient statistics (`"stats"`), or an
#'   object of class `nls` (`"model"`).
#'
#' @section Errors:
#' `fit_data()` stops if `seedset` and `visitation` are not numeric vectors of
#' equal length without missing values, or if `simplify` is not one of
#' `"params"`, `"stats"` or `"model"`.
#'
#' @seealso [calculate_visits()] for estimating `c` without a full model fit,
#'   [pseudo_R2()] for goodness of fit, [plot_visits()] for visualisation.
#'
#' @examples
#' visits <- c(0, 2, 5, 10, 20, 40, 80)
#' seedset <- c(20, 45, 70, 88, 96, 99, 100)
#'
#' fit <- fit_data(seedset = seedset, visitation = visits)
#' fit
#'
#' stats_table <- fit_data(
#'   seedset = seedset,
#'   visitation = visits,
#'   simplify = "stats"
#' )
#' stats_table
#'
#' model <- fit_data(seedset = seedset, visitation = visits, simplify = "model")
#' pseudo_R2(model = model, seedset = seedset)
#' plot_visits(a = coef(model)[["a"]], b = coef(model)[["b"]],
#'             c = coef(model)[["c"]], to_ = 90)
#'
#' @export
fit_data <- function(seedset, visitation, a_start = NULL, b_start = NULL,
                     c_start = NULL, simplify = "params") {
  if (!is.numeric(seedset) || !is.numeric(visitation)) {
    stop("`seedset` and `visitation` must both be numeric.", call. = FALSE)
  }
  if (length(seedset) != length(visitation)) {
    stop("`seedset` and `visitation` must have the same length.", call. = FALSE)
  }
  if (anyNA(seedset) || anyNA(visitation)) {
    stop("`seedset` and `visitation` must not contain missing values.",
         call. = FALSE)
  }
  simplify <- match.arg(simplify, c("params", "stats", "model"))

  # Starting values derived from the data, unless the user supplies their own.
  # `b` is padded slightly above the data because the largest observed seed set
  # sits on the asymptote, where the gradient with respect to `b` vanishes and
  # the fit cannot converge.
  if (is.null(b_start)) b_start <- max(seedset) * 1.05
  if (is.null(a_start)) a_start <- 100 * min(seedset) / b_start
  if (is.null(c_start)) {
    # Visitation rate at which the data reach the midpoint of the observed
    # range; on the fitted curve that corresponds to c = log(2) / visits.
    baseline <- (b_start * a_start) / 100
    midpoint <- baseline + 0.5 * (b_start - baseline)
    reached <- which(seedset >= midpoint & visitation > 0)
    c_start <- if (length(reached) > 0) {
      log(2) / min(visitation[reached])
    } else {
      0.1
    }
  }

  nlmod3 <- nls(seedset ~  ((b*a)/100) + (b-((b*a)/100)) * (1-exp(-c*visitation)),
                start = list(a = a_start, b = b_start, c = c_start),
                algorithm = "port",
                lower = c(0, 1e-8, 1e-8),
                control = nls.control(maxiter = 1000))

  switch(
    simplify,
    params = coef(nlmod3),
    stats = summary(nlmod3)$coefficients,
    model = nlmod3
  )
}