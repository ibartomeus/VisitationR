#' Per-visit decay rate used to build a simulated deposition series
#'
#' Returns the per-visit loss (decay) rate that makes a geometrically decaying
#' single-visit deposition series converge exactly on the remaining gap between
#' the autonomous-selfing baseline and the potential seed set:
#'
#' \deqn{\mathrm{loss} = \frac{\mathrm{SVD}}{b - \frac{b \cdot a}{100}}}
#'
#' Feeding this value back into [calculate_visits0()] as `loss` makes the
#' simulated accumulation approach the potential seed set `b` asymptotically
#' without ever overshooting it.
#'
#' @param a Percentage of the maximum seed set obtained through autonomous
#'   selfing.
#' @param b Potential (maximum) seed set.
#' @param SVD Single-visit deposition expressed in seed set units, i.e. the
#'   seed set increment produced by one pollinator visit.
#'
#' @return A single numeric value, the decay rate to pass as the `loss`
#'   argument of [calculate_visits0()].
#'
#' @seealso [calculate_visits0()], [calculate_visits()].
#'
#' @examples
#' loss(a = 20, b = 100, SVD = 50)
#'
#' # Match the simulated curve to a single visit worth half the remaining gap
#' calculate_visits0(a = 20, b = 100, SVD = 50, loss = 0.5)
#'
#' @export
loss <- function(a, b, SVD) {
  SVD/(b-(b*a/100))
}

#' Estimate the visitation parameter with a user-supplied decay rate
#'
#' Simulation-based variant of [calculate_visits()]. A deposition series with a
#' geometrically decaying per-visit contribution is accumulated, and the
#' saturation parameter `c` is then obtained by fitting the saturating model to
#' that simulated series with [stats::nls()].
#'
#' @param a Percentage of the maximum seed set obtained through autonomous
#'   selfing.
#' @param b Potential (maximum) seed set.
#' @param SVD Single-visit deposition expressed in seed set units, i.e. the
#'   seed set increment produced by one pollinator visit.
#' @param loss Per-visit decay rate of the single-visit deposition. Use
#'   [loss()] to compute a value that converges on `b`.
#' @param to_ Maximum number of visits to simulate. Defaults to `100`.
#' @param plot_ Logical. If `TRUE`, plot the simulated points and the fitted
#'   curve. Defaults to `FALSE`.
#' @param col_ Colour used for the fitted curve when `plot_` is `TRUE`.
#'   Defaults to `2` (red).
#'
#' @return A named numeric vector of length one with the fitted saturation
#'   parameter `c`.
#'
#' @seealso [calculate_visits()] for the recommended entry point, [loss()] to
#'   derive `loss`, [plot_visits()] to draw the curve.
#'
#' @examples
#' calculate_visits0(a = 20, b = 100, SVD = 50, loss = 0.5)
#'
#' # Plot the simulated series together with the fitted curve
#' calculate_visits0(a = 20, b = 100, SVD = 50, loss = 0.625, to_ = 100,
#'                    plot_ = TRUE, col_ = 2)
#'
#' @export
calculate_visits0 <- function(a, b, SVD, loss = 0.1, to_ = 100,
                              plot_ = FALSE, col_ = 2) {
  # set a
  if (!is.numeric(a)) stop("a must be numeric")
  # set b
  if (!is.numeric(b)) stop("b must be numeric")
  # force from_ = 0 because of the way this is written.
  from_ <- 0
  # create a vector of accumulated pollen dep per visit
  visits <- c(from_:to_)
  poldep <- rep(NA, length(visits))
  # 0 visits = % of seed set without visits.
  poldep[1] <- (b*a)/100
  # 1 visit (assuming selfing rates are not discounted from the empirical
  # calculation)
  poldep[2] <- SVD
  for (i in 3:to_) {
    lost <- (1-loss)^(i-2)
    poldep[i] <- SVD*lost
  }
  # one extra visit, the asymptote the series converges to
  poldep[to_+1] <- SVD*((1-loss)^(to_-2))
  poldep2 <- rep(NA, length(visits))
  for (j in from_:to_) {
    poldep2[j+1] <- sum(poldep[1:(j+1)])
  }
  # fit c
  nlmod3 <- nls(poldep2 ~  ((b*a)/100) + (b-((b*a)/100)) * (1-exp(-c*visits)),
                start = list(c = 0.2),
                control = nls.control(maxiter = 1000))
  c <- coef(nlmod3)
  # plot it
  if (plot_ == TRUE) {
    a2 <- (b*a)/100
    b2 <- b-a2
    plot(poldep2 ~ visits, las = 1, xlab = "visits",
         ylab = "seed set", xlim = c(0, to_), ylim = c(0, max(c(poldep2, b))))
    curve(a2+b2*(1-exp(-c*x)), from = from_, to = to_, add = TRUE,
          col = col_, ylim = c(0, max(c(poldep2, b))), las = 1)
  }
  c
}

#' Estimate the visitation parameter analytically
#'
#' Derives the per-visit saturation parameter `c` of the saturating visitation
#' model from three quantities that are usually easy to measure:
#'
#' \describe{
#'   \item{`a`}{the percentage of the potential seed set obtained through
#'     autonomous selfing;}
#'   \item{`b`}{the potential (maximum) seed set;}
#'   \item{`SVD`}{the single-visit deposition, i.e. the seed set increment
#'     produced by one pollinator visit.}
#' }
#'
#' The function discounts `SVD` by the autonomous-selfing baseline, derives the
#' decay rate [loss()] that makes the simulated accumulation converge on the
#' remaining gap `b - (b * a / 100)`, builds the corresponding cumulative
#' deposition series, and fits the saturating model to it with [stats::nls()].
#' The fit is reported with its standard error.
#'
#' @param a Percentage of the maximum seed set obtained through autonomous
#'   selfing.
#' @param b Potential (maximum) seed set.
#' @param SVD Single-visit deposition expressed in seed set units, i.e. the
#'   seed set increment produced by one pollinator visit. This is the raw
#'   empirical value; the autonomous-selfing contribution is subtracted
#'   internally.
#' @param from_ Lower bound of the x-axis when `plot_` is `TRUE`. Defaults
#'   to `0`.
#' @param to_ Maximum number of visits to simulate, and upper bound of the
#'   x-axis when `plot_` is `TRUE`. Defaults to `100`.
#' @param plot_ Logical. If `TRUE`, plot the simulated points and the fitted
#'   curve. Defaults to `FALSE`.
#' @param col_ Colour used for the fitted curve when `plot_` is `TRUE`.
#'   Defaults to `2` (red).
#'
#' @return A list with three elements:
#'   \describe{
#'     \item{`optimal_loss`}{the decay rate derived from `a`, `b` and `SVD`,
#'       see [loss()].}
#'     \item{`c_parameter`}{the estimated saturation parameter `c`.}
#'     \item{`c_se`}{the standard error of `c_parameter`.}
#'   }
#'
#' @section Errors:
#' The function stops if the empirical `SVD` is smaller than the
#' autonomous-selfing baseline (the pollinator contribution would be
#' negative), or if the discounted `SVD` exceeds the remaining gap (total seed
#' set would overshoot `b`).
#'
#' @seealso [loss()], [calculate_required_visits()] to invert the resulting
#'   curve, [fit_data()] to estimate `c` from observational data instead.
#'
#' @examples
#' # 20% autonomous selfing, 100 seeds possible, 50 seeds from one visit
#' calculate_visits(a = 20, b = 100, SVD = 50)
#'
#' # Visualise the simulated series and the fitted curve
#' calculate_visits(a = 20, b = 100, SVD = 50, to_ = 100,
#'                  plot_ = TRUE, col_ = 2)
#'
#' @export
calculate_visits <- function(a, b, SVD, from_ = 0, to_ = 100,
                             plot_ = FALSE, col_ = 2) {
  baseline <- (b * a) / 100   # Calculate the baseline (autonomous selfing)
  target_gap <- b - baseline  # Calculate the "target gap" pollinators fill
  # Discount selfing from the empirical estimation
  SVD <- SVD - baseline
  # Safety check: SVD cannot be < 0 or the math breaks
  if (SVD < 0) {
    stop("SVD is lower than the selfing; insect contribution will be negative.")
  }
  calculated_loss <- SVD / target_gap   # Calculate the perfect loss value
  # Safety check: loss cannot be > 1 or the math breaks
  if (calculated_loss > 1) {
    stop("SVD is higher than the remaining gap; total seed set will exceed 'b'.")
  }
  # Create a vector to store accumulated pollen
  visits <- c(0:to_)
  poldep <- rep(NA, length(visits))
  poldep[1] <- baseline
  for (i in 2:length(poldep)) {
    # This simulates the decay towards the limit b
    poldep[i] <- SVD * (1 - calculated_loss)^(i - 2)
  }
  poldep2 <- cumsum(poldep)
  # Fit c, calculating starting values first
  c_start <- -log(1 - (SVD / (b - baseline)))
  if (is.na(c_start) | is.infinite(c_start)) c_start <- 0.1
  nlmod3 <- nls(poldep2 ~  ((b*a)/100) + (b-((b*a)/100)) * (1-exp(-c*visits)),
                start = list(c = c_start),
                control = nls.control(maxiter = 2000,
                                     minFactor = 1/2048,
                                     warnOnly = TRUE,
                                     scaleOffset = 1))
  c <- coef(nlmod3)
  c_se <- summary(nlmod3)$coefficients[2]
  # plot it
  if (plot_ == TRUE) {
    a2 <- (b*a)/100
    b2 <- b-a2
    plot(poldep2 ~ visits, las = 1, xlab = "visits",
         ylab = "seed set", xlim = c(0, to_), ylim = c(0, max(c(poldep2, b))))
    curve(a2+b2*(1-exp(-c*x)), from = from_, to = to_, add = TRUE,
          col = col_, ylim = c(0, max(c(poldep2, b))), las = 1)
  }
  list(optimal_loss = calculated_loss, c_parameter = c, c_se = c_se)
}