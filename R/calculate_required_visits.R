#' Visits required to reach a target seed set
#'
#' Inverts the saturating visitation model
#'
#' \deqn{\mathrm{seedset} = \frac{b \cdot a}{100} + \left(b - \frac{b \cdot a}{100}\right) \cdot \left(1 - e^{-c \cdot \mathrm{visits}}\right)}
#'
#' to report how many pollinator visits are needed to reach a target fraction
#' of the potential seed set. Use it to translate a seed set objective (for
#' example "95% of the seeds a fully visited flower can produce") into a
#' required visitation rate.
#'
#' @param c_val Per-visit saturation parameter, as returned by
#'   [calculate_visits()]`$c_parameter` or as the `c` coefficient of
#'   [fit_data()].
#' @param b Potential (maximum) seed set.
#' @param a Percentage of the maximum seed set obtained through autonomous
#'   selfing.
#' @param target_percent Proportion of `b` to be reached, between 0 and 1.
#'   Defaults to `0.95` (95% of the potential seed set).
#'
#' @return A single numeric value: the expected number of visits per flower
#'   needed to reach `target_percent` of `b`. Returns `0` when autonomous
#'   selfing already meets the target.
#'
#' @seealso [calculate_visits()], [plot_visits()].
#'
#' @examples
#' my_c <- calculate_visits(a = 20, b = 100, SVD = 35)
#'
#' # Visits needed for 95% seed set
#' calculate_required_visits(c_val = my_c$c_parameter, b = 100, a = 20)
#'
#' # Visits needed for 50% seed set
#' calculate_required_visits(c_val = my_c$c_parameter, b = 100, a = 20,
#'                           target_percent = 0.5)
#'
#' @export
calculate_required_visits <- function(c_val, b, a, target_percent = 0.95) {
  # Calculate the baseline (autonomous selfing)
  baseline <- (b * a) / 100
  # Define the target seed set (e.g. 95% of b)
  target_seeds <- b * target_percent
  # Check if baseline already exceeds target
  if (baseline >= target_seeds) {
    # Zero visits needed if selfing covers it
    return(0)
  }
  # Solving for x (visitation)
  # Rearranging: (target - baseline) / (b - baseline) = 1 - exp(-c*x)
  numerator <- target_seeds - baseline
  denominator <- b - baseline
  visits_needed <- as.numeric(-log(1 - (numerator / denominator)) / c_val)
  visits_needed
}