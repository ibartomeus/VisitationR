# VisitationR 0.1.0

* First release.

* `fit_data()` fits a saturating visitation model to empirical
  `(visitation, seedset)` pairs.

* `calculate_visits()` derives the visitation parameter `c` analytically from
  selfing rate, potential seed set and single-visit deposition, without
  requiring a full simulation fit.

* `calculate_visits0()` kept as the earlier, simulation-based variant of
  `calculate_visits()`.

* `loss()` returns the per-visit decay rate that makes the simulated pollen
  deposition converge on the potential seed set.

* `calculate_required_visits()` inverts the model to report how many visits are
  needed to reach a target proportion of the maximum seed set.

* `plot_visits()` draws the fitted curve.

* `pseudeR2()` has been renamed to `pseudo_R2()` to correct the spelling of
  "pseudo". The old name is no longer exported.