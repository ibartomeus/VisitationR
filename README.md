# VisitationR

<!-- badges: start -->
<!-- badges: end -->

Estimate pollinator visitation rates from seed set data.

## The model

Seed set rises with pollinator visits but saturates. VisitationR fits

```
seedset = (b * a) / 100 + (b - (b * a) / 100) * (1 - exp(-c * visits))
```

where `a` is autonomous selfing (as a percentage of the potential seed set),
`b` is the potential seed set, and `c` is the per-visit saturation parameter.

## Installation

```r
# install.packages("remotes")
remotes::install_github("nachobartomeus/VisitationR")
```

For a local checkout:

```r
devtools::install()
```

## Usage

```r
library(VisitationR)

# Fit c to observed visitation rates and seed set
visits  <- c(0, 2, 5, 10, 20, 40, 80)
seedset <- c(20, 45, 70, 88, 96, 99, 100)

fit_data(seedset = seedset, visitation = visits)
#>         a         b         c 
#> 20.04434 98.96858  0.19667 

# Derive c from a single-visit experiment instead
calculate_visits(a = 20, b = 100, SVD = 50)
#> $optimal_loss
#> [1] 0.375
#> 
#> $c_parameter
#>         c 
#> 0.4700036 
#> 
#> $c_se
#> [1] 3.23466e-16

# Invert the model: how many visits to reach 95% of b?
my_c <- calculate_visits(a = 20, b = 100, SVD = 35)$c_parameter
calculate_required_visits(c_val = my_c, b = 100, a = 20)
#> [1] 13.35291

# Goodness of fit and plots
model <- fit_data(seedset = seedset, visitation = visits, simplify = "model")
pseudo_R2(model = model, seedset = seedset)
plot_visits(a = coef(model)[["a"]], b = coef(model)[["b"]],
            c = coef(model)[["c"]], to_ = 90)

# Rescale a transect visit count
V_transect_to_flower(V_transect = 50, flw_x_m2 = 100, lifespan = 8)
#> [1] 400
```

See `vignette("VisitationR")` for a worked walkthrough, and
`help(package = "VisitationR")` for the full reference.

## Functions

| Function                      | Purpose                                             |
|-------------------------------|-----------------------------------------------------|
| `fit_data()`                  | Fit `a`, `b` and `c` to empirical data              |
| `calculate_visits()`          | Estimate `c` from selfing, seed set and SVD         |
| `calculate_visits0()`         | Earlier simulation-based variant of `c`             |
| `loss()`                      | Per-visit decay rate used in the simulation         |
| `calculate_required_visits()` | Visits needed to reach a target seed set            |
| `plot_visits()`               | Draw the fitted curve                               |
| `pseudo_R2()`                 | Pseudo R-squared of a fitted model                 |
| `V_transect_to_flower()`      | Transect visits to number of flowers visited        |

## Development

```r
devtools::document()   # regenerate man/ and NAMESPACE from roxygen comments
devtools::test()      # run the testthat suite
devtools::check()     # full R CMD check
```

`man/` and `NAMESPACE` are generated from the roxygen comments in `R/` and are
tracked in git. Never edit them by hand.

Building the vignette requires [pandoc](https://pandoc.org/) (bundled with
RStudio and RStudio Desktop's markdown support). `R CMD check` is clean:
0 errors, 0 warnings, 0 notes.

## License

MIT. See [LICENSE](LICENSE).