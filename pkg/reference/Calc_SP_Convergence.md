# Convergence of the Surplus Production MPs

The proportion of management cycles in which the surplus production
model of each MP converged, from
[`MSEtool::SPEstimates()`](https://msetool.openmse.com/reference/SPEstimates.html).
When the fit fails or does not converge, the advice is set by the
`OnFail` rule of
[`MSEtool::SurplusProduction()`](https://msetool.openmse.com/reference/SurplusProduction.html)
(by default, the previous TAC is kept).

## Usage

``` r
Calc_SP_Convergence(MSE)
```

## Arguments

- MSE:

  An `mse` object.

## Value

A `data.frame` with columns `MP`, `nCycles` (management cycles over all
simulations), and `Converged` (proportion of cycles); no rows if no MP
fits a surplus production model.
