# Probability Spawning Biomass is Above the Limit Reference Point

Performance metric for the Safety objective, as specified in Table 1 of
the candidate inputs (NC22 Summary Report, Attachment 7): the
probability that spawning biomass is above a limit reference point,
`Lim` of the equilibrium unfished spawning biomass SB0, in any given
year, over all simulations and the years in which the MP is active. See
[`PM_SafetyNoBreach()`](https://wcpfcnswo.bluematterscience.com/pkg/reference/PM_SafetyNoBreach.md)
for the probability of no breach in any year.

## Usage

``` r
PM_SafetyLRP(object, Lim = 0.17, Years = NULL, Stock = "Female")
```

## Arguments

- object:

  An `mse` object.

- Lim:

  Numeric. Limit reference point as a fraction of SB0. Default `0.17`.

- Years:

  `NULL` (default, all years the MP is active; see
  [`PM_Window_Years()`](https://wcpfcnswo.bluematterscience.com/pkg/reference/PM_Window_Years.md))
  or the projection years to include.

- Stock:

  Character. Name of the spawning stock. Default `'Female'`.

## Value

A `pm` object (see
[`MSEtool::NewPM()`](https://msetool.openmse.com/reference/NewPM.html)).

## Details

Spawning biomass is averaged over the seasons of each calendar year.
