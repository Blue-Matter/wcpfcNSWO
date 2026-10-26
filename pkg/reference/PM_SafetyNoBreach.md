# Probability Spawning Biomass Remains Above the Limit Reference Point

Performance metric for the Safety objective: the probability that
spawning biomass remains above a limit reference point, `Lim` of the
equilibrium unfished spawning biomass SB0, in every year in which the MP
is active. A simulation in which SB / SB0 falls below `Lim` in any year
is a breach, so the value is the proportion of simulations without a
breach.

## Usage

``` r
PM_SafetyNoBreach(object, Lim = 0.17, Years = NULL, Stock = "Female")
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
