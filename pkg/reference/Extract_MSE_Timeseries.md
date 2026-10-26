# Annual Time Series from an MSE

Per-simulation annual time series for the historical and projection
periods: spawning biomass relative to the equilibrium unfished spawning
biomass SB0 (`SB_SB0`; SB0 is constant over years) and to SBMSY
(`SB_SBMSY`), fishing mortality relative to FMSY (`F_FMSY`) and, if
`FTarget_FMSY` is supplied, to the F target (`F_FTarget`), and catch
(`Yield`, removals summed over stocks and fleets).

## Usage

``` r
Extract_MSE_Timeseries(MSE, Stock = "Female", FTarget_FMSY = NULL)
```

## Arguments

- MSE:

  An `mse` object.

- Stock:

  Character. Name of the spawning stock. Default `'Female'`.

- FTarget_FMSY:

  Numeric. The F target relative to FMSY, for each simulation or a
  single value (see
  [`Calc_FTarget_FMSY()`](https://wcpfcnswo.bluematterscience.com/pkg/reference/Calc_FTarget_FMSY.md)).
  Default `NULL` (no `F_FTarget`).

## Value

A `data.frame` with columns `Sim`, `Year`, `Period`, `MP`, `Value`, and
`Variable`. Historical rows have `MP = 'Historical'`.

## Details

Ratios are averaged over the seasons of each calendar year (fishing
mortality is an annual rate in each season) and catch is summed.
