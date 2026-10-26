# Per-Simulation Performance Indicators

Calculates the value of each performance indicator (PI) for each
simulation and MP, over the years the CMPs are active (see
[`PM_Window_Years()`](https://wcpfcnswo.bluematterscience.com/pkg/reference/PM_Window_Years.md);
`_Short`, `_Medium`, and `_Long` are the first 10, middle, and last 10
years). The PIs of the candidate management objectives (Table 1 of the
MSE specifications):

## Usage

``` r
Calc_PM_Stats(
  MSE,
  Lim = 0.17,
  StabilityThreshold = 0.1,
  NoChangeThreshold = 0.01,
  FTarget_FMSY = NULL,
  CatchRefYears = 2008:2010,
  TS = Extract_MSE_Timeseries(MSE, FTarget_FMSY = FTarget_FMSY)
)
```

## Arguments

- MSE:

  An `mse` object.

- Lim:

  Numeric. Limit reference point as a fraction of the equilibrium
  unfished spawning biomass. Default `0.17`.

- StabilityThreshold:

  Numeric. Maximum TAC change for `Stability`. Default `0.1`.

- NoChangeThreshold:

  Numeric. Maximum TAC change counted as no management change for
  `NoChange`. Default `0.01` (the CMPs leave the TAC unchanged when the
  change would be less than 1%).

- FTarget_FMSY:

  Numeric. The F target relative to FMSY (see
  [`Calc_FTarget_FMSY()`](https://wcpfcnswo.bluematterscience.com/pkg/reference/Calc_FTarget_FMSY.md)),
  used for the default `TS`. Default `NULL` (no `Status` or
  `FTarget_F`).

- CatchRefYears:

  Numeric. Historical years of the reference catch for `Yield_Hist`.
  Default `2008:2010`.

- TS:

  The output of
  [`Extract_MSE_Timeseries()`](https://wcpfcnswo.bluematterscience.com/pkg/reference/Extract_MSE_Timeseries.md)
  for `MSE`, if already calculated.

## Value

A `data.frame` with columns `PI`, `MP`, `Sim`, and `Value`.

## Details

- `Safety`, `Safety_*`: probability that SB \> `Lim` SB0 in every year,
  i.e. a simulation that falls below the LRP in any year is a breach.

- `Status`: probability that F \<= F target (proportion of years), and
  `FTarget_F`: median F target / F. Only if `TS` has `F_FTarget`.

- `Stability`: probability that the TAC changes by no more than
  `StabilityThreshold` between management cycles.

- `NoChange`: probability of no management change, i.e. that the TAC
  changes by no more than `NoChangeThreshold` between management cycles.

- `Yield_Hist`: probability that the annual catch is at least the mean
  catch in `CatchRefYears` (proportion of years).

and other PIs:

- `Yield_*`: mean annual catch.

- `Depletion_*`: mean SB/SB0; `Depletion_Min`: lowest SB/SB0.

- `Kobe_Green`: probability that SB \> SBMSY and F \< FMSY in the same
  year.

- `SB_SBMSY_All`, `F_FMSY_All`: mean SB/SBMSY and F/FMSY.

- `AAVY`: mean absolute relative TAC change between management cycles.
