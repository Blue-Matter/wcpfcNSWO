# Probability Fishing Mortality is at or Below the F Target

Performance metric for the Status objective: the probability that
fishing mortality is at or below the F target (e.g. F40%), over all
simulations and the years in which the MP is active.

## Usage

``` r
PM_StatusF(object, FTarget_FMSY, Years = NULL)
```

## Arguments

- object:

  An `mse` object.

- FTarget_FMSY:

  Numeric. The F target relative to FMSY, for each simulation or a
  single value (see
  [`Calc_FTarget_FMSY()`](https://wcpfcnswo.bluematterscience.com/pkg/reference/Calc_FTarget_FMSY.md)).

- Years:

  `NULL` (default, all years the MP is active) or the projection years
  to include.

## Value

A `pm` object (see
[`MSEtool::NewPM()`](https://msetool.openmse.com/reference/NewPM.html)).

## Details

F is MSEtool's F/FMSY averaged over the seasons of each calendar year,
divided by the F target relative to FMSY (see
[`Calc_FTarget_FMSY()`](https://wcpfcnswo.bluematterscience.com/pkg/reference/Calc_FTarget_FMSY.md)).
