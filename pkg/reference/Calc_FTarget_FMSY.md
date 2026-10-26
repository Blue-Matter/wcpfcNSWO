# F Target Relative to FMSY

The target fishing mortality, F at a spawning potential ratio of `SPR`
(e.g. F40%), relative to FMSY for each simulation, in the last year with
both reference points. Used to express F relative to the F target from
F/FMSY (see
[`Extract_MSE_Timeseries()`](https://wcpfcnswo.bluematterscience.com/pkg/reference/Extract_MSE_Timeseries.md)).

## Usage

``` r
Calc_FTarget_FMSY(Hist, SPR = 0.4, Stock = "Female")
```

## Arguments

- Hist:

  A `hist` object.

- SPR:

  Numeric. Spawning potential ratio of the F target. Default `0.4`.

- Stock:

  Character. Stock for the SPR calculation. Default `'Female'`.

## Value

Numeric vector with one value per simulation.
