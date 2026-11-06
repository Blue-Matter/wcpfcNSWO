# Use the observed indices in an OM conditioned on adjusted indices

For an OM conditioned on index values that were adjusted before the SS3
fit (e.g. for effort creep, see
[`AddEffortCreep()`](https://wcpfcnswo.bluematterscience.com/pkg/reference/AddEffortCreep.md)),
the MPs should see the indices as observed. `Simulate()` fits the index
catchability and residuals to the indices in `OM@Data`, so these must
stay as the adjusted values.

## Usage

``` r
StoreObservedIndices(OM, Observed)

ApplyObservedIndices(Hist)
```

## Arguments

- OM:

  an `om` object conditioned on the adjusted indices

- Observed:

  an `indicesdata` object with the observed indices (e.g.,
  `OM@Data[[1]]@Survey` of the base case OM)

- Hist:

  a `hist` object from `Simulate()`

## Value

the modified `OM` or `Hist` object

## Details

`StoreObservedIndices()` stores the observed indices in `OM@Misc`, with
the ratio of the observed to the adjusted value in the last year of each
index. `ApplyObservedIndices()` is used on the `Hist` object after
`Simulate()`: it replaces the historical indices in `Hist@Data` with the
observed values, and multiplies the catchability (`Efficiency`) of each
index by its ratio, so the projected indices continue from the last
observed value (i.e., the adjustment, such as effort creep, only applies
to the historical period).
