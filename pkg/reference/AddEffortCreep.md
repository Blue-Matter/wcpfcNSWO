# Create an SS3 run with effort creep in the indices of abundance

Copies the SS3 input files of an existing run and deflates the index
observations for an assumed annual increase in fishing efficiency
(effort creep): `obs_y / (1 + creep)^(y - ref_year)`. With a separate
catchability for each index, only the trend matters, so `ref_year` only
rescales `q`.

## Usage

``` r
AddEffortCreep(from, to, creep, fleets = NULL, ref_year = NULL)
```

## Arguments

- from:

  character. Directory of the SS3 run to copy the input files from

- to:

  character. Directory of the new SS3 run. Created if it doesn't exist

- creep:

  numeric. Annual effort creep, e.g. `0.01` for 1% per year

- fleets:

  optional numeric vector. Fleet numbers of the indices to adjust.
  `NULL` (default) adjusts all indices in the data file

- ref_year:

  optional numeric. Year with no adjustment. `NULL` (default) uses the
  first year of each index

## Value

invisibly, a `data.frame` with the adjustment for each fleet
