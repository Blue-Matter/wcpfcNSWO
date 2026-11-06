# Create a data frame with details of SS3 Models

Used to import OMs into the MSE framework

## Usage

``` r
ListSS3Runs(
  SSDir_Base = "SS3/Base-case",
  SSDir_Sens = file.path(dirname(SSDir_Base), "Sensitivities")
)
```

## Arguments

- SSDir_Base:

  character. File path to the base case SS3 run

- SSDir_Sens:

  character. File path to the directory containing the sensitivities

## Value

a `data.frame` with details on available models. `Description` is the
change from the base case in the SS3 input files of each run, and `Type`
is the kind of sensitivity: `'Base case'`, `'Life history'`,
`'Catchability'` (increasing catchability in the indices; effort creep)
or `'Assessment'` (other sensitivities of the 2023 assessment: catch,
model settings and CPUE indices). The base case and the life-history and
catchability runs are used as operating models (`Include = TRUE`),
except the runs in `Excluded`, with the reason in `Note`. `Import` is
the runs imported in `02-Construct-OMs.R` (initially the same as
`Include`).
