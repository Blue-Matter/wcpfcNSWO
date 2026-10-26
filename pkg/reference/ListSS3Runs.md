# Create a data frame with details of SS3 Models

Used to import OMs into the MSE framework

## Usage

``` r
ListSS3Runs(
  SSDir_Base = "../WCNPOSWO-2023/Final Base-case",
  SSDir_Sens = file.path(SSDir_Base, "Sensitivities")
)
```

## Arguments

- SSDir_Base:

  character. File path to the base case output

- SSDir_Sens:

  character. File path to the directory containing the sensitivities

## Value

a `data.frame` with details on available models. `Description` is the
change from the base case in the SS3 input files of each run, and
`Include` is `FALSE` for SS3 runs not used as operating models, with the
reason in `Note`.
