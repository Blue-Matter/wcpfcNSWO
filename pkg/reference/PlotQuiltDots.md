# Quilt values of each MP across OMs, as a dot plot

Plots one Quilt performance indicator for each MP and OM, with the value
in a reference OM (e.g. the Base Case) highlighted, to show how much the
performance of each MP varies among the OMs.

## Usage

``` r
PlotQuiltDots(
  slick,
  PI = 1,
  MPs = NULL,
  OMs = NULL,
  ref_OM = 1,
  x_by = NULL,
  color_by = NULL,
  colors = NULL,
  ylab = NULL,
  xlab = NULL,
  color_title = NULL,
  base_size = 11,
  base_family = "serif"
)
```

## Arguments

- slick:

  A Slick object.

- PI:

  Index or code of the Quilt performance indicator.

- MPs:

  Codes or indices of the MPs. Defaults to all.

- OMs:

  Codes or indices of the OMs. Defaults to all.

- ref_OM:

  Code or index of the reference OM, highlighted.

- x_by, color_by:

  Vectors (one per MP, or named by MP code) giving the x-axis position
  and colour of each MP (e.g. HCR and MP type).

- colors:

  Optional colours for the levels of `color_by`.

- ylab, xlab, color_title:

  Labels.

- base_size, base_family:

  Passed to
  [`theme_slick_print()`](https://wcpfcnswo.bluematterscience.com/pkg/reference/theme_slick_print.md).

## Value

A `ggplot2` object.

## Details

Candidate Slick plot: a by-OM view of the Quilt values (see
[`PlotQuiltOM()`](https://wcpfcnswo.bluematterscience.com/pkg/reference/PlotQuiltOM.md)).
