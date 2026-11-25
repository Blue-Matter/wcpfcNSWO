# Quilt values by OM and MP, as a heat map

Plots one Quilt performance indicator for each OM (columns) and MP
(rows), with the value in each cell. Values below a minimum (e.g. the
minimum probability of an objective) are shown in bold.

## Usage

``` r
PlotQuiltOM(
  slick,
  PI = 1,
  MPs = NULL,
  OMs = NULL,
  minimum = NULL,
  limits = c(0, 1),
  low = "#f2f6fc",
  high = "#174a8b",
  digits = 2,
  fill_title = NULL,
  OM_label = c("code", "name"),
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

  Codes or indices of the MPs (rows, in this order). Defaults to all.

- OMs:

  Codes or indices of the OMs (columns). Defaults to all.

- minimum:

  Optional value below which values are shown in bold.

- limits:

  Limits of the colour scale.

- low, high:

  Colours of the ends of the (single-hue) colour scale.

- digits:

  Number of decimal places.

- fill_title:

  Title of the colour legend. Defaults to the label of the PI.

- OM_label:

  `'code'` (first column of the OM design) or `'name'`.

- base_size, base_family:

  Passed to
  [`theme_slick_print()`](https://wcpfcnswo.bluematterscience.com/pkg/reference/theme_slick_print.md).

## Value

A `ggplot2` object.
