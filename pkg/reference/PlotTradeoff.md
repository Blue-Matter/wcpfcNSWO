# Trade-off between two performance indicators

Plots the mean (over the selected OMs) of two performance indicators for
each MP, using the data of
[`Slick::plotTradeoff()`](https://slick.bluematterscience.com/reference/plotTradeoff.html),
with control over the colour and shape of the points, labels, reference
lines, and axis limits.

## Usage

``` r
PlotTradeoff(
  slick,
  xPI = 1,
  yPI = 2,
  MPs = NULL,
  OMs = 1,
  color_by = NULL,
  shape_by = NULL,
  colors = NULL,
  shapes = NULL,
  labels = FALSE,
  ref_x = NULL,
  ref_y = NULL,
  xlim = NULL,
  ylim = NULL,
  xlab = NULL,
  ylab = NULL,
  color_title = NULL,
  shape_title = NULL,
  point_size = 2.8,
  base_size = 11,
  base_family = "serif"
)
```

## Arguments

- slick:

  A Slick object.

- xPI, yPI:

  Index or code of the Tradeoff performance indicators.

- MPs:

  Codes or indices of the MPs. Defaults to all.

- OMs:

  Codes or indices of the OMs to average over. Defaults to the first OM.

- color_by, shape_by:

  Optional vectors (one per MP, or named by MP code) that set the colour
  and shape of the points.

- colors:

  Optional colours for the levels of `color_by`.

- shapes:

  Optional shapes for the levels of `shape_by`.

- labels:

  Logical, or a character vector of the MP codes to label.

- ref_x, ref_y:

  Optional reference lines.

- xlim, ylim:

  Optional axis limits.

- xlab, ylab:

  Optional axis labels.

- color_title, shape_title:

  Legend titles.

- point_size:

  Size of the points.

- base_size, base_family:

  Passed to
  [`theme_slick_print()`](https://wcpfcnswo.bluematterscience.com/pkg/reference/theme_slick_print.md).

## Value

A `ggplot2` object.
