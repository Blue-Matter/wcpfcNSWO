# Time series of a performance indicator, with uncertainty ribbons

Plots the median (or mean) and quantile ribbons of a time-series
performance indicator for a set of MPs, using
[`Slick::plotTimeseries()`](https://slick.bluematterscience.com/reference/plotTimeseries.html).
The MPs can be split into groups, with one panel per group and the MPs
of each group shown together with ribbons in the colour of each MP (e.g.
one panel per HCR, with one line and ribbon per CMP type), or shown with
one panel per OM.

## Usage

``` r
PlotTimeseries(
  slick,
  PI = 1,
  MPs = NULL,
  OMs = 1,
  groups = NULL,
  byOM = FALSE,
  MP_labels = NULL,
  MP_colors = NULL,
  MeanMed = "median",
  quants1 = c(0.25, 0.75),
  quants2 = c(0.05, 0.95),
  alpha1 = 0.35,
  alpha2 = 0.15,
  ref_year = NULL,
  ref_lines = NULL,
  ref_color = "#e34948",
  xlim = NULL,
  ylim = NULL,
  ylab = NULL,
  ncol = 2,
  strip_width = 30,
  legend = TRUE,
  legend_title = NULL,
  base_size = 11,
  base_family = "serif",
  ...
)
```

## Arguments

- slick:

  A Slick object.

- PI:

  Index or code of the time-series performance indicator.

- MPs:

  Codes or indices of the MPs to plot. Defaults to all.

- OMs:

  Codes or indices of the OMs. Values are pooled over the OMs unless
  `byOM = TRUE`. Defaults to the first OM.

- groups:

  Optional named list of MP codes; one panel per group.

- byOM:

  Logical. One panel per OM?

- MP_labels:

  Optional labels of the MPs (named by MP code), shown in the legend.

- MP_colors:

  Optional colours of the MPs (named by MP code).

- MeanMed:

  `'median'` or `'mean'`.

- quants1, quants2:

  Quantiles of the inner and outer ribbons.

- alpha1, alpha2:

  Transparency of the inner and outer ribbons.

- ref_year:

  Optional year of the first management advice (vertical line).

- ref_lines:

  Optional named numeric vector of horizontal reference lines.

- ref_color:

  Colour of the reference lines.

- xlim, ylim:

  Optional axis limits.

- ylab:

  Optional y-axis label. Defaults to the label of the PI.

- ncol:

  Number of columns of panels.

- strip_width:

  Width (characters) at which the OM names are wrapped when
  `byOM = TRUE`.

- legend:

  Logical. Show the MP legend? (e.g. `FALSE` when the panel titles name
  the MPs)

- legend_title:

  Title of the legend.

- base_size, base_family:

  Passed to
  [`theme_slick_print()`](https://wcpfcnswo.bluematterscience.com/pkg/reference/theme_slick_print.md).

- ...:

  Other arguments passed to
  [`Slick::plotTimeseries()`](https://slick.bluematterscience.com/reference/plotTimeseries.html).

## Value

A `ggplot2` object, or a `patchwork` object if `groups` is used.
