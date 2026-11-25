# Violin plots of a Boxplot performance indicator, pooled over OMs

Wraps
[`Slick::plotBoxplot()`](https://slick.bluematterscience.com/reference/plotBoxplot.html)
(`type = 'violin'`). Each violin shows the distribution of a
per-simulation performance indicator for one MP, pooled over the
simulations of the selected OMs, so its spread includes both the
simulation and the OM uncertainty. The mean of each OM can be overlaid
as points, with a reference OM (e.g. the Base Case) highlighted, which
shows the robustness of each MP across the OMs on the same plot.

## Usage

``` r
PlotViolin(
  slick,
  PI = 1,
  MPs = NULL,
  OMs = NULL,
  pool = c("pool", "mean"),
  x_by = NULL,
  fill_by = NULL,
  colors = NULL,
  OM_means = TRUE,
  ref_OM = 1,
  quants = c(0.25, 0.5, 0.75),
  alpha = 0.45,
  ylim = NULL,
  ylab = NULL,
  xlab = NULL,
  fill_title = NULL,
  base_size = 11,
  base_family = "serif"
)
```

## Arguments

- slick:

  A Slick object.

- PI:

  Index or code of the Boxplot performance indicator.

- MPs:

  Codes or indices of the MPs. Defaults to all.

- OMs:

  Codes or indices of the OMs to pool. Defaults to all.

- pool:

  `'pool'` or `'mean'` (see Details).

- x_by, fill_by:

  Vectors (one per MP, or named by MP code) giving the x-axis group and
  fill of each MP. Defaults: MP code, and no grouping.

- colors:

  Optional colours for the levels of `fill_by`.

- OM_means:

  Overlay the mean of each OM as points?

- ref_OM:

  Code or index of the OM highlighted (diamond) among the OM means.
  `NULL` for none.

- quants:

  Quantiles drawn as lines inside the violins (`NULL` for none).

- alpha:

  Transparency of the violin fill.

- ylim:

  Optional y-axis limits.

- ylab, xlab, fill_title:

  Labels.

- base_size, base_family:

  Passed to
  [`theme_slick_print()`](https://wcpfcnswo.bluematterscience.com/pkg/reference/theme_slick_print.md).

## Value

A `ggplot2` object.
