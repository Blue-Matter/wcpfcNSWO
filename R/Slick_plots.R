# Wrappers around the Slick plotting functions, for report and presentation
# figures.

# ---- Helpers ----

#' Print theme for Slick figures
#'
#' A `ggplot2` theme for figures in reports and presentations: black text,
#' light grid, plain facet strips, and the legend at the bottom.
#'
#' @param base_size Base font size.
#' @param base_family Font family (e.g. `'serif'` for Times New Roman in Word).
#' @return A `ggplot2` theme.
#' @export
theme_slick_print <- function(base_size = 11, base_family = 'serif') {
  ggplot2::theme_bw(base_size = base_size, base_family = base_family) +
    ggplot2::theme(text             = ggplot2::element_text(colour = 'black'),
                   axis.text        = ggplot2::element_text(colour = 'black', size = base_size - 1),
                   axis.title       = ggplot2::element_text(size = base_size, face = 'plain'),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_line(colour = 'grey92'),
                   strip.background = ggplot2::element_rect(fill = 'grey95', colour = 'grey70'),
                   strip.text       = ggplot2::element_text(colour = 'black', size = base_size - 1, face = 'plain'),
                   plot.title       = ggplot2::element_text(size = base_size, hjust = 0.5),
                   legend.position  = 'bottom')
}

# Index of MPs in a Slick object, from codes or indices
.MPIndex <- function(slick, MPs) {
  if (is.null(MPs)) return(seq_along(Slick::Code(Slick::MPs(slick))))
  if (is.character(MPs)) {
    ind <- match(MPs, Slick::Code(Slick::MPs(slick)))
    if (anyNA(ind)) cli::cli_abort('MPs not found in the Slick object: {MPs[is.na(ind)]}')
    return(ind)
  }
  MPs
}

# Index of OMs, from OM codes (first column of the OM design) or indices
.OMIndex <- function(slick, OMs) {
  nOM <- nrow(Slick::Design(Slick::OMs(slick)))
  if (is.null(OMs)) return(seq_len(nOM))
  if (is.character(OMs)) {
    codes <- as.character(Slick::Design(Slick::OMs(slick))[[1]])
    ind <- match(OMs, codes)
    if (anyNA(ind)) cli::cli_abort('OMs not found in the Slick object: {OMs[is.na(ind)]}')
    return(ind)
  }
  OMs
}

# Replace the labels and colours of the MPs (all MPs in the Slick object)
.SetMPs <- function(slick, MP_labels = NULL, MP_colors = NULL) {
  mps <- Slick::MPs(slick)
  if (!is.null(MP_labels)) Slick::Label(mps) <- unname(MP_labels)
  if (!is.null(MP_colors)) Slick::Color(mps) <- unname(MP_colors)
  Slick::MPs(slick) <- mps
  slick
}

# Expand per-MP values (named by MP code, or one per MP) to all MPs
.PerMP <- function(slick, x) {
  if (is.null(x)) return(NULL)
  codes <- Slick::Code(Slick::MPs(slick))
  if (!is.null(names(x))) {
    out <- x[codes]
    out[is.na(out)] <- x[1]
    return(unname(out))
  }
  rep_len(x, length(codes))
}

# ---- Time series ----

#' Time series of a performance indicator, with uncertainty ribbons
#'
#' Plots the median (or mean) and quantile ribbons of a time-series performance
#' indicator for a set of MPs, using [Slick::plotTimeseries()]. The MPs can be
#' split into groups, with one panel per group and the MPs of each group shown
#' together with ribbons in the colour of each MP (e.g. one panel per HCR, with
#' one line and ribbon per CMP type), or shown with one panel per OM.
#'
#' @param slick A Slick object.
#' @param PI Index or code of the time-series performance indicator.
#' @param MPs Codes or indices of the MPs to plot. Defaults to all.
#' @param OMs Codes or indices of the OMs. Values are pooled over the OMs
#'   unless `byOM = TRUE`. Defaults to the first OM.
#' @param groups Optional named list of MP codes; one panel per group.
#' @param byOM Logical. One panel per OM?
#' @param MP_labels Optional labels of the MPs (named by MP code), shown in the legend.
#' @param MP_colors Optional colours of the MPs (named by MP code).
#' @param MeanMed `'median'` or `'mean'`.
#' @param quants1,quants2 Quantiles of the inner and outer ribbons.
#' @param alpha1,alpha2 Transparency of the inner and outer ribbons.
#' @param ref_year Optional year of the first management advice (vertical line).
#' @param ref_lines Optional named numeric vector of horizontal reference lines.
#' @param ref_color Colour of the reference lines.
#' @param xlim,ylim Optional axis limits.
#' @param ylab Optional y-axis label. Defaults to the label of the PI.
#' @param ncol Number of columns of panels.
#' @param strip_width Width (characters) at which the OM names are wrapped when `byOM = TRUE`.
#' @param legend Logical. Show the MP legend? (e.g. `FALSE` when the panel titles name the MPs)
#' @param legend_title Title of the legend.
#' @param base_size,base_family Passed to [theme_slick_print()].
#' @param ... Other arguments passed to [Slick::plotTimeseries()].
#' @return A `ggplot2` object, or a `patchwork` object if `groups` is used.
#' @export
PlotTimeseries <- function(slick, PI = 1, MPs = NULL, OMs = 1, groups = NULL, byOM = FALSE,
                           MP_labels = NULL, MP_colors = NULL, MeanMed = 'median',
                           quants1 = c(0.25, 0.75), quants2 = c(0.05, 0.95),
                           alpha1 = 0.35, alpha2 = 0.15,
                           ref_year = NULL, ref_lines = NULL, ref_color = '#e34948',
                           xlim = NULL, ylim = NULL, ylab = NULL, ncol = 2, strip_width = 30,
                           legend = TRUE, legend_title = NULL, base_size = 11, base_family = 'serif', ...) {
  if (is.character(PI)) PI <- match(PI, Slick::Code(Slick::Timeseries(slick)))
  slick <- .SetMPs(slick, .PerMP(slick, MP_labels), .PerMP(slick, MP_colors))
  OMind <- .OMIndex(slick, OMs)
  if (is.null(ylab)) ylab <- Slick::Label(Slick::Timeseries(slick))[PI]

  One <- function(mp_ind, title = NULL) {
    s <- Slick::FilterSlick(slick, MPs = mp_ind, OMs = OMind, plot = 'Timeseries')
    p <- Slick::plotTimeseries(s, PI = PI, byMP = FALSE, byOM = byOM, includeHist = TRUE,
                               includeQuants = TRUE, includeLabels = FALSE, MeanMed = MeanMed,
                               quants1 = quants1, quants2 = quants2, alpha1 = alpha1, alpha2 = alpha2,
                               MP_label = if (is.null(MP_labels)) 'Code' else 'Label',
                               ncol = ncol, col_line = 'grey40', fill_ribbon1 = 'grey80',
                               col_ribbon1 = NA, fill_ribbon2 = 'grey90', col_ribbon2 = NA,
                               linewidth.median.line = 0.7, ...)
    # Slick hides the MP legend and draws Target/Limit lines; show the legend instead
    p <- p + ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(linewidth = 1)),
                             fill = 'none') +
      theme_slick_print(base_size, base_family) +
      ggplot2::labs(y = ylab, x = NULL, colour = legend_title, title = title)
    # the Target/Limit labels of Slick (ggrepel) in the print font and size
    for (i in seq_along(p$layers))
      if (inherits(p$layers[[i]]$geom, 'GeomTextRepel'))
        p$layers[[i]]$aes_params[c('size', 'family')] <- list((base_size - 2) / ggplot2::.pt, base_family)
    if (!legend) p <- p + ggplot2::theme(legend.position = 'none')
    if (byOM)
      p <- p + ggplot2::facet_wrap(~OM, ncol = ncol, labeller = ggplot2::label_wrap_gen(strip_width))
    if (!is.null(ref_year))
      p <- p + ggplot2::geom_vline(xintercept = ref_year, linetype = 3, colour = 'grey30')
    if (!is.null(ref_lines))
      p <- p + ggplot2::geom_hline(yintercept = ref_lines, linetype = 2, colour = ref_color)
    if (!is.null(xlim) || !is.null(ylim))
      p <- p + ggplot2::coord_cartesian(xlim = xlim, ylim = ylim)
    p
  }

  if (is.null(groups)) return(One(.MPIndex(slick, MPs)))

  if (!requireNamespace('patchwork', quietly = TRUE))
    cli::cli_abort('The `patchwork` package is required for `groups`')
  plots <- lapply(names(groups), \(g) One(.MPIndex(slick, groups[[g]]), g) +
                    ggplot2::theme(plot.title = ggplot2::element_text(size = base_size - 1)))
  patchwork::wrap_plots(plots, ncol = ncol, guides = 'collect', axis_titles = 'collect') &
    ggplot2::theme(legend.position = if (legend) 'bottom' else 'none')
}

# ---- Trade-off ----

#' Trade-off between two performance indicators
#'
#' Plots the mean (over the selected OMs) of two performance indicators for each
#' MP, using the data of [Slick::plotTradeoff()], with control over the colour
#' and shape of the points, labels, reference lines, and axis limits.
#'
#' @param slick A Slick object.
#' @param xPI,yPI Index or code of the Tradeoff performance indicators.
#' @param MPs Codes or indices of the MPs. Defaults to all.
#' @param OMs Codes or indices of the OMs to average over. Defaults to the first OM.
#' @param color_by,shape_by Optional vectors (one per MP, or named by MP code)
#'   that set the colour and shape of the points.
#' @param colors Optional colours for the levels of `color_by`.
#' @param shapes Optional shapes for the levels of `shape_by`.
#' @param labels Logical, or a character vector of the MP codes to label.
#' @param ref_x,ref_y Optional reference lines.
#' @param xlim,ylim Optional axis limits.
#' @param xlab,ylab Optional axis labels.
#' @param color_title,shape_title Legend titles.
#' @param point_size Size of the points.
#' @param base_size,base_family Passed to [theme_slick_print()].
#' @return A `ggplot2` object.
#' @export
PlotTradeoff <- function(slick, xPI = 1, yPI = 2, MPs = NULL, OMs = 1,
                         color_by = NULL, shape_by = NULL, colors = NULL, shapes = NULL,
                         labels = FALSE, ref_x = NULL, ref_y = NULL, xlim = NULL, ylim = NULL,
                         xlab = NULL, ylab = NULL, color_title = NULL, shape_title = NULL,
                         point_size = 2.8, base_size = 11, base_family = 'serif') {
  codes <- Slick::Code(Slick::Tradeoff(slick))
  if (is.character(xPI)) xPI <- match(xPI, codes)
  if (is.character(yPI)) yPI <- match(yPI, codes)
  mp_ind <- .MPIndex(slick, MPs)
  s <- Slick::FilterSlick(slick, MPs = mp_ind, OMs = .OMIndex(slick, OMs), plot = 'Tradeoff')

  # the data of the Slick plot (mean over the OMs), so the values are those shown in Slick
  base <- Slick::plotTradeoff(s, xPI = xPI, yPI = yPI)
  d <- base$data
  d$Code <- Slick::Code(Slick::MPs(s))
  Lab <- function(v) if (is.null(v)) NULL else if (is.null(names(v))) v[mp_ind] else v[d$Code]
  d$Colour <- Lab(color_by)
  d$Shape  <- Lab(shape_by)

  aes_pt <- ggplot2::aes(x = x, y = y)
  if (!is.null(d$Colour)) aes_pt$colour <- quote(Colour)
  if (!is.null(d$Shape)) aes_pt$shape <- quote(Shape)

  p <- ggplot2::ggplot(d, aes_pt)
  if (!is.null(ref_x)) p <- p + ggplot2::geom_vline(xintercept = ref_x, linetype = 2, colour = 'grey40')
  if (!is.null(ref_y)) p <- p + ggplot2::geom_hline(yintercept = ref_y, linetype = 2, colour = 'grey40')
  p <- p + ggplot2::geom_point(size = point_size, stroke = 0.9)
  if (!isFALSE(labels)) {
    dl <- if (isTRUE(labels)) d else d[d$Code %in% labels, ]
    p <- p + ggrepel::geom_text_repel(data = dl, ggplot2::aes(label = Code), colour = 'black',
                                      size = (base_size - 2) / ggplot2::.pt, family = base_family,
                                      inherit.aes = TRUE, show.legend = FALSE)
  }
  if (!is.null(colors)) p <- p + ggplot2::scale_colour_manual(values = colors)
  if (!is.null(shapes)) p <- p + ggplot2::scale_shape_manual(values = shapes)
  pi_lab <- Slick::Label(Slick::Tradeoff(s))
  p + ggplot2::coord_cartesian(xlim = xlim, ylim = ylim) +
    ggplot2::labs(x = if (is.null(xlab)) pi_lab[xPI] else xlab, y = if (is.null(ylab)) pi_lab[yPI] else ylab,
                  colour = color_title, shape = shape_title) +
    theme_slick_print(base_size, base_family) +
    ggplot2::theme(legend.box = 'vertical')
}

# ---- Quilt as a heat map, by OM ----

#' Quilt values by OM and MP, as a heat map
#'
#' Plots one Quilt performance indicator for each OM (columns) and MP (rows),
#' with the value in each cell. Values below a minimum (e.g. the minimum
#' probability of an objective) are shown in bold.
#'
#' @param slick A Slick object.
#' @param PI Index or code of the Quilt performance indicator.
#' @param MPs Codes or indices of the MPs (rows, in this order). Defaults to all.
#' @param OMs Codes or indices of the OMs (columns). Defaults to all.
#' @param minimum Optional value below which values are shown in bold.
#' @param limits Limits of the colour scale.
#' @param low,high Colours of the ends of the (single-hue) colour scale.
#' @param digits Number of decimal places.
#' @param fill_title Title of the colour legend. Defaults to the label of the PI.
#' @param OM_label `'code'` (first column of the OM design) or `'name'`.
#' @param base_size,base_family Passed to [theme_slick_print()].
#' @return A `ggplot2` object.
#' @export
PlotQuiltOM <- function(slick, PI = 1, MPs = NULL, OMs = NULL, minimum = NULL, limits = c(0, 1),
                        low = '#f2f6fc', high = '#174a8b', digits = 2, fill_title = NULL,
                        OM_label = c('code', 'name'), base_size = 11, base_family = 'serif') {
  OM_label <- match.arg(OM_label)
  quilt <- Slick::Quilt(slick)
  if (is.character(PI)) PI <- match(PI, Slick::Code(quilt))
  mp_ind <- .MPIndex(slick, MPs)
  om_ind <- .OMIndex(slick, OMs)
  V <- Slick::Value(quilt)[om_ind, mp_ind, PI, drop = FALSE]   # OM x MP x PI
  design <- Slick::Design(Slick::OMs(slick))
  om_lab <- if (OM_label == 'code') as.character(design[[1]])[om_ind] else rownames(design)[om_ind]
  mp_lab <- Slick::Code(Slick::MPs(slick))[mp_ind]
  d <- data.frame(OM = factor(rep(om_lab, length(mp_ind)), levels = om_lab),
                  MP = factor(rep(mp_lab, each = length(om_ind)), levels = rev(mp_lab)),
                  Value = as.vector(V[, , 1]))
  d$Bold <- if (is.null(minimum)) FALSE else round(d$Value, digits) < minimum
  d$Dark <- d$Value > mean(limits) + 0.05 * diff(limits)
  ggplot2::ggplot(d, ggplot2::aes(OM, MP, fill = Value)) +
    ggplot2::geom_tile(colour = 'white', linewidth = 0.6) +
    ggplot2::geom_text(ggplot2::aes(label = formatC(Value, format = 'f', digits = digits),
                                    fontface = ifelse(Bold, 'bold', 'plain'), colour = Dark),
                       size = (base_size - 3) / ggplot2::.pt, family = base_family, show.legend = FALSE) +
    ggplot2::scale_fill_gradient(low = low, high = high, limits = limits, oob = scales::squish) +
    ggplot2::scale_colour_manual(values = c(`TRUE` = 'white', `FALSE` = 'black')) +
    ggplot2::labs(x = 'Operating model', y = NULL,
                  fill = if (is.null(fill_title)) Slick::Label(quilt)[PI] else fill_title) +
    theme_slick_print(base_size, base_family) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(), legend.position = 'right')
}

#' Quilt values of each MP across OMs, as a dot plot
#'
#' Plots one Quilt performance indicator for each MP and OM, with the value in a
#' reference OM (e.g. the Base Case) highlighted, to show how much the
#' performance of each MP varies among the OMs.
#'
#' Candidate Slick plot: a by-OM view of the Quilt values (see [PlotQuiltOM()]).
#'
#' @param slick A Slick object.
#' @param PI Index or code of the Quilt performance indicator.
#' @param MPs Codes or indices of the MPs. Defaults to all.
#' @param OMs Codes or indices of the OMs. Defaults to all.
#' @param ref_OM Code or index of the reference OM, highlighted.
#' @param x_by,color_by Vectors (one per MP, or named by MP code) giving the
#'   x-axis position and colour of each MP (e.g. HCR and MP type).
#' @param colors Optional colours for the levels of `color_by`.
#' @param ylab,xlab,color_title Labels.
#' @param base_size,base_family Passed to [theme_slick_print()].
#' @return A `ggplot2` object.
#' @export
PlotQuiltDots <- function(slick, PI = 1, MPs = NULL, OMs = NULL, ref_OM = 1, x_by = NULL, color_by = NULL,
                          colors = NULL, ylab = NULL, xlab = NULL, color_title = NULL,
                          base_size = 11, base_family = 'serif') {
  quilt <- Slick::Quilt(slick)
  if (is.character(PI)) PI <- match(PI, Slick::Code(quilt))
  mp_ind <- .MPIndex(slick, MPs)
  om_ind <- .OMIndex(slick, OMs)
  ref_ind <- .OMIndex(slick, ref_OM)
  codes <- Slick::Code(Slick::MPs(slick))[mp_ind]
  V <- Slick::Value(quilt)[om_ind, mp_ind, PI, drop = FALSE]
  Lab <- function(v) if (is.null(v)) codes else if (is.null(names(v))) v[mp_ind] else v[codes]
  d <- data.frame(OM = rep(om_ind, length(mp_ind)), Code = rep(codes, each = length(om_ind)),
                  X = rep(Lab(x_by), each = length(om_ind)), Colour = rep(Lab(color_by), each = length(om_ind)),
                  Value = as.vector(V[, , 1]))
  d$Ref <- d$OM == ref_ind
  pos <- ggplot2::position_dodge(width = 0.6)
  p <- ggplot2::ggplot(d, ggplot2::aes(X, Value, colour = Colour)) +
    ggplot2::geom_point(data = d[!d$Ref, ], position = pos, size = 1.3, alpha = 0.55) +
    ggplot2::geom_point(data = d[d$Ref, ], position = pos, size = 3, shape = 18)
  if (!is.null(colors)) p <- p + ggplot2::scale_colour_manual(values = colors)
  p + ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::labs(x = xlab, y = if (is.null(ylab)) Slick::Label(quilt)[PI] else ylab, colour = color_title) +
    theme_slick_print(base_size, base_family)
}

#' Violin plots of a Boxplot performance indicator, pooled over OMs
#'
#' Wraps [Slick::plotBoxplot()] (`type = 'violin'`). Each violin shows the
#' distribution of a per-simulation performance indicator for one MP, pooled
#' over the simulations of the selected OMs, so its spread includes both the
#' simulation and the OM uncertainty. The mean of each OM can be overlaid as
#' points, with a reference OM (e.g. the Base Case) highlighted, which shows
#' the robustness of each MP across the OMs on the same plot.
#'
#' @param slick A Slick object.
#' @param PI Index or code of the Boxplot performance indicator.
#' @param MPs Codes or indices of the MPs. Defaults to all.
#' @param OMs Codes or indices of the OMs to pool. Defaults to all.
#' @param pool `'pool'` or `'mean'` (see Details).
#' @param x_by,fill_by Vectors (one per MP, or named by MP code) giving the
#'   x-axis group and fill of each MP. Defaults: MP code, and no grouping.
#' @param colors Optional colours for the levels of `fill_by`.
#' @param OM_means Overlay the mean of each OM as points?
#' @param ref_OM Code or index of the OM highlighted (diamond) among the OM
#'   means. `NULL` for none.
#' @param quants Quantiles drawn as lines inside the violins (`NULL` for none).
#' @param alpha Transparency of the violin fill.
#' @param ylim Optional y-axis limits.
#' @param ylab,xlab,fill_title Labels.
#' @param base_size,base_family Passed to [theme_slick_print()].
#' @return A `ggplot2` object.
#' @export
PlotViolin <- function(slick, PI = 1, MPs = NULL, OMs = NULL, pool = c('pool', 'mean'),
                       x_by = NULL, fill_by = NULL, colors = NULL, OM_means = TRUE, ref_OM = 1,
                       quants = c(0.25, 0.5, 0.75), alpha = 0.45, ylim = NULL,
                       ylab = NULL, xlab = NULL, fill_title = NULL,
                       base_size = 11, base_family = 'serif') {
  pool <- match.arg(pool)
  box <- Slick::Boxplot(slick)
  if (is.character(PI)) PI <- match(PI, Slick::Code(box))
  mp_ind <- .MPIndex(slick, MPs)
  om_ind <- .OMIndex(slick, OMs)
  codes <- Slick::Code(Slick::MPs(slick))

  # per-simulation values from Slick::plotBoxplot (byOM = TRUE keeps every OM)
  sl <- slick
  Slick::Label(Slick::MPs(sl)) <- codes
  d <- suppressWarnings(Slick::plotBoxplot(sl, PI = PI, type = 'violin', byOM = TRUE, MP_label = 'Label'))$data
  om_lab <- levels(d$OM)
  d <- d[as.character(d$MP) %in% codes[mp_ind] & as.character(d$OM) %in% om_lab[om_ind], ]
  d$Code <- as.character(d$MP)
  d$OMi <- match(as.character(d$OM), om_lab)
  if (pool == 'mean') d <- stats::aggregate(value ~ Sim + Code, data = d, FUN = mean)

  Lab <- function(v, default) {
    if (is.null(v)) return(setNames(default, codes[mp_ind]))
    if (is.null(names(v))) setNames(v[mp_ind], codes[mp_ind]) else v[codes[mp_ind]]
  }
  X <- Lab(x_by, codes[mp_ind])
  Fill <- Lab(fill_by, codes[mp_ind])
  d$X <- factor(X[d$Code], levels = unique(X))
  d$Fill <- factor(Fill[d$Code], levels = unique(Fill))
  d$Code <- factor(d$Code, levels = codes[mp_ind])

  dodge <- if (is.null(x_by) || is.null(fill_by)) 0 else 0.8
  pos <- ggplot2::position_dodge(width = dodge)
  p <- ggplot2::ggplot(d, ggplot2::aes(X, value, fill = Fill, group = Code)) +
    ggplot2::geom_violin(scale = 'width', width = if (dodge) 0.75 else 0.8, position = pos,
                         alpha = alpha, colour = 'grey30', linewidth = 0.3,
                         draw_quantiles = quants)

  if (OM_means && pool == 'pool') {
    m <- stats::aggregate(value ~ Code + OMi + X + Fill, data = d, FUN = mean)
    ref <- if (is.null(ref_OM)) -1 else .OMIndex(slick, ref_OM)
    p <- p +
      ggplot2::geom_point(data = m[m$OMi != ref, ], ggplot2::aes(colour = Fill), position = pos,
                          size = 1.1, alpha = 0.9, show.legend = FALSE) +
      ggplot2::geom_point(data = m[m$OMi == ref, ], position = pos, shape = 23, size = 2.6,
                          fill = 'white', colour = 'black', show.legend = FALSE)
    if (!is.null(colors)) p <- p + ggplot2::scale_colour_manual(values = colors)
  }
  if (!is.null(colors)) p <- p + ggplot2::scale_fill_manual(values = colors)
  p + ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::coord_cartesian(ylim = ylim) +
    ggplot2::labs(x = xlab, y = if (is.null(ylab)) Slick::Label(box)[PI] else ylab,
                  fill = fill_title) +
    theme_slick_print(base_size, base_family)
}
