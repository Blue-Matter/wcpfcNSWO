# Replay example CMPs: how one CMP of each type sets the TAC in each
# management cycle of one simulation, with the data available at the time.
#
# MSEtool::ReplayMP() runs the CMP again on the data it received in each
# management year of the projection. The frames drawn here add the management
# timeline: the TAC for years Y to Y + Interval - 1 is calculated in Y - 1 from
# the data up to Y - DataLag - 1 (e.g. the 2031-2033 TAC is calculated in 2030
# from the data up to 2028).
#
# Outputs (analysis/figures/Replay):
#   <CMP>.gif           animation, one frame per management cycle
#   <CMP>_<Year>.png    the frame of one management year (FrameYear)
#   <CMP>_Table.csv     the values behind each TAC
#   Replay_Info.csv     the OM, simulation, and frame year
library(MSEtool)
library(ggplot2)

MSEDir <- 'objects/MSE'
FigDir <- 'analysis/figures/Replay'
dir.create(FigDir, recursive = TRUE, showWarnings = FALSE)

ReplayOM   <- '00'   # base case OM
ReplayCMPs <- c(IR = 'IR_H2_All_10', IT = 'IT_H2_All_10', SP = 'SP_H2_All_10')
# Simulation: NULL for the simulation with the lowest SB/SB0 under ReplayCMPs[['IR']]
# over the MP years, so the HCR comes into play (it rarely does in the base case)
ReplaySim  <- NULL
# Year of the static frame: NULL for the first management year in which the HCR
# reduces the IR trial TAC (multiplier < 1), or the second management year
FrameYear  <- NULL
FPS        <- 0.5    # frames (management cycles) per second

IndexLabels <- c(S2 = 'S2: JPN late longline, Area 1', S4 = 'S4: JPN late longline, Area 2',
                 S5 = 'S5: TWN distant-water longline', S8 = 'S8: US shallow-set longline')
Colours <- c(Data = '#9ecae1', Lag = 'grey55', TAC = '#e67e22', Estimate = '#e67e22')

MSEFile <- function(MP) file.path(MSEDir, ReplayOM, paste0(sub('^(\\w+?)_H\\d+_(\\w+?)_\\d+$', '\\1_\\2', MP), '.mse'))
fmt     <- function(x) format(round(x), big.mark = ',', trim = TRUE)

# ---- simulation and replay ----
MSE <- readRDS(MSEFile(ReplayCMPs[['IR']]))
OM  <- MSE@OM
Lag <- DataLag(OM)
Int <- Interval(OM)
# lowest annual SB/SB0 (mean over the seasons, equilibrium SB0, as in the performance
# metrics) of each simulation over the MP years, and the ThRP of the HCR of the IR CMP
MinSB <- MSEtool::SB_SB0(MSE, type = 'Equilibrium', df = TRUE, Reduce = FALSE)
MinSB <- MinSB[MinSB$MP == ReplayCMPs[['IR']] & as.character(MinSB$Stock) == 'Female' & MinSB$Year >= MPStartYear(OM), ]
MinSB$Year <- floor(MinSB$Year)
MinSB <- stats::aggregate(Value ~ Sim + Year, MinSB, mean)
MinSB <- stats::aggregate(Value ~ Sim, MinSB, min)
TuneSummary <- utils::read.csv(file.path('objects', 'Tune', 'Summary.csv'))
ThRP <- TuneSummary$ThRP[TuneSummary$Name == ReplayCMPs[['IR']]]
if (is.null(ReplaySim)) ReplaySim <- MinSB$Sim[which.min(MinSB$Value)]
rm(MSE); gc()

Replays <- list()
for (type in names(ReplayCMPs)) {
  MP  <- ReplayCMPs[[type]]
  MSE <- readRDS(MSEFile(MP))
  # SP: total biomass, to compare the estimated B/BMSY with the OM B/BMSY
  Replays[[type]] <- ReplayMP(MSE, MP = MP, Sim = ReplaySim, Status = if (type == 'SP') 'B' else 'SB')
  rm(MSE); gc()
}
if (is.null(FrameYear)) {
  Tab1 <- Replays$IR$Table
  FrameYear <- if (any(Tab1$HCRMultiplier < 1)) Tab1$Year[which(Tab1$HCRMultiplier < 1)[1]] else Tab1$Year[2]
}

# ---- frames ----
# timeline of year Y: data used, years without data when the TAC is calculated, and the TAC years
Timing <- function(Y) list(Data = Y - Lag - 1, Calc = Y - 1, First = Y, Last = Y + Int - 1)

# data of a layer of a static plot, by geom
LayerData <- function(p, Geom) {
  for (L in p$layers) if (inherits(L$geom, Geom) && is.data.frame(L$data) && nrow(L$data)) return(L$data)
  NULL
}

# add layers below the existing layers
Below <- function(p, ...) {
  n <- length(p$layers)
  for (L in list(...)) p <- p + L
  New <- seq_len(length(p$layers) - n) + n
  p$layers <- c(p$layers[New], p$layers[seq_len(n)])
  p
}

Shade <- function(t) list(
  annotate('rect', xmin = t$Data + 0.5, xmax = t$Calc + 0.5, ymin = -Inf, ymax = Inf, fill = Colours[['Lag']], alpha = 0.2),
  annotate('rect', xmin = t$First - 0.5, xmax = t$Last + 0.5, ymin = -Inf, ymax = Inf, fill = Colours[['TAC']], alpha = 0.15))

# management timeline above the plots: the data used, the years without data, the year the
# TAC is calculated, and the years it applies
TimelinePlot <- function(Y, Years) {
  t  <- Timing(Y)
  x0 <- min(Years) - Lag - 7
  Box <- data.frame(xmin = c(x0, t$Data + 0.5, t$First - 0.5), xmax = c(t$Data + 0.5, t$Calc + 0.5, t$Last + 0.5),
                    Fill = c('Data', 'Lag', 'TAC'))
  ggplot() +
    geom_rect(data = Box, aes(xmin = xmin, xmax = xmax, ymin = 0.15, ymax = 0.75, fill = Fill), alpha = 0.85) +
    annotate('text', x = t$Data, y = 0.45, label = paste('Data up to', t$Data), hjust = 1, size = 3.6) +
    annotate('text', x = t$First - 0.3, y = 1.2, label = paste0('TAC applies ', t$First, '-', t$Last),
             hjust = 0, size = 3.6, colour = '#a04f0b', fontface = 'bold') +
    annotate('point', x = t$Calc + 0.5, y = 0.95, shape = 25, size = 3, fill = 'black') +
    annotate('text', x = t$Calc + 0.2, y = 1.2, label = paste('TAC calculated in', t$Calc), hjust = 1, size = 3.6) +
    scale_fill_manual(values = Colours, guide = 'none') +
    scale_x_continuous(limits = c(x0, max(Years) + Int + 1), breaks = seq(2020, 2100, 5), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 1.5), expand = c(0, 0)) +
    labs(x = NULL, y = NULL) +
    theme_minimal(base_size = 11) +
    theme(panel.grid = element_blank(), axis.text.y = element_blank(),
          axis.line.x = element_line(colour = 'grey60'), axis.ticks.x = element_line(colour = 'grey60'))
}

ConstraintText <- function(x) {
  x <- sub('^Increase capped at DeltaUp = (.*)\\.$', 'increase limited to \\1', x)
  x <- sub('^Decrease capped at DeltaDown = (.*)\\.$', 'decrease limited to \\1', x)
  x <- sub('^Trial TAC applied without constraint\\.$', 'TAC = trial TAC', x)
  sub('^Change smaller than the minimum change: TAC unchanged\\.$', 'change < 1%, so the TAC is unchanged', x)
}

# the ReplayPlot() plots of year Y, with the management timeline
Frame <- function(R, type, Y) {
  t   <- Timing(Y)
  Tab <- R$Table
  Row <- Tab[Tab$Year == Y, ]
  P   <- suppressMessages(ReplayPlot(R, Y))
  Strip <- function(p, Title) Below(p, Shade(t)) + labs(title = Title)

  if (!is.null(P$Index)) {
    Names <- unique(as.character(LayerData(P$Index, 'GeomPoint')$Name))
    Lab   <- stats::setNames(ifelse(sub('_.*', '', Names) %in% names(IndexLabels),
                                    IndexLabels[sub('_.*', '', Names)], sub('_.*', '', Names)), Names)
    P$Index <- Strip(P$Index, paste('Indices of abundance: data up to', t$Data)) +
      facet_wrap(~Name, scales = 'free_y', ncol = 2, labeller = as_labeller(Lab))
    if (type != 'SP')
      P$Index <- suppressMessages(P$Index + scale_linetype_manual(
        values = c(IndexTarget = 'solid', 'HCR control points' = 'dashed'),
        labels = c(IndexTarget = 'Index at SB0', 'HCR control points' = 'LRP and ThRP')))
  }

  P$Status <- Strip(P$Status, 'True stock status (operating model)')
  if (type != 'SP') {
    # the CMP's estimate of SB/SB0 (index / index at SB0), at the last data year of each decision
    Est <- data.frame(Year = Tab$Year - Lag - 1, Value = Tab$IndexRatio,
                      What = ifelse(Tab$Year == Y, 'CMP estimate used for this TAC', 'Earlier CMP estimates'))[Tab$Year <= Y, ]
    P$Status <- P$Status +
      geom_point(data = Est, aes(x = Year, y = Value, shape = What), colour = Colours[['Estimate']],
                 fill = Colours[['Estimate']], size = 3, stroke = 1.1, inherit.aes = FALSE) +
      scale_shape_manual(values = c('CMP estimate used for this TAC' = 21, 'Earlier CMP estimates' = 1),
                         breaks = c('CMP estimate used for this TAC', 'Earlier CMP estimates'), name = NULL)
  }

  if (!is.null(P$HCR)) {
    P$HCR <- P$HCR + labs(title = 'Harvest control rule')
    if (type != 'SP') P$HCR <- P$HCR + labs(x = 'Estimated SB/SB0 (index / index at SB0)')
  }

  P$Catch <- Strip(P$Catch, 'Catch and TAC') +
    annotate('segment', x = t$First - 0.5, xend = t$Last + 0.5, y = Row$TAC, yend = Row$TAC,
             colour = Colours[['TAC']], linewidth = 1.4)

  Title <- sprintf('%s: TAC for %d-%d = %s t', R$MP, t$First, t$Last, fmt(Row$TAC))
  Sub   <- sprintf('Calculated in %d from the data up to %d. Trial TAC %s t; %s.',
                   t$Calc, t$Data, fmt(Row$TrialTAC), ConstraintText(Row$Constraint))
  Caption <- sprintf(paste0('Simulation %d of the base case OM. Grey shading: years without data when the TAC is calculated ',
                            '(%d-year data lag). Orange shading: the years the TAC applies.'), R$Sim, Lag)
  Panels <- patchwork::wrap_plots(P[intersect(c('Index', 'Status', 'HCR', 'Catch'), names(P))], ncol = 2)
  patchwork::wrap_plots(TimelinePlot(Y, Tab$Year), Panels, ncol = 1, heights = c(0.1, 1)) +
    patchwork::plot_annotation(title = Title, subtitle = Sub, caption = Caption,
                               theme = theme(plot.title = element_text(face = 'bold', size = 17),
                                             plot.subtitle = element_text(size = 12.5, colour = 'grey20'),
                                             plot.caption = element_text(size = 10, colour = 'grey30', hjust = 0)))
}

# ---- outputs ----
Width <- 12; Height <- 10.5; Res <- 90
for (type in names(Replays)) {
  R <- Replays[[type]]
  MP <- R$MP
  cli::cli_inform("Drawing {.val {MP}}")
  ggsave(file.path(FigDir, paste0(MP, '_', FrameYear, '.png')), Frame(R, type, FrameYear),
         width = Width, height = Height, dpi = Res, bg = 'white')
  gifski::save_gif(for (Y in R$Table$Year) print(Frame(R, type, Y)),
                   gif_file = file.path(FigDir, paste0(MP, '.gif')),
                   width = Width * Res, height = Height * Res, res = Res, delay = 1 / FPS, progress = FALSE)

  # true status at the last data year of each decision: SB/SB0 (IR, IT) or B/BMSY (SP)
  Last  <- suppressMessages(ReplayPlot(R, max(R$Table$Year), 'Status'))
  OMS   <- LayerData(Last, 'GeomLine')
  OMS   <- OMS[grepl('^Operating model', OMS$Series), ]
  K     <- if (type == 'SP') LayerData(Last, 'GeomHline')$yintercept[1] else 1
  Tab   <- R$Table
  Tab$CalcYear <- Tab$Year - 1
  Tab$DataYear <- Tab$Year - Lag - 1
  Tab$TrueStatus <- OMS$Value[match(Tab$DataYear, OMS$Year)] / K
  utils::write.csv(Tab, file.path(FigDir, paste0(MP, '_Table.csv')), row.names = FALSE)
}
utils::write.csv(data.frame(OM = ReplayOM, Sim = ReplaySim, FrameYear = FrameYear, DataLag = Lag, Interval = Int,
                            nSim = nrow(MinSB), ThRP = ThRP, nBelowThRP = sum(MinSB$Value < ThRP),
                            IR = ReplayCMPs[['IR']], IT = ReplayCMPs[['IT']], SP = ReplayCMPs[['SP']]),
                 file.path(FigDir, 'Replay_Info.csv'), row.names = FALSE)
