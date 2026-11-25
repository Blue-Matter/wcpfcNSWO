# Replay example CMPs: how one CMP of each type sets the TAC in each
# management cycle of one simulation, with the data available at the time.
#
# Outputs (analysis/figures/Replay):
#   <CMP>.gif           animation, one frame per management cycle
#   <CMP>_<Year>.png    the frame of one management year (FrameYear)
#   <CMP>_Table.csv     the values behind each TAC
#   Replay_Info.csv     the OM, simulation, and frame year
library(MSEtool)

MSEDir <- 'objects/MSE'
FigDir <- 'analysis/figures/Replay'
dir.create(FigDir, recursive = TRUE, showWarnings = FALSE)

ReplayOM   <- '00'   # base case OM
ReplayCMPs <- c(IR = 'IR_H2_All_10', IT = 'IT_H2_All_10', SP = 'SP_H2_All_10')

# Simulation: NULL for the simulation with the lowest SB/SB0 under ReplayCMPs[['IR']]
# over the MP years, so the HCR comes into play
ReplaySim  <- NULL

# Year of the static frame: NULL for the first management year in which the HCR
# reduces the IR trial TAC (multiplier < 1), or the second management year
FrameYear  <- NULL
FPS        <- 0.5    # frames (management cycles) per second

IndexLabels <- c(S2 = 'S2: JPN late longline, Area 1', S4 = 'S4: JPN late longline, Area 2',
                 S5 = 'S5: TWN distant-water longline', S8 = 'S8: US shallow-set longline')

MSEFile <- function(MP) file.path(MSEDir, ReplayOM, paste0(sub('^(\\w+?)_H\\d+_(\\w+?)_\\d+$', '\\1_\\2', MP), '.mse'))

# ---- simulation and replay ----
MSE <- readRDS(MSEFile(ReplayCMPs[['IR']]))
OM  <- MSE@OM
Lag <- DataLag(OM)
Int <- Interval(OM)
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
  Replays[[type]] <- ReplayMP(MSE, MP = MP, Sim = ReplaySim, Status = if (type == 'SP') 'B' else 'SB')
  rm(MSE); gc()
}
if (is.null(FrameYear)) {
  Tab1 <- Replays$IR$Table
  FrameYear <- if (any(Tab1$HCRMultiplier < 1)) Tab1$Year[which(Tab1$HCRMultiplier < 1)[1]] else Tab1$Year[2]
}

# ---- outputs ----
Labels <- function(R) {
  Names <- unique(unlist(lapply(R$Animations$Index$layers, \(L)
    if (is.data.frame(L$data) && 'Name' %in% names(L$data)) as.character(L$data$Name))))
  Code <- sub('_.*', '', Names)
  stats::setNames(IndexLabels[Code], Names)[Code %in% names(IndexLabels)]
}
Caption <- sprintf(paste0('Simulation %d of the base case OM. Grey shading: no data available when the TAC is calculated ',
                          '(%d-year data lag). Orange shading: the years the TAC applies to.\nVertical lines: end of the ',
                          'historical period (solid) and first year of MP advice (dashed).'), ReplaySim, Lag)

IndexCMP <- function(R) function(Panels, Year) {
  Tab <- R$Table
  Est <- data.frame(Year = Tab$Year - Lag - 1, Value = Tab$IndexRatio,
                    What = ifelse(Tab$Year == Year, 'CMP estimate used for this TAC', 'Earlier CMP estimates'))
  Panels$Status <- Panels$Status +
    ggplot2::geom_point(data = Est[Tab$Year <= Year, ], ggplot2::aes(x = Year, y = Value, shape = What),
                        colour = '#e67e22', fill = '#e67e22', size = 3, stroke = 1.1, inherit.aes = FALSE) +
    ggplot2::scale_shape_manual(values = c('CMP estimate used for this TAC' = 21, 'Earlier CMP estimates' = 1),
                                breaks = c('CMP estimate used for this TAC', 'Earlier CMP estimates'), name = NULL)
  Panels$HCR <- Panels$HCR + ggplot2::labs(x = 'Estimated SB/SB0 (index / index at SB0)')
  Panels$Index <- suppressMessages(Panels$Index + ggplot2::scale_linetype_manual(
    values = c(IndexTarget = 'solid', 'HCR control points' = 'dashed'),
    labels = c(IndexTarget = 'Index at SB0', 'HCR control points' = 'LRP and ThRP')))
  Panels
}

Width <- 12; Height <- 10.5; Res <- 90
for (type in names(Replays)) {
  R  <- Replays[[type]]
  MP <- R$MP
  IL <- Labels(R)
  Mod <- if (type != 'SP') IndexCMP(R)
  cli::cli_inform("Drawing {.val {MP}}")
  ggplot2::ggsave(file.path(FigDir, paste0(MP, '_', FrameYear, '.png')),
                  ReplayPlot(R, FrameYear, Caption = Caption, IndexLabels = IL, Modify = Mod),
                  width = Width, height = Height, dpi = Res, bg = 'white')
  ReplayAnimate(R, fps = FPS, width = Width * Res, height = Height * Res, res = Res,
                file = file.path(FigDir, paste0(MP, '.gif')), Caption = Caption, IndexLabels = IL, Modify = Mod)

  Status <- suppressMessages(ReplayPanels(R, max(R$Table$Year), 'Status'))
  Layer  <- function(Geom) for (L in Status$layers)
    if (inherits(L$geom, Geom) && is.data.frame(L$data) && nrow(L$data)) return(L$data)
  OMS <- Layer('GeomLine')
  OMS <- OMS[grepl('^Operating model', OMS$Series), ]
  K   <- if (type == 'SP') Layer('GeomHline')$yintercept[1] else 1
  Tab <- R$Table
  Tab$CalcYear <- Tab$Year - 1
  Tab$DataYear <- Tab$Year - Lag - 1
  Tab$TrueStatus <- OMS$Value[match(Tab$DataYear, OMS$Year)] / K
  utils::write.csv(Tab, file.path(FigDir, paste0(MP, '_Table.csv')), row.names = FALSE)
}
utils::write.csv(data.frame(OM = ReplayOM, Sim = ReplaySim, FrameYear = FrameYear, DataLag = Lag, Interval = Int,
                            nSim = nrow(MinSB), ThRP = ThRP, nBelowThRP = sum(MinSB$Value < ThRP),
                            IR = ReplayCMPs[['IR']], IT = ReplayCMPs[['IT']], SP = ReplayCMPs[['SP']]),
                 file.path(FigDir, 'Replay_Info.csv'), row.names = FALSE)
