#' Years for a Performance Metric Window
#'
#' The calendar years in which the CMPs are active (from `MPStartYear`), or a
#' window of them: `'short'` (the first 10), `'medium'` (the years between the
#' first and last 10), `'long'` (the last 10), or `'terminal'` (the last year).
#'
#' @param object An `mse` object.
#' @param window Character. One of `'all'`, `'short'`, `'medium'`, `'long'`,
#'   or `'terminal'`.
#'
#' @returns Numeric vector of calendar years.
#' @export
PM_Window_Years <- function(object, window = c('all', 'short', 'medium', 'long', 'terminal')) {
  window <- match.arg(window)
  Years  <- unique(floor(MSEtool::Years(object@OM, 'Projection')))
  if (!is.null(object@OM@MPStartYear))
    Years <- Years[Years >= object@OM@MPStartYear]

  switch(window,
         all      = Years,
         short    = utils::head(Years, 10),
         medium   = setdiff(Years, union(utils::head(Years, 10), utils::tail(Years, 10))),
         long     = utils::tail(Years, 10),
         terminal = utils::tail(Years, 1))
}

#' F Target Relative to FMSY
#'
#' The target fishing mortality, F at a spawning potential ratio of `SPR`
#' (e.g. F40%), relative to FMSY for each simulation, in the last year with
#' both reference points. Used to express F relative to the F target from
#' F/FMSY (see [Extract_MSE_Timeseries()]).
#'
#' @param Hist A `hist` object.
#' @param SPR Numeric. Spawning potential ratio of the F target. Default `0.4`.
#' @param Stock Character. Stock for the SPR calculation. Default `'Female'`.
#'
#' @returns Numeric vector with one value per simulation.
#' @export
Calc_FTarget_FMSY <- function(Hist, SPR = 0.4, Stock = 'Female') {
  Hist <- MSEtool::CalcRefPoints(Hist, SPRTarget = SPR, silent = TRUE)
  FSPR <- MSEtool::FSPR(Hist)
  FMSY <- Hist@Reference@MSY@FMSY
  Year <- utils::tail(intersect(dimnames(FSPR)[[3]], dimnames(FMSY)[[3]]), 1)
  Ratio <- FSPR[, Stock, Year, as.character(SPR)] / FMSY[, 1, Year]
  unname(rep_len(Ratio, Hist@OM@nSim))
}

#' Annual Time Series from an MSE
#'
#' Per-simulation annual time series for the historical and projection periods:
#' spawning biomass relative to the equilibrium unfished spawning biomass SB0
#' (`SB_SB0`; SB0 is constant over years) and to SBMSY (`SB_SBMSY`), fishing
#' mortality relative to FMSY (`F_FMSY`) and, if `FTarget_FMSY` is supplied, to
#' the F target
#' (`F_FTarget`), and catch (`Yield`, removals summed over stocks and fleets).
#'
#' Ratios are averaged over the seasons of each calendar year (fishing
#' mortality is an annual rate in each season) and catch is summed.
#'
#' @param MSE An `mse` object.
#' @param Stock Character. Name of the spawning stock. Default `'Female'`.
#' @param FTarget_FMSY Numeric. The F target relative to FMSY, for each
#'   simulation or a single value (see [Calc_FTarget_FMSY()]). Default `NULL`
#'   (no `F_FTarget`).
#'
#' @returns A `data.frame` with columns `Sim`, `Year`, `Period`, `MP`,
#'   `Value`, and `Variable`. Historical rows have `MP = 'Historical'`.
#' @export
Extract_MSE_Timeseries <- function(MSE, Stock = 'Female', FTarget_FMSY = NULL) {
  Annual <- function(df, Variable, FUN) {
    df$Year <- floor(df$Year)
    out <- stats::aggregate(Value ~ Sim + Year + Period + MP, data = df, FUN = FUN)
    out$Sim      <- as.integer(as.character(out$Sim))
    out$Variable <- Variable
    out
  }

  SB0   <- MSEtool::SB_SB0(MSE, type = 'Equilibrium', df = TRUE, Reduce = FALSE)
  SBMSY <- MSEtool::SB_SBMSY(MSE, df = TRUE, Reduce = FALSE)
  FFMSY <- MSEtool::F_FMSY(MSE, df = TRUE, Reduce = FALSE)
  Catch <- MSEtool::Removals(MSE, df = TRUE, Reduce = FALSE)

  FFMSY <- Annual(FFMSY, 'F_FMSY', mean)
  FFTarget <- NULL
  if (!is.null(FTarget_FMSY)) {
    FFTarget <- FFMSY
    FFTarget$Value    <- FFMSY$Value / rep_len(FTarget_FMSY, max(FFMSY$Sim))[FFMSY$Sim]
    FFTarget$Variable <- 'F_FTarget'
  }

  rbind(Annual(SB0[SB0$Stock %in% Stock, ], 'SB_SB0', mean),
        Annual(SBMSY[SBMSY$Stock %in% Stock, ], 'SB_SBMSY', mean),
        FFMSY,
        FFTarget,
        Annual(Catch, 'Yield', sum))
}

#' Per-Simulation Performance Indicators
#'
#' Calculates the value of each performance indicator (PI) for each
#' simulation and MP, over the years the CMPs are active (see
#' [PM_Window_Years()]; `_Short`, `_Medium`, and `_Long` are the first 10,
#' middle, and last 10 years). The PIs of the candidate management objectives
#' (Table 1 of the MSE specifications):
#'
#' - `Safety`, `Safety_*`: probability that SB > `Lim` SB0 in every year,
#'   i.e. a simulation that falls below the LRP in any year is a breach.
#' - `Status`: probability that F <= F target (proportion of years), and
#'   `FTarget_F`: median F target / F. Only if `TS` has `F_FTarget`.
#' - `Stability`: probability that the TAC changes by no more than
#'   `StabilityThreshold` between management cycles.
#' - `NoChange`: probability of no management change, i.e. that the TAC
#'   changes by no more than `NoChangeThreshold` between management cycles.
#' - `Yield_Hist`: probability that the annual catch is at least the mean
#'   catch in `CatchRefYears` (proportion of years).
#'
#' and other PIs:
#'
#' - `Yield_*`: mean annual catch.
#' - `Depletion_*`: mean SB/SB0; `Depletion_Min`: lowest SB/SB0.
#' - `Kobe_Green`: probability that SB > SBMSY and F < FMSY in the same year.
#' - `SB_SBMSY_All`, `F_FMSY_All`: mean SB/SBMSY and F/FMSY.
#' - `AAVY`: mean absolute relative TAC change between management cycles.
#'
#' @param MSE An `mse` object.
#' @param Lim Numeric. Limit reference point as a fraction of the equilibrium
#'   unfished spawning biomass. Default `0.17`.
#' @param StabilityThreshold Numeric. Maximum TAC change for `Stability`.
#'   Default `0.1`.
#' @param NoChangeThreshold Numeric. Maximum TAC change counted as no
#'   management change for `NoChange`. Default `0.01` (the CMPs leave the TAC
#'   unchanged when the change would be less than 1%).
#' @param FTarget_FMSY Numeric. The F target relative to FMSY (see
#'   [Calc_FTarget_FMSY()]), used for the default `TS`. Default `NULL` (no
#'   `Status` or `FTarget_F`).
#' @param CatchRefYears Numeric. Historical years of the reference catch for
#'   `Yield_Hist`. Default `2008:2010`.
#' @param TS The output of [Extract_MSE_Timeseries()] for `MSE`, if already
#'   calculated.
#'
#' @returns A `data.frame` with columns `PI`, `MP`, `Sim`, and `Value`.
#' @export
Calc_PM_Stats <- function(MSE, Lim = 0.17, StabilityThreshold = 0.1, NoChangeThreshold = 0.01,
                          FTarget_FMSY = NULL, CatchRefYears = 2008:2010,
                          TS = Extract_MSE_Timeseries(MSE, FTarget_FMSY = FTarget_FMSY)) {
  CatchRef <- mean(TS$Value[TS$Period == 'Historical' & TS$Variable == 'Yield' &
                              TS$Year %in% CatchRefYears])
  TS <- TS[TS$Period == 'Projection', ]

  PerSim <- function(Variable, FUN, Window = 'all') {
    d <- TS[TS$Variable == Variable & TS$Year %in% PM_Window_Years(MSE, Window), ]
    stats::aggregate(Value ~ Sim + MP, data = d, FUN = FUN)
  }
  # per-sim values of an MSEtool PM (Sim x Complex x MP); NaN when the previous TAC is 0
  FromPM <- function(Arr, NaNValue) {
    Arr <- Arr[, 1, , drop = FALSE]
    Arr[is.nan(Arr)] <- NaNValue
    df <- as.data.frame.table(Arr, responseName = 'Value', stringsAsFactors = FALSE)
    data.frame(Sim = as.integer(df$Sim), MP = df$MP, Value = df$Value)
  }

  Rows <- list()
  Add  <- function(PI, df) Rows[[PI]] <<- data.frame(PI = PI, MP = as.character(df$MP),
                                                     Sim = as.integer(as.character(df$Sim)),
                                                     Value = df$Value)

  Years <- PM_Window_Years(MSE)

  # ---- Candidate management objectives ----
  Windows <- c(All = 'all', Short = 'short', Medium = 'medium', Long = 'long')
  for (w in names(Windows))
    Add(if (w == 'All') 'Safety' else paste0('Safety_', w),
        PerSim('SB_SB0', \(x) as.numeric(all(x > Lim)), Windows[[w]]))

  if ('F_FTarget' %in% TS$Variable) {
    Add('Status', PerSim('F_FTarget', \(x) mean(x <= 1)))
    Add('FTarget_F', PerSim('F_FTarget', \(x) stats::median(1 / x)))
  }

  Add('Stability', FromPM(MSEtool::PM_Stability(MSE, Threshold = StabilityThreshold, Type = 'TAC')@Prob, 1))

  Add('NoChange', FromPM(MSEtool::PM_Stability(MSE, Threshold = NoChangeThreshold, Type = 'TAC')@Prob, 1))

  Add('Yield_Hist', PerSim('Yield', \(x) mean(x >= CatchRef)))

  # ---- Other PIs ----
  for (w in names(Windows)) {
    Add(paste0('Yield_', w), PerSim('Yield', mean, Windows[[w]]))
    Add(paste0('Depletion_', w), PerSim('SB_SB0', mean, Windows[[w]]))
  }
  Add('Depletion_Min', PerSim('SB_SB0', min))
  Add('SB_SBMSY_All', PerSim('SB_SBMSY', mean))
  Add('F_FMSY_All', PerSim('F_FMSY', mean))

  Kobe <- merge(TS[TS$Variable == 'SB_SBMSY' & TS$Year %in% Years, c('Sim', 'MP', 'Year', 'Value')],
                TS[TS$Variable == 'F_FMSY' & TS$Year %in% Years, c('Sim', 'MP', 'Year', 'Value')],
                by = c('Sim', 'MP', 'Year'), suffixes = c('_SB', '_F'))
  Kobe$Value <- as.numeric(Kobe$Value_SB > 1 & Kobe$Value_F < 1)
  Add('Kobe_Green', stats::aggregate(Value ~ Sim + MP, data = Kobe, FUN = mean))

  Add('AAVY', FromPM(MSEtool::PM_AAVY(MSE, Type = 'TAC')@Stat, 0))

  out <- do.call(rbind, Rows)
  rownames(out) <- NULL
  out
}
