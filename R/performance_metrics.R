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
#' each F target (`F_<name>`, e.g. `F_F40`, for a named list; `F_FTarget` for
#' a single F target), and catch (`Yield`, removals summed over stocks and fleets).
#'
#' Ratios are averaged over the seasons of each calendar year (fishing
#' mortality is an annual rate in each season) and catch is summed.
#'
#' @param MSE An `mse` object.
#' @param Stock Character. Name of the spawning stock. Default `'Female'`.
#' @param FTarget_FMSY The F target relative to FMSY, for each simulation or a
#'   single value (see [Calc_FTarget_FMSY()]): a numeric vector (one F target,
#'   `F_FTarget`) or a named list of them (e.g. `list(F40 = ..., F35 = ...)`,
#'   `F_F40`, `F_F35`). Default `NULL` (no F relative to an F target).
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
  if (!is.null(FTarget_FMSY) && !is.list(FTarget_FMSY))
    FTarget_FMSY <- list(FTarget = FTarget_FMSY)
  FFTarget <- lapply(names(FTarget_FMSY), \(nm) {
    out <- FFMSY
    out$Value    <- FFMSY$Value / rep_len(FTarget_FMSY[[nm]], max(FFMSY$Sim))[FFMSY$Sim]
    out$Variable <- paste0('F_', nm)
    out
  })

  do.call(rbind, c(list(Annual(SB0[SB0$Stock %in% Stock, ], 'SB_SB0', mean),
                        Annual(SBMSY[SBMSY$Stock %in% Stock, ], 'SB_SBMSY', mean),
                        FFMSY),
                   FFTarget,
                   list(Annual(Catch, 'Yield', sum))))
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
#'   `FTarget_F`: median F target / F, with the F target of each MP
#'   (`MPFTarget`), or the single F target in `TS` (`F_FTarget`).
#' - `Status_<name>`, `FTarget_F_<name>` (e.g. `Status_F40`): the same for
#'   each F target in `TS` (`F_<name>`; see [Extract_MSE_Timeseries()]), for
#'   every MP.
#' - `Stability`: probability that the TAC changes by no more than
#'   `StabilityThreshold` between management cycles.
#' - `NoChange`: probability of no management change, i.e. that the TAC
#'   changes by no more than `NoChangeThreshold` between management cycles.
#' - `Yield_Hist` (and the other names of `CatchRefYears`): probability that
#'   the annual catch is at least the mean historical catch in the reference
#'   years (proportion of years).
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
#' @param FTarget_FMSY The F target(s) relative to FMSY (see
#'   [Calc_FTarget_FMSY()] and [Extract_MSE_Timeseries()]), used for the
#'   default `TS`. Default `NULL` (no `Status` or `FTarget_F`).
#' @param MPFTarget Optional named character vector: the name of the F target
#'   in `FTarget_FMSY` of each MP (e.g. `c(IR_H1 = 'F40')`), for `Status` and
#'   `FTarget_F`.
#' @param CatchRefYears Historical years of the reference catch: a numeric
#'   vector (`Yield_Hist`) or a named list of them, one PI per element (e.g.
#'   `list(Yield_Hist = 2008:2010, Yield_Recent = 2016:2021)`).
#' @param TS The output of [Extract_MSE_Timeseries()] for `MSE`, if already
#'   calculated.
#'
#' @returns A `data.frame` with columns `PI`, `MP`, `Sim`, and `Value`.
#' @export
Calc_PM_Stats <- function(MSE, Lim = 0.17, StabilityThreshold = 0.1, NoChangeThreshold = 0.01,
                          FTarget_FMSY = NULL, MPFTarget = NULL, CatchRefYears = 2008:2010,
                          TS = Extract_MSE_Timeseries(MSE, FTarget_FMSY = FTarget_FMSY)) {
  if (!is.list(CatchRefYears)) CatchRefYears <- list(Yield_Hist = CatchRefYears)
  CatchRef <- vapply(CatchRefYears, \(yrs)
    mean(TS$Value[TS$Period == 'Historical' & TS$Variable == 'Yield' & TS$Year %in% yrs]),
    numeric(1))
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

  StatusFUN    <- \(x) mean(x <= 1)
  FTargetFFUN  <- \(x) stats::median(1 / x)
  FTargetNames <- setdiff(sub('^F_', '', grep('^F_', unique(TS$Variable), value = TRUE)),
                          c('FMSY', 'FTarget'))

  # F target of each MP
  if (!is.null(MPFTarget)) {
    MPFTarget <- MPFTarget[intersect(names(MPFTarget), unique(TS$MP))]
    if (length(setdiff(MPFTarget, FTargetNames)))
      cli::cli_abort("F target{?s} of `MPFTarget` not in `TS`: {.val {setdiff(MPFTarget, FTargetNames)}}.")
    OwnF <- do.call(rbind, lapply(names(MPFTarget), \(mp) {
      d <- TS[TS$Variable == paste0('F_', MPFTarget[[mp]]) & TS$MP == mp, ]
      d$Variable <- 'F_Own'
      d
    }))
    TS <- rbind(TS, OwnF)
    Add('Status', PerSim('F_Own', StatusFUN))
    Add('FTarget_F', PerSim('F_Own', FTargetFFUN))
  } else if ('F_FTarget' %in% TS$Variable) {
    Add('Status', PerSim('F_FTarget', StatusFUN))
    Add('FTarget_F', PerSim('F_FTarget', FTargetFFUN))
  }

  # every F target
  for (nm in FTargetNames) {
    Add(paste0('Status_', nm), PerSim(paste0('F_', nm), StatusFUN))
    Add(paste0('FTarget_F_', nm), PerSim(paste0('F_', nm), FTargetFFUN))
  }

  Add('Stability', FromPM(MSEtool::PM_Stability(MSE, Threshold = StabilityThreshold, Type = 'TAC')@Prob, 1))

  Add('NoChange', FromPM(MSEtool::PM_Stability(MSE, Threshold = NoChangeThreshold, Type = 'TAC')@Prob, 1))

  for (nm in names(CatchRef))
    Add(nm, PerSim('Yield', \(x) mean(x >= CatchRef[[nm]])))

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

#' Convergence of the Surplus Production MPs
#'
#' The proportion of management cycles in which the surplus production model
#' of each MP converged, from [MSEtool::SPEstimates()]. When the fit fails or
#' does not converge, the advice is set by the `OnFail` rule of
#' [MSEtool::SurplusProduction()] (by default, the previous TAC is kept).
#'
#' @param MSE An `mse` object.
#'
#' @returns A `data.frame` with columns `MP`, `nCycles` (management cycles
#'   over all simulations), and `Converged` (proportion of cycles); no rows if
#'   no MP fits a surplus production model.
#' @export
Calc_SP_Convergence <- function(MSE) {
  # SPEstimates() errors when no MP fits a surplus production model
  Est <- tryCatch(MSEtool::SPEstimates(MSE), error = function(e) {
    if (grepl('estimates found', conditionMessage(e), fixed = TRUE)) NULL else stop(e)
  })
  if (!NROW(Est))
    return(data.frame(MP = character(0), nCycles = integer(0), Converged = numeric(0)))
  Out <- stats::aggregate(Converged ~ MP, data = Est, FUN = mean)
  Out$nCycles <- as.vector(table(Est$MP)[Out$MP])
  Out[c('MP', 'nCycles', 'Converged')]
}
