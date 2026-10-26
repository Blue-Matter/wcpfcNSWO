#' Probability Spawning Biomass Remains Above the Limit Reference Point
#'
#' Performance metric for the Safety objective: the probability that spawning
#' biomass remains above a limit reference point, `Lim` of the equilibrium
#' unfished spawning biomass SB0, in every year in which the MP is active. A
#' simulation in which SB / SB0 falls below `Lim` in any year is a breach, so
#' the value is the proportion of simulations without a breach.
#'
#' Spawning biomass is averaged over the seasons of each calendar year.
#'
#' @param object An `mse` object.
#' @param Lim Numeric. Limit reference point as a fraction of SB0. Default
#'   `0.17`.
#' @param Years `NULL` (default, all years the MP is active; see
#'   [PM_Window_Years()]) or the projection years to include.
#' @param Stock Character. Name of the spawning stock. Default `'Female'`.
#'
#' @returns A `pm` object (see [MSEtool::NewPM()]).
#' @export
PM_SafetyNoBreach <- function(object, Lim = 0.17, Years = NULL, Stock = 'Female') {
  Years <- Years %||% PM_Window_Years(object)
  df <- MSEtool::SB_SB0(object, type = 'Equilibrium', df = TRUE, Reduce = FALSE)
  df <- df[df$Period == 'Projection' & df$Stock %in% Stock, ]
  df$Year <- floor(df$Year)
  df <- df[df$Year %in% Years, ]
  df <- stats::aggregate(Value ~ Sim + Stock + MP + Year, data = df, FUN = mean)

  # lowest annual SB/SB0 of each simulation
  df <- stats::aggregate(Value ~ Sim + Stock + MP, data = df, FUN = min)
  df$Year <- max(Years)

  MSEtool::NewPM(df,
                 Name    = 'SafetyNoBreach',
                 Caption = paste0('P(SB > ', Lim, ' SB0 in every year)'),
                 Ref     = Lim,
                 Op      = `>`)
}
class(PM_SafetyNoBreach) <- 'pm'

#' Probability Fishing Mortality is at or Below the F Target
#'
#' Performance metric for the Status objective: the probability that
#' fishing mortality is at or below the F target (e.g. F40%), over all
#' simulations and the years in which the MP is active.
#'
#' F is MSEtool's F/FMSY averaged over the seasons of each calendar year,
#' divided by the F target relative to FMSY (see [Calc_FTarget_FMSY()]).
#'
#' @param object An `mse` object.
#' @param FTarget_FMSY Numeric. The F target relative to FMSY, for each
#'   simulation or a single value (see [Calc_FTarget_FMSY()]).
#' @param Years `NULL` (default, all years the MP is active) or the projection
#'   years to include.
#'
#' @returns A `pm` object (see [MSEtool::NewPM()]).
#' @export
PM_StatusF <- function(object, FTarget_FMSY, Years = NULL) {
  df <- MSEtool::F_FMSY(object, df = TRUE, Reduce = FALSE)
  df <- df[df$Period == 'Projection', ]
  df$Year <- floor(df$Year)
  df <- stats::aggregate(Value ~ Sim + Stock + MP + Year, data = df, FUN = mean)
  Sim <- as.integer(as.character(df$Sim))
  df$Value <- df$Value / rep_len(FTarget_FMSY, max(Sim))[Sim]

  MSEtool::NewPM(df,
                 Name    = 'StatusF',
                 Caption = 'P(F <= F target)',
                 Ref     = 1,
                 Op      = `<=`,
                 Years   = Years,
                 OM      = object@OM)
}
class(PM_StatusF) <- 'pm'
