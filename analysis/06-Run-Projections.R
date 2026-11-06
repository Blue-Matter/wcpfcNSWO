# Run the MSE projections of the tuned CMPs (05e-Summarise-Tuning.R) for each OM
#
# The CMPs are projected in groups (MP and index set), saved as
# objects/MSE/<OM>/<Group>.mse. A group with results for the same tuned CMPs is
# not projected again, so the OMs can be split across machines (RunMSE) and the
# objects/MSE folders combined
library(wcpfcNSWO)

TuneDir <- 'objects/Tune'
MSEDir  <- 'objects/MSE'

Overwrite <- FALSE  # re-project groups with current results
nWorkers  <- 12     # CMPs are projected in parallel; each worker needs ~8 GB of RAM

TunedCMPs <- readRDS(file.path(TuneDir, 'TunedCMPs.rds'))
if (!length(TunedCMPs))
  cli::cli_abort("No tuned CMPs in {.file {file.path(TuneDir, 'TunedCMPs.rds')}}; run 05e-Summarise-Tuning.R.")
TuneSummary <- utils::read.csv(file.path(TuneDir, 'Summary.csv'))
TuneSummary <- TuneSummary[match(names(TunedCMPs), TuneSummary$Name), ]

# CMP groups: MP and index set
TuneSummary$Group <- paste(TuneSummary$MP, TuneSummary$IndexSet, sep = '_')
Groups <- split(TuneSummary$Name, factor(TuneSummary$Group, unique(TuneSummary$Group)))

# Included OMs with a Hist object are projected; set RunMSE to FALSE to skip an OM
OM_DF <- ListSS3Runs() |>
  dplyr::filter(Include) |>
  dplyr::mutate(HistFile = file.path('objects/Hist', paste0(OM, '.hist')),
                RunMSE   = file.exists(HistFile))

# OM_DF$RunMSE <- OM_DF$OM %in% c('00', '01')

MSEFile <- function(OM, Group) file.path(MSEDir, OM, paste0(Group, '.mse'))

# A saved group is current if it has the same tuned CMPs (MP arguments, incl. tunepar)
IsCurrentMSE <- function(File, CMPNames) {
  if (!file.exists(File)) return(FALSE)
  isTRUE(all.equal(readRDS(File)@Misc$CMPArgs, lapply(TunedCMPs[CMPNames], formals), tolerance = 1e-8))
}

# ---- Project ----
if (any(OM_DF$RunMSE)) {
  MSEtool::SetupParallel(workers = nWorkers)
  for (i in seq_len(nrow(OM_DF))) {
    if (!OM_DF$RunMSE[i]) next
    cli::cli_h1('OM {OM_DF$OM[i]}: {OM_DF$Description[i]}')
    Hist <- NULL
    for (g in names(Groups)) {
      File <- MSEFile(OM_DF$OM[i], g)
      if (!Overwrite && IsCurrentMSE(File, Groups[[g]])) {
        cli::cli_alert_info('{g}: current results exist')
        next
      }
      cli::cli_h2(g)
      if (is.null(Hist)) Hist <- readRDS(OM_DF$HistFile[i])
      MSE <- MSEtool::Project(Hist, MPs = TunedCMPs[Groups[[g]]], parallel = TRUE)
      MSE@Misc$CMPArgs <- lapply(TunedCMPs[Groups[[g]]], formals)
      dir.create(dirname(File), recursive = TRUE, showWarnings = FALSE)
      MSEtool::Save(MSE, File, overwrite = TRUE)
      rm(MSE)
      gc()
    }
    rm(Hist)
  }
  MSEtool::DisableParallel()
}

# ---- Performance ----
# Mean over simulations of each performance indicator (see Calc_PM_Stats()):
# Status against the F target of the HCR of each CMP, and against each F target
source('analysis/00a-PM-Specifications.R')

MPFTarget <- stats::setNames(paste0('F', round(100 * TuneSummary$FTargetSPR)), TuneSummary$Name)

Done <- OM_DF[file.exists(OM_DF$HistFile), ]
PerformanceDF <- NULL
SPConvergence <- NULL
for (i in seq_len(nrow(Done))) {
  Files <- MSEFile(Done$OM[i], names(Groups))
  Have  <- vapply(seq_along(Files), \(g) IsCurrentMSE(Files[g], Groups[[g]]), logical(1))
  if (!any(Have)) next
  if (!all(Have))
    cli::cli_alert_warning("OM {Done$OM[i]} has no current results for {.val {names(Groups)[!Have]}}; set RunMSE to TRUE and re-run.")

  FTarget_FMSY <- PM_FTarget_FMSY(readRDS(Done$HistFile[i]))
  for (File in Files[Have]) {
    MSE   <- readRDS(File)
    Stats <- Calc_PM_Stats(MSE, Lim = LRP, StabilityThreshold = StabilityThreshold,
                           NoChangeThreshold = NoChangeThreshold,
                           FTarget_FMSY = FTarget_FMSY, MPFTarget = MPFTarget,
                           CatchRefYears = CatchRefYears)
    Mean  <- tapply(Stats$Value, list(Stats$MP, Stats$PI), mean)
    PerformanceDF <- rbind(PerformanceDF,
                           data.frame(OM = Done$OM[i], Description = Done$Description[i], CMP = rownames(Mean),
                                      Mean[, unique(Stats$PI), drop = FALSE], row.names = NULL, check.names = FALSE))
    Conv <- Calc_SP_Convergence(MSE)
    if (nrow(Conv)) SPConvergence <- rbind(SPConvergence, data.frame(OM = Done$OM[i], Conv))
    rm(MSE)
  }
}

if (!is.null(PerformanceDF)) {
  PerformanceDF <- PerformanceDF[order(PerformanceDF$OM, match(PerformanceDF$CMP, names(TunedCMPs))), ]
  print(PerformanceDF[, c('OM', 'CMP', 'Safety', 'Status', 'Stability', 'NoChange', names(CatchRefYears),
                          'Yield_All')],
        row.names = FALSE, digits = 3)
  utils::write.csv(PerformanceDF, file.path(MSEDir, 'Performance.csv'), row.names = FALSE)

  ## Base case: projections reproduce the tuning constraints (Safety and Status)
  Base <- PerformanceDF[PerformanceDF$OM == '00', ]
  if (nrow(Base)) {
    Check <- merge(Base[, c('CMP', 'Safety', 'Status')], TuneSummary[, c('Name', 'Safety', 'Status')],
                   by.x = 'CMP', by.y = 'Name', suffixes = c('', '_Tuned'))
    for (pi in c('Safety', 'Status')) {
      Differ <- abs(Check[[pi]] - Check[[paste0(pi, '_Tuned')]]) > 1e-6
      if (any(Differ))
        cli::cli_alert_warning("Base case {pi} differs from the tuning for {.val {Check$CMP[Differ]}}.")
    }
  }
}

## Surplus production MPs: proportion of management cycles in which the model converged
if (!is.null(SPConvergence)) {
  print(stats::reshape(SPConvergence[c('OM', 'MP', 'Converged')], idvar = 'MP', timevar = 'OM',
                       direction = 'wide'), row.names = FALSE, digits = 2)
  utils::write.csv(SPConvergence, file.path(MSEDir, 'SP_Convergence.csv'), row.names = FALSE)
}
