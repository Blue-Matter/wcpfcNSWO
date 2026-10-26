# Run the MSE projections of the tuned CMPs (04b-Tune-CMPs.R) for each OM
library(wcpfcNSWO)

TuneDir <- 'objects/Tune'
MSEDir  <- 'objects/MSE'

TunedCMPs <- readRDS(file.path(TuneDir, 'TunedCMPs.rds'))
if (!length(TunedCMPs))
  cli::cli_abort("No tuned CMPs in {.file {file.path(TuneDir, 'TunedCMPs.rds')}}; run 04b-Tune-CMPs.R.")

# Included OMs with a Hist object are projected; set RunMSE to FALSE to skip an OM
OM_DF <- ListSS3Runs() |>
  dplyr::filter(Include) |>
  dplyr::mutate(HistFile = file.path('objects/Hist', paste0(OM, '.hist')),
                MSEFile  = file.path(MSEDir, paste0(OM, '.mse')),
                RunMSE   = file.exists(HistFile))

# OM_DF$RunMSE <- FALSE
# OM_DF$RunMSE[OM_DF$OM %in% c('12')] <- FALSE

nWorkers <- min(length(TunedCMPs), 12)  # CMPs are projected in parallel; each worker needs ~8 GB of RAM

# ---- Project ----
if (any(OM_DF$RunMSE)) {
  MSEtool::SetupParallel(workers = nWorkers)
  for (i in seq_len(nrow(OM_DF))) {
    if (!OM_DF$RunMSE[i]) next
    cli::cli_h1('OM {OM_DF$OM[i]}: {OM_DF$Description[i]}')
    Hist <- readRDS(OM_DF$HistFile[i])
    MSE  <- MSEtool::Project(Hist, MPs = TunedCMPs, parallel = TRUE)
    MSEtool::Save(MSE, OM_DF$MSEFile[i], overwrite = TRUE)
    rm(Hist, MSE)
  }
  MSEtool::DisableParallel()
}

# ---- Performance ----
# Mean over simulations of each performance indicator (see Calc_PM_Stats()).
# The F target of HCRs 1-3 is F40%
TuneSummary <- utils::read.csv(file.path(TuneDir, 'Summary.csv'))
FTargetSPR  <- 0.4

Done <- OM_DF[file.exists(OM_DF$MSEFile), ]
PerformanceDF <- do.call(rbind, lapply(seq_len(nrow(Done)), \(i) {
  MSE <- readRDS(Done$MSEFile[i])
  Missing <- setdiff(names(TunedCMPs), names(MSE@MPs))
  if (length(Missing))
    cli::cli_alert_warning("OM {Done$OM[i]} was not projected with {.val {Missing}}; set RunMSE to TRUE and re-run.")
  FTarget_FMSY <- Calc_FTarget_FMSY(readRDS(Done$HistFile[i]), SPR = FTargetSPR)
  Stats <- Calc_PM_Stats(MSE, FTarget_FMSY = FTarget_FMSY)
  Mean  <- tapply(Stats$Value, list(Stats$MP, Stats$PI), mean)
  CMPs  <- intersect(names(TunedCMPs), rownames(Mean))
  data.frame(OM = Done$OM[i], Description = Done$Description[i], CMP = CMPs,
             Mean[CMPs, unique(Stats$PI), drop = FALSE], row.names = NULL, check.names = FALSE)
}))

print(PerformanceDF[, c('OM', 'CMP', 'Safety', 'Status', 'Stability', 'NoChange', 'Yield_Hist',
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
