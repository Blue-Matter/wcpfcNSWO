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

# CMP groups: MP and index set (the order of the CMPs in a saved group can
# differ from the order here, after re-tuned CMPs are combined into it)
TuneSummary$Group <- paste(TuneSummary$MP, TuneSummary$IndexSet, sep = '_')
Groups <- split(TuneSummary$Name, factor(TuneSummary$Group, unique(TuneSummary$Group)))

# Included OMs with a Hist object are projected; set RunMSE to FALSE to skip an OM
OM_DF <- ListSS3Runs() |>
  dplyr::filter(Include) |>
  dplyr::mutate(HistFile = file.path('objects/Hist', paste0(OM, '.hist')),
                RunMSE   = file.exists(HistFile))

# OM_DF$RunMSE <- OM_DF$OM %in% c('00', '01')

MSEFile  <- function(OM, Group) {
  file.path(MSEDir, OM, paste0(Group, '.mse'))
}

SideFile <- function(File, Type) {
  paste0(tools::file_path_sans_ext(File), '.', Type, '.rds')
}

ReadSide <- function(File, Type) {
  Side <- SideFile(File, Type)
  if (file.exists(Side) && file.mtime(Side) >= file.mtime(File)) readRDS(Side)
}

CMPArgs <- lapply(TunedCMPs, formals)

SavedArgs <- function(File) {
  Args <- ReadSide(File, 'args')
  if (is.null(Args)) {
    Args <- readRDS(File)@Misc$CMPArgs
    saveRDS(Args, SideFile(File, 'args'))
  }
  Args
}

CurrentCMPs <- function(File, CMPNames) {
  if (!file.exists(File)) return(character(0))
  Saved <- SavedArgs(File)
  names(Saved)[vapply(names(Saved), \(n) n %in% CMPNames &&
                        isTRUE(all.equal(Saved[[n]], CMPArgs[[n]], tolerance = 1e-8)), logical(1))]
}

# A saved group is current if it has exactly the same tuned CMPs
IsCurrentMSE <- function(File, CMPNames) {
  file.exists(File) && setequal(names(SavedArgs(File)), CMPNames) &&
    setequal(CurrentCMPs(File, CMPNames), CMPNames)
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

      Keep    <- if (Overwrite) character(0) else CurrentCMPs(File, Groups[[g]])
      Partial <- length(Keep) > 0 && names(SavedArgs(File))[1] %in% Keep
      ToRun   <- if (Partial) setdiff(Groups[[g]], Keep) else Groups[[g]]
      cli::cli_h2('{g}: projecting {length(ToRun)} of {length(Groups[[g]])} CMPs')
      if (is.null(Hist)) Hist <- readRDS(OM_DF$HistFile[i])
      MSE <- MSEtool::Project(Hist, MPs = TunedCMPs[ToRun], parallel = TRUE)
      if (Partial) {
        Old <- readRDS(File)
        if (length(Keep) < length(Old@MPs))
          Old <- MSEtool::Subset(Old, MPs = sort(match(Keep, names(Old@MPs))))
        MSE <- MSEtool::CombineMP(list(Old, MSE), silent = TRUE)
        rm(Old)
      }
      MSE@Misc$CMPArgs <- CMPArgs[names(MSE@MPs)]
      MSEtool::Save(MSE, File, overwrite = TRUE)
      saveRDS(MSE@Misc$CMPArgs, SideFile(File, 'args'))
      rm(MSE)
      gc()
    }
    rm(Hist)
  }
  MSEtool::DisableParallel()
}

# ---- Performance ----
source('analysis/00a-PM-Specifications.R')

MPFTarget <- stats::setNames(paste0('F', round(100 * TuneSummary$FTargetSPR)), TuneSummary$Name)
MPThRP    <- stats::setNames(TuneSummary$ThRP, TuneSummary$Name)

Done <- OM_DF[file.exists(OM_DF$HistFile), ]
Jobs <- expand.grid(Group = names(Groups), i = seq_len(nrow(Done)), stringsAsFactors = FALSE)
Jobs$File    <- MSEFile(Done$OM[Jobs$i], Jobs$Group)
Jobs$Current <- vapply(seq_len(nrow(Jobs)), \(j) IsCurrentMSE(Jobs$File[j], Groups[[Jobs$Group[j]]]), logical(1))
for (i in seq_len(nrow(Done))) {
  Missing <- Jobs$Group[Jobs$i == i & !Jobs$Current]
  if (length(Missing) && length(Missing) < length(Groups))
    cli::cli_alert_warning("OM {Done$OM[i]} has no current results for {.val {Missing}}; set RunMSE to TRUE and re-run.")
}
Jobs <- Jobs[Jobs$Current, ]

cl <- parallel::makeCluster(min(nWorkers, max(nrow(Jobs), nrow(Done))))
parallel::clusterEvalQ(cl, {
  library(wcpfcNSWO)
  source('analysis/00a-PM-Specifications.R')
})
FTarget_FMSY <- parallel::parLapply(cl, Done$HistFile, \(f) PM_FTarget_FMSY(readRDS(f)))

PerfKey <- function(j) {
  CMPNames <- Groups[[Jobs$Group[j]]]
  list(CMPArgs = CMPArgs[CMPNames], FTarget_FMSY = FTarget_FMSY[[Jobs$i[j]]],
       MPFTarget = MPFTarget[CMPNames], MPThRP = MPThRP[CMPNames],
       Settings = list(LRP = LRP, StatusThRP = StatusThRP, StabilityThreshold = StabilityThreshold,
                       NoChangeThreshold = NoChangeThreshold, CatchRefYears = CatchRefYears),
       Code = lapply(list(Calc_PM_Stats, Extract_MSE_Timeseries, PM_Window_Years, Calc_SP_Convergence),
                     \(f) deparse(f)),
       MSEtool = as.character(utils::packageVersion('MSEtool')))
}
Keys    <- lapply(seq_len(nrow(Jobs)), PerfKey)
Results <- lapply(seq_len(nrow(Jobs)), \(j) {
  Saved <- ReadSide(Jobs$File[j], 'perf')
  if (!is.null(Saved) && identical(Saved$Key, Keys[[j]])) Saved
})
ToDo <- which(vapply(Results, is.null, logical(1)))
cli::cli_inform("Performance of {nrow(Jobs) - length(ToDo)} group{?s} re-used; calculating {length(ToDo)}.")

if (length(ToDo)) {
  parallel::clusterExport(cl, c('Jobs', 'Done', 'Keys', 'SideFile'))
  Results[ToDo] <- parallel::parLapplyLB(cl, ToDo, \(j) {
    i    <- Jobs$i[j]
    Key  <- Keys[[j]]
    MSE  <- readRDS(Jobs$File[j])
    Stats <- Calc_PM_Stats(MSE, Lim = LRP, StabilityThreshold = StabilityThreshold,
                           NoChangeThreshold = NoChangeThreshold, FTarget_FMSY = Key$FTarget_FMSY,
                           MPFTarget = Key$MPFTarget, ThRP = StatusThRP, MPThRP = Key$MPThRP, CatchRefYears = CatchRefYears)
    Mean <- tapply(Stats$Value, list(Stats$MP, Stats$PI), mean)
    Conv <- Calc_SP_Convergence(MSE)
    Out  <- list(Key = Key,
                 Performance = data.frame(OM = Done$OM[i], Description = Done$Description[i], CMP = rownames(Mean),
                                          Mean[, unique(Stats$PI), drop = FALSE], row.names = NULL,
                                          check.names = FALSE),
                 SPConvergence = if (nrow(Conv)) data.frame(OM = Done$OM[i], Conv))
    saveRDS(Out, SideFile(Jobs$File[j], 'perf'))
    Out
  })
}
parallel::stopCluster(cl)

PerformanceDF <- do.call(rbind, lapply(Results, `[[`, 'Performance'))
SPConvergence <- do.call(rbind, lapply(Results, `[[`, 'SPConvergence'))

if (!is.null(PerformanceDF)) {
  PerformanceDF <- PerformanceDF[order(PerformanceDF$OM, match(PerformanceDF$CMP, names(TunedCMPs))), ]
  print(PerformanceDF[, c('OM', 'CMP', 'Safety', 'Status_F', 'Stability', 'NoChange', names(CatchRefYears),
                          'Yield_All')],
        row.names = FALSE, digits = 3)
  utils::write.csv(PerformanceDF, file.path(MSEDir, 'Performance.csv'), row.names = FALSE)

  Base <- PerformanceDF[PerformanceDF$OM == '00', ]
  if (nrow(Base)) {
    SafetyPI <- (TuneSummary$SafetyPI %||% rep('Safety_NoBreach', nrow(TuneSummary)))[
      match(Base$CMP, TuneSummary$Name)]
    Check <- data.frame(CMP = Base$CMP, Status = Base$Status_F,
                        Safety = ifelse(SafetyPI == 'Safety', Base$Safety, Base$Safety_NoBreach))
    Check <- merge(Check, TuneSummary[, c('Name', 'Safety', 'Status')],
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
