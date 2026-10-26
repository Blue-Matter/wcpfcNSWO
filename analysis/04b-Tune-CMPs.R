# Tune the candidate management procedures for the base case OM: maximise
# catch subject to the Safety (P(SB > LRP in every year) >= PSafety) and
# Status (P(F <= F40%) >= PStatus) constraints. Run 04a-Select-Configs.R first
# to select the HCR and other MP settings not set by tuning
source('analysis/04-Define-CMPs.R')

HistFile <- normalizePath('objects/Hist/00.hist')

Overwrite    <- FALSE     # re-tune CMPs with a saved result for the same MP settings
DryRun       <- FALSE     # only estimate the number of projections and run time
nWorkers     <- 10        # CMPs are tuned in parallel, one per worker; each needs ~8-10 GB of RAM
MinCatchGain <- 0.02      # a looser TAC change limit is kept only if it increases catch by at least this fraction
AlwaysKeep   <- 0.2       # TAC change limits always kept (e.g. to show that a looser limit does not increase catch)

Control <- MSEtool::TuneControl(Interval = TuneInterval, DryRun = DryRun)

# MP arguments other than tunepar; a saved result is re-used only if these are unchanged
MPSettings <- function(MP) {
  Args <- formals(MP)
  Args[names(Args) != 'tunepar']
}

# Objective and constraints (e.g. the definition of the LRP in PM_SafetyNoBreach);
# a saved result is also re-used only if these are unchanged
MetricSpec <- function(Metrics) {
  lapply(Metrics, \(m) list(Name = m$Name, Type = m$Type, Min = m$Min, Max = m$Max,
                            Args = m$Args, PM = deparse(body(m$PM))))
}
TuneSpec <- MetricSpec(c(list(Objective),
                         if (inherits(Constraints, 'tunemetric')) list(Constraints) else Constraints))

# TuneMP records a failed projection as infeasible without the error
AllFailed <- function(Tuned) all(Tuned@History$FailRate >= 1)

# A saved result is current if the MP settings and the tuning specification are unchanged
IsCurrent <- function(Tuned, i) {
  identical(MPSettings(Tuned@MP), MPSettings(CMPs$Fn[[i]])) &&
    identical(MetricSpec(Tuned@Settings$Metrics), TuneSpec) &&
    !AllFailed(Tuned)
}

TuneCMP <- function(i) {
  File <- file.path(TuneDir, paste0(CMPs$Name[i], '.tune'))
  if (file.exists(File) && !Overwrite && IsCurrent(readRDS(File), i))
    return('exists')
  tryCatch({
    Hist  <- readRDS(HistFile)
    Tuned <- MSEtool::TuneMP(Hist, CMPs$Fn[[i]],
                             Objective   = Objective,
                             Constraints = Constraints,
                             Control     = Control,
                             parallel    = FALSE,
                             silent      = TRUE)
    Tuned@MPName <- CMPs$Name[i]
    saveRDS(Tuned, File)
    if (AllFailed(Tuned)) 'Error: every projection failed' else Tuned@Status
  }, error = function(e) paste('Error:', conditionMessage(e)))
}

# Workers attach the packages, and a CMP must project in a worker before all are tuned
WorkerOptions <- furrr::furrr_options(seed = TRUE, scheduling = Inf, packages = c('MSEtool', 'wcpfcNSWO'))

CheckWorker <- function(i) {
  tryCatch({
    MSEtool::Project(readRDS(HistFile), MPs = list(Check = CMPs$Fn[[i]]), nSim = 2, silent = TRUE)
    'OK'
  }, error = function(e) conditionMessage(e))
}

# ---- Tune ----
if (DryRun) {
  # one CMP of each type
  Hist <- readRDS(HistFile)
  for (i in match(unique(CMPs$MP), CMPs$MP)) {
    cli::cli_h2(CMPs$Name[i])
    MSEtool::TuneMP(Hist, CMPs$Fn[[i]], Objective = Objective, Constraints = Constraints,
                    Control = Control)
  }
} else {
  MSEtool::SetupParallel(workers = nWorkers)
  Check <- furrr::future_map_chr(match(unique(CMPs$MP), CMPs$MP), CheckWorker, .options = WorkerOptions)
  if (any(Check != 'OK')) {
    MSEtool::DisableParallel()
    cli::cli_abort("Projection failed in a parallel worker: {unique(Check[Check != 'OK'])}")
  }
  TuneLog <- furrr::future_map_chr(seq_len(nrow(CMPs)), TuneCMP,
                                   .progress = TRUE,
                                   .options  = WorkerOptions)
  MSEtool::DisableParallel()
  print(data.frame(CMP = CMPs$Name, Result = TuneLog), row.names = FALSE)
}

# ---- Summarise ----
Files  <- file.path(TuneDir, paste0(CMPs$Name, '.tune'))
Tuned  <- lapply(Files, \(f) if (file.exists(f)) readRDS(f))
IsDone <- vapply(seq_along(Tuned), \(i) !is.null(Tuned[[i]]) && IsCurrent(Tuned[[i]], i), logical(1))

if (!DryRun && any(IsDone)) {
  Summary <- CMPs[IsDone, c('Name', 'MP', 'HCR', 'ThRP', 'TACChange')]
  Summary$tunepar    <- vapply(Tuned[IsDone], \(x) x@Args$tunepar, numeric(1))
  Summary$TuneStatus <- vapply(Tuned[IsDone], \(x) x@Status, character(1))
  Summary[[Objective$Name]] <- vapply(Tuned[IsDone], \(x) x@Objective, numeric(1))
  ConstraintNames <- vapply(Constraints, \(x) x$Name, character(1))
  for (nm in ConstraintNames)
    Summary[[nm]] <- vapply(Tuned[IsDone], \(x) {
      Tab <- MSEtool::TuneTable(x, 'Constraints')
      Tab$Value[Tab$Name == nm]
    }, numeric(1))

  # Tighter TAC change limits give more stable TACs: for each MP, the tightest
  # limit is kept, and a looser limit only if its catch is at least MinCatchGain
  # higher than that of the last kept limit. Infeasible tunings are dropped
  Obj <- Summary[[Objective$Name]]
  Summary$Keep <- Summary$TuneStatus != 'infeasible'
  for (g in split(seq_len(nrow(Summary)), Summary$MP)) {
    g <- g[Summary$Keep[g]]
    Best <- -Inf
    for (j in g[order(Summary$TACChange[g])]) {
      if (Obj[j] >= (1 + MinCatchGain) * Best) Best <- Obj[j] else Summary$Keep[j] <- FALSE
    }
  }
  Summary$Keep[Summary$TACChange %in% AlwaysKeep & Summary$TuneStatus != 'infeasible'] <- TRUE

  print(Summary, row.names = FALSE)
  if (any(Summary$TuneStatus == 'at_bound'))
    cli::cli_alert_warning("Catch still increasing at the end of the tuning range for {.val {Summary$Name[Summary$TuneStatus == 'at_bound']}}; widen {.arg Interval} in {.fn TuneControl}.")
  if (any(Summary$TuneStatus == 'infeasible'))
    cli::cli_alert_warning("No tuning met the constraints for {.val {Summary$Name[Summary$TuneStatus == 'infeasible']}}.")
  if (!all(IsDone))
    cli::cli_alert_warning("Not tuned with the current MP settings and tuning specification: {.val {CMPs$Name[!IsDone]}}.")

  utils::write.csv(Summary, file.path(TuneDir, 'Summary.csv'), row.names = FALSE)

  ## Tuned CMPs for the projections
  TunedCMPs <- lapply(Tuned[IsDone][Summary$Keep], \(x) x@MP)
  names(TunedCMPs) <- Summary$Name[Summary$Keep]
  saveRDS(TunedCMPs, file.path(TuneDir, 'TunedCMPs.rds'))
}
