# Tune the candidate management procedures for the base case OM: maximise
# catch subject to the Safety (P(SB > LRP in every year) >= PSafety) and
# Status (P(F <= F target of the HCR) >= PStatus) constraints. Run
# 05c-Select-Configs.R first to select the MP settings not set by tuning
#
# The CMPs can be tuned on several machines: set Batch to a different value
# (1, ..., nBatch) on each machine, then copy the .tune files of every machine
# into objects/Tune and run 05e-Summarise-Tuning.R. Every machine must have the
# same objects/Hist/00.hist, objects/Tune/Configs.rds, wcpfcNSWO commit, and
# MSEtool version. A CMP with a current saved result is not tuned again
source('analysis/05b-Define-CMPs.R')

HistFile <- normalizePath('objects/Hist/00.hist')

nBatch    <- 2         # number of batches (machines)
Batch     <- 1         # batch tuned on this machine; NULL for all CMPs
Overwrite <- FALSE     # re-tune CMPs with a current saved result
DryRun    <- FALSE     # only estimate the number of projections and run time
nWorkers  <- 10        # CMPs are tuned in parallel, one per worker; each needs ~8-10 GB of RAM

Control <- MSEtool::TuneControl(Interval = TuneInterval, DryRun = DryRun)

# CMPs alternate between batches; CMPs are ordered by TAC change limit, HCR,
# index set and MP, so each batch has a similar mix of MPs
CMPs$Batch <- rep_len(seq_len(nBatch), nrow(CMPs))
BatchInd   <- if (is.null(Batch)) seq_len(nrow(CMPs)) else which(CMPs$Batch %in% Batch)
cli::cli_inform("Tuning {length(BatchInd)} of {nrow(CMPs)} CMPs (batch {Batch %||% 'all'} of {nBatch}).")

# The machine, package versions, and time of a tuning, kept in the result
RunInfo <- function(Start) {
  MSEtoolDesc <- utils::packageDescription('MSEtool')
  list(Machine   = unname(Sys.info()['nodename']),
       MSEtool   = as.character(utils::packageVersion('MSEtool')),
       MSEtoolSHA = MSEtoolDesc$RemoteSha %||% NA_character_,
       MSEtoolBuilt = MSEtoolDesc$Built,
       wcpfcNSWO = as.character(utils::packageVersion('wcpfcNSWO')),
       R         = R.version.string,
       Start     = Start,
       Minutes   = as.numeric(difftime(Sys.time(), Start, units = 'mins')))
}

TuneCMP <- function(i) {
  File <- file.path(TuneDir, paste0(CMPs$Name[i], '.tune'))
  if (file.exists(File) && !Overwrite && IsCurrent(readRDS(File), i))
    return('exists')
  tryCatch({
    Start <- Sys.time()
    Hist  <- readRDS(HistFile)
    Tuned <- MSEtool::TuneMP(Hist, CMPs$Fn[[i]],
                             Objective   = Objective,
                             Constraints = TuneConstraints(CMPs$FTargetSPR[i]),
                             Control     = Control,
                             parallel    = FALSE,
                             silent      = TRUE)
    Tuned@MPName <- CMPs$Name[i]
    Tuned@Settings$RunInfo <- RunInfo(Start)
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
  # one CMP of each MP and index set
  Hist <- readRDS(HistFile)
  for (i in BatchInd[!duplicated(CMPs[BatchInd, c('MP', 'IndexSet')])]) {
    cli::cli_h2(CMPs$Name[i])
    MSEtool::TuneMP(Hist, CMPs$Fn[[i]], Objective = Objective,
                    Constraints = TuneConstraints(CMPs$FTargetSPR[i]), Control = Control)
  }
} else {
  MSEtool::SetupParallel(workers = nWorkers)
  CheckInd <- BatchInd[!duplicated(CMPs[BatchInd, c('MP', 'IndexSet')])]
  Check <- furrr::future_map_chr(CheckInd, CheckWorker, .options = WorkerOptions)
  if (any(Check != 'OK')) {
    MSEtool::DisableParallel()
    cli::cli_abort("Projection failed in a parallel worker: {unique(Check[Check != 'OK'])}")
  }
  TuneLog <- furrr::future_map_chr(BatchInd, TuneCMP,
                                   .progress = TRUE,
                                   .options  = WorkerOptions)
  MSEtool::DisableParallel()
  print(data.frame(CMP = CMPs$Name[BatchInd], Result = TuneLog), row.names = FALSE)
}
