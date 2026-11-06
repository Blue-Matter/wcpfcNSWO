# Summarise the tuned CMPs (05d-Tune-CMPs.R), after the .tune files of every
# machine have been copied into objects/Tune. Writes Summary.csv and the tuned
# CMPs for the projections (TunedCMPs.rds)
source('analysis/05b-Define-CMPs.R')

Files <- file.path(TuneDir, paste0(CMPs$Name, '.tune'))
Tuned <- lapply(Files, \(f) if (file.exists(f)) readRDS(f))

Exists <- !vapply(Tuned, is.null, logical(1))
IsDone <- vapply(seq_along(Tuned), \(i) Exists[i] && IsCurrent(Tuned[[i]], i), logical(1))

if (any(!Exists))
  cli::cli_alert_warning("Not tuned ({sum(!Exists)} CMP{?s}): {.val {CMPs$Name[!Exists]}}.")
if (any(Exists & !IsDone))
  cli::cli_alert_warning("Tuned with different MP settings or tuning specification (re-tune with 05d-Tune-CMPs.R): {.val {CMPs$Name[Exists & !IsDone]}}.")
if (!any(IsDone))
  cli::cli_abort("No current tuning results in {.file {TuneDir}}.")

Summary <- CMPs[IsDone, c('Name', 'MP', 'HCR', 'FTargetSPR', 'ThRP', 'IndexSet', 'TACChange')]
Summary$tunepar    <- vapply(Tuned[IsDone], \(x) x@Args$tunepar, numeric(1))
Summary$TuneStatus <- vapply(Tuned[IsDone], \(x) x@Status, character(1))
Summary[[Objective$Name]] <- vapply(Tuned[IsDone], \(x) x@Objective, numeric(1))
for (nm in c('Safety', 'Status'))
  Summary[[nm]] <- vapply(Tuned[IsDone], \(x) {
    Tab <- MSEtool::TuneTable(x, 'Constraints')
    Tab$Value[Tab$Name == nm]
  }, numeric(1))

# where each tuning was run
Info <- function(x, nm) x@Settings$RunInfo[[nm]] %||% NA
Summary$Machine      <- vapply(Tuned[IsDone], \(x) as.character(Info(x, 'Machine')), character(1))
Summary$MSEtool      <- vapply(Tuned[IsDone], \(x) as.character(Info(x, 'MSEtool')), character(1))
Summary$MSEtoolSHA   <- vapply(Tuned[IsDone], \(x) as.character(Info(x, 'MSEtoolSHA')), character(1))
Summary$MSEtoolBuilt <- vapply(Tuned[IsDone], \(x) as.character(Info(x, 'MSEtoolBuilt')), character(1))
Summary$Minutes      <- vapply(Tuned[IsDone], \(x) as.numeric(Info(x, 'Minutes')), numeric(1))

# MSEtool source of each tuning: the GitHub commit, or (installed from a local
# copy, so no commit recorded) the build time, which differs between machines
Summary$MSEtoolSource <- ifelse(is.na(Summary$MSEtoolSHA), paste('built', Summary$MSEtoolBuilt),
                                substr(Summary$MSEtoolSHA, 1, 8))
Builds <- unique(Summary[c('MSEtool', 'MSEtoolSource')])
if (nrow(Builds) > 1) {
  cli::cli_alert_warning("CMPs were tuned with {nrow(Builds)} different MSEtool versions or builds; check they are the same commit:")
  print(stats::aggregate(Name ~ MSEtool + MSEtoolSource + Machine, data = Summary, FUN = length),
        row.names = FALSE)
}

print(Summary[, c('Name', 'tunepar', 'TuneStatus', Objective$Name, 'Safety', 'Status', 'Machine', 'Minutes')],
      row.names = FALSE, digits = 3)

if (any(Summary$TuneStatus == 'at_bound'))
  cli::cli_alert_warning("Catch still increasing at the end of the tuning range for {.val {Summary$Name[Summary$TuneStatus == 'at_bound']}}; widen {.arg Interval} in {.fn TuneControl}.")
if (any(Summary$TuneStatus == 'infeasible'))
  cli::cli_alert_warning("No tuning met the constraints for {.val {Summary$Name[Summary$TuneStatus == 'infeasible']}}.")

utils::write.csv(Summary, file.path(TuneDir, 'Summary.csv'), row.names = FALSE)

## Tuned CMPs for the projections: all feasible tunings
Keep <- Summary$TuneStatus != 'infeasible'
TunedCMPs <- lapply(Tuned[IsDone][Keep], \(x) x@MP)
names(TunedCMPs) <- Summary$Name[Keep]
saveRDS(TunedCMPs, file.path(TuneDir, 'TunedCMPs.rds'))
cli::cli_alert_success("{length(TunedCMPs)} tuned CMP{?s} saved for the projections.")
