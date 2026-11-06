# Select the MP settings not set by tuning or by the HCR (ConfigCandidates in
# 05b-Define-CMPs.R) for each MP: the configuration with the highest catch when
# tuned to the Safety and Status constraints. Selected on the base case OM, with
# the HCR, index set, and TAC change limit of ConfigCMP, and used for all CMPs
# of that MP in 05d-Tune-CMPs.R. Copy ConfigsFile to every machine used for tuning
source('analysis/05b-Define-CMPs.R')

HistFile <- normalizePath('objects/Hist/00.hist')

ConfigMPs <- 'IT' # names(ConfigCandidates)  # MPs to select settings for
nWorkers  <- 12                       # each worker needs ~8 GB of RAM to project an MP

Control <- MSEtool::TuneControl(Interval    = TuneInterval,
                                nGridConfig = 5,
                                nSimConfig  = 50,  # simulations used to compare configurations
                                nRefine     = 2)   # best configurations tuned on all simulations

# Configurations of an MP: all combinations of its candidate values
Candidates <- function(MP) {
  Grid <- expand.grid(ConfigCandidates[[MP]], KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  lapply(seq_len(nrow(Grid)), \(i) as.list(Grid[i, , drop = FALSE]))
}

Hist <- readRDS(HistFile)
MSEtool::SetupParallel(workers = nWorkers)

for (mp in ConfigMPs) {
  cli::cli_h1('Configurations: {mp}')
  i <- which(CMPs$MP == mp & CMPs$HCR == ConfigCMP$HCR & CMPs$IndexSet == ConfigCMP$IndexSet &
               CMPs$TACChange == ConfigCMP$TACChange)
  Cand <- Candidates(mp)
  Args <- lapply(Cand, \(x) ConfigArgs(mp, ConfigCMP$HCR, x))

  Tuned <- MSEtool::TuneMP(Hist, CMPs$Fn[[i]],
                           Objective   = Objective,
                           Constraints = TuneConstraints(CMPs$FTargetSPR[i]),
                           Configs     = Args,
                           Control     = Control,
                           parallel    = TRUE)
  Tuned@MPName <- mp
  saveRDS(Tuned, file.path(TuneDir, paste0('Configs_', mp, '.tune')))

  Tab <- MSEtool::TuneTable(Tuned, 'Configs')
  Tab$Label <- vapply(Cand, ConfigLabel, character(1))[Tab$Config]
  print(Tab, row.names = FALSE)

  if (Tuned@Status == 'infeasible') {
    cli::cli_alert_warning("No configuration of {mp} met the constraints; settings not updated.")
  } else {
    # the candidate whose MP arguments were selected
    Selected <- which(vapply(Args, \(a) isTRUE(all.equal(Tuned@Args[names(a)], a, check.attributes = FALSE)),
                             logical(1)))
    SelectedConfigs[[mp]] <- Cand[[Selected[1]]]
    saveRDS(SelectedConfigs, ConfigsFile)
    cli::cli_alert_success("{mp}: {ConfigLabel(SelectedConfigs[[mp]])}")
  }
}

MSEtool::DisableParallel()
SelectedConfigs
