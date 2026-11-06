# Set up the SS3 runs in SS3/ from the 2023 assessment repository
# https://github.com/noaa-pifsc/WCNPOSWO-2023
#
# The original runs used SS3 v3.30.20, which has a bug in the spawning biomass
# calculation. They are re-run with the latest executable (ss3.exe, v3.30.23.1)
# in 01b-Run-SS3.R.

library(wcpfcNSWO)

SourceDir <- "../WCNPOSWO-2023/Final Base-case"
SS3Dir    <- "SS3"

# The published base case used male L_at_Amin = 83.4 (from ss.par), not the
# 83.2 in the control file. Used in all runs except 14 (Amin = 1)
L_at_Amin_Mal <- 83.4

# ---- Copy the input files ----
Runs <- data.frame(Source = c(SourceDir, list.dirs(file.path(SourceDir, 'Sensitivities'), recursive = FALSE)))
Runs$Dir <- c(file.path(SS3Dir, 'Base-case'),
              file.path(SS3Dir, 'Sensitivities', basename(Runs$Source[-1])))

for (i in seq_len(nrow(Runs))) {
  starter <- r4ss::SS_readstarter(file.path(Runs$Source[i], 'starter.ss'), verbose = FALSE)
  files   <- c('starter.ss', 'forecast.ss', starter$datfile, starter$ctlfile)
  if (!dir.exists(Runs$Dir[i])) dir.create(Runs$Dir[i], recursive = TRUE)
  file.copy(file.path(Runs$Source[i], files), Runs$Dir[i], overwrite = TRUE)
  file.copy(file.path(Runs$Source[i], 'ss.par'), file.path(Runs$Dir[i], 'ss_v3.30.20.par'), overwrite = TRUE)
}

# ---- Initialise parameters from the control file ----

#' Copy the estimated parameters in `parfile` to the INIT column of the control
#' file, and set `init_values_src = 0`

ParToCtl <- function(dir, parfile = 'ss_v3.30.20.par') {
  starter <- r4ss::SS_readstarter(file.path(dir, 'starter.ss'), verbose = FALSE)
  ctlfile <- file.path(dir, starter$ctlfile)
  dat <- r4ss::SS_readdat(file.path(dir, starter$datfile), verbose = FALSE)
  ctl <- r4ss::SS_readctl(ctlfile, datlist = dat, use_datlist = TRUE, verbose = FALSE)
  par <- r4ss::SS_readpar_3.30(file.path(dir, parfile), datsource = dat, ctlsource = ctl,
                               verbose = FALSE)

  # control file block = ss.par block; time-varying selectivity parameters
  # follow the base parameters in ss.par
  Blocks <- list(MG_parms         = par$MG_parms$ESTIM,
                 SR_parms         = par$SR_parms$ESTIM,
                 Q_parms          = par$Q_parms$ESTIM,
                 init_F           = par$init_F,
                 size_selex_parms = par$S_parms$ESTIM[seq_len(nrow(ctl$size_selex_parms))])
  if (!is.null(ctl$size_selex_parms_tv))
    Blocks$size_selex_parms_tv <- par$S_parms$ESTIM[-seq_len(nrow(ctl$size_selex_parms))]

  for (b in names(Blocks)) {
    est <- Blocks[[b]]
    if (length(est) != nrow(ctl[[b]]))
      cli::cli_abort('{dir}: {b} has {nrow(ctl[[b]])} parameters in the control file and {length(est)} in {parfile}')
    estimated <- ctl[[b]]$PHASE > 0
    ctl[[b]]$INIT[estimated] <- est[estimated]
  }

  if (ctl$MG_parms['L_at_Amin_Mal_GP_1', 'INIT'] == 83.2)
    ctl$MG_parms['L_at_Amin_Mal_GP_1', 'INIT'] <- L_at_Amin_Mal

  r4ss::SS_writectl(ctl, ctlfile, overwrite = TRUE, verbose = FALSE)

  starter$init_values_src <- 0
  r4ss::SS_writestarter(starter, dir, overwrite = TRUE, verbose = FALSE)
}

for (dir in Runs$Dir) {
  cli::cli_alert_info(dir)
  ParToCtl(dir)
}

# ---- Forecast ----
# Turn off forecast
for (dir in Runs$Dir) {
  file <- file.path(dir, 'forecast.ss')
  fc   <- readLines(file)
  fc   <- sub('^2 # Forecast: 0=none', '0 # Forecast: 0=none', fc)
  writeLines(fc, file)
}

# ---- Effort creep sensitivities ----

# Use e.g. fleets = c(20:24, 26:27) to exclude the S6 recruitment index
CreepFleets <- NULL # all indices

AddEffortCreep(from=file.path(SS3Dir, 'Base-case'),
               to=file.path(SS3Dir, 'Sensitivities', '20a_EffortCreep_1pct'),
               creep = 0.01,
               fleets = CreepFleets)
AddEffortCreep(file.path(SS3Dir, 'Base-case'),
               file.path(SS3Dir, 'Sensitivities', '20b_EffortCreep_2pct'),
               creep = 0.02, fleets = CreepFleets)
