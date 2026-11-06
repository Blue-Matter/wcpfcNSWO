# Run the SS3 models in SS3/ with the latest SS3 executable (ss3.exe, v3.30.23.1)
# Set up the runs with 01a-Setup-SS3.R first

library(wcpfcNSWO)

SS3Exe    <- normalizePath('ss3.exe')
SourceDir <- "../WCNPOSWO-2023/Final Base-case" # original v3.30.20 runs, for comparison

SS3_DF <- ListSS3Runs('SS3/Base-case', 'SS3/Sensitivities')
stopifnot(all(startsWith(SS3_DF$Path, 'SS3/')))

# ---- Run SS3 ----
# set Run to FALSE to skip a run
SS3_DF$Run <- TRUE

nCores  <- min(sum(SS3_DF$Run), parallel::detectCores() - 2)
cl      <- parallel::makeCluster(nCores)
RunTime <- parallel::parLapply(cl, SS3_DF$Path[SS3_DF$Run], function(dir, exe) {
  system.time(r4ss::run(dir, exe = exe, skipfinished = FALSE, verbose = FALSE))[['elapsed']]
}, exe = SS3Exe)

parallel::stopCluster(cl)

# ---- Summary ----
#' Key outputs of an SS3 run
SS3Summary <- function(dir) {
  if (!file.exists(file.path(dir, 'Report.sso')))
    return(data.frame(Version = NA))
  rep <- r4ss::SS_output(dir, covar = file.exists(file.path(dir, 'covar.sso')), compfile = 'none',
                         verbose = FALSE, printstats = FALSE, hidewarn = TRUE)
  dq  <- rep$derived_quants
  val <- function(label) dq$Value[match(label, dq$Label)]
  data.frame(Version  = sub(';.*', '', sub('^#V', '', rep$SS_version)),
             NLL      = rep$likelihoods_used['TOTAL', 'values'],
             MaxGrad  = rep$maximum_gradient_component,
             Hessian  = !is.null(rep$CoVar),
             R0       = exp(rep$parameters['SR_LN(R0)', 'Value']),
             h        = rep$parameters['SR_BH_steep', 'Value'],
             SB0      = val('SSB_Virgin'),
             SB2021   = val(paste0('SSB_', rep$endyr)),
             Depl2021 = val(paste0('SSB_', rep$endyr)) / val('SSB_Virgin'),
             SBMSY    = val('SSB_MSY'),
             MSY      = val('Dead_Catch_MSY'))
}

OldPath <- c(SourceDir, file.path(SourceDir, 'Sensitivities', basename(SS3_DF$Path[-1])))

New <- dplyr::bind_rows(lapply(SS3_DF$Path, SS3Summary))
Old <- dplyr::bind_rows(New[0, ], lapply(OldPath, SS3Summary))

Summary <- cbind(SS3_DF[c('OM', 'Description')],
                 Converged = !is.na(New$MaxGrad) & New$MaxGrad < 1e-3 & New$Hessian,
                 New,
                 Old |> dplyr::select(-Version) |> dplyr::rename_with(\(x) paste0(x, '_old')))

utils::write.csv(Summary, 'SS3/SS3_Run_Summary.csv', row.names = FALSE)
Summary
