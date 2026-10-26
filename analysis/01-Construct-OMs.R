# Import the operating models from SS3 Output

source('analysis/00-Specifications.R')

# ---- Import Operating Models, re-running SS3 if specified ----
OM_DF$Import <- FALSE

OM_DF$Import[c(1,2,3,5,8,9,10)] <- TRUE

for (i in seq_len(nrow(OM_DF))) {

  OMName <- OM_DF$OM[i]
  cli::cli_h1('OM {OMName}')

  dir <- OM_DF$Path[i]

  ## ---- Re-run SS3 if specified ----
  # (earlier version had a bug in the calculation of SBiomass)

  OM_DF$ReRunSS[i] <- FALSE # see comments below. Not a quick fix
  if (OM_DF$ReRunSS[i]) {
    # doesn't run with latest SS3 executable
    # out of scope to fix this - at this stage at least
    r4ss::run(dir = dir,
              exe = 'C:/Users/Admin/Documents/GitHub/wcpfcNSWO/ss3.exe',
              skipfinished = FALSE,
              verbose = TRUE)
  }

  ## ---- Import SS3 Output ----
  if (!OM_DF$Import[i]) next

  valid <- union(names(formals(MSEtool::ImportSS)),
                 names(formals(r4ss::SS_output)))

  RepList <- MSEtool::ImportSSReport(dir, silent = TRUE)

  args <- utils::modifyList(OM_Specs, list(SSDir = RepList,
                                           InterimAdvice = MakeInterimAdvice(RepList[[1]], RealCatch, IntYrs),
                                           LengthUnits = 'cm',
                                           WeightUnits = 'kg',
                                           R0Units = 1000,
                                           CatchUnits = 'Biomass'))


  OM <- do.call(MSEtool::ImportSS, args)
  MSEtool::Save(OM, file.path('objects/OM/', paste0(OMName, '_all_fleets.om')), overwrite = TRUE)

  # Combine all fleets into one to improve run time
  OM_Combined <- MSEtool::CombineFleets(OM)

  MSEtool::Save(OM_Combined, file.path('objects/OM', paste0(OMName, '.om')), overwrite = TRUE)

}
