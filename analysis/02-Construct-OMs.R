# Import the operating models from SS3 Output

source('analysis/00-Specifications.R')

# ---- Import Operating Models ----

# SS3 runs are re-run with the latest SS3 executable in 01b-Run-SS3.R
# The OMs (OM_DF$Include) are selected in ListSS3Runs(); set Import to FALSE to
# skip an OM, e.g. OM_DF$Import <- OM_DF$OM %in% c('10')

for (i in seq_len(nrow(OM_DF))) {

  OMName <- OM_DF$OM[i]
  cli::cli_h1('OM {OMName}')

  dir <- OM_DF$Path[i]

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

  # Effort creep OMs are conditioned on the creep-adjusted indices, but the MPs
  # see the observed indices (applied after Simulate() in 03-Simulate.R)
  if (grepl('EffortCreep', dir)) {
    Observed <- readRDS(file.path('objects/OM', '00_all_fleets.om'))@Data[[1]]@Survey
    OM <- StoreObservedIndices(OM, Observed)
  }

  MSEtool::Save(OM, file.path('objects/OM/', paste0(OMName, '_all_fleets.om')), overwrite = TRUE)

  # Combine all fleets into one to improve run time
  OM_Combined <- MSEtool::CombineFleets(OM)

  MSEtool::Save(OM_Combined, file.path('objects/OM', paste0(OMName, '.om')), overwrite = TRUE)

}
