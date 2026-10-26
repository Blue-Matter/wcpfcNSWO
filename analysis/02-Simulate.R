# Simulate historical dynamics for the operating models
library(wcpfcNSWO)

OM_DF <- ListSS3Runs() |>
  dplyr::mutate(Simulate  = TRUE,
                AllFleets = FALSE)


for (i in seq_len(nrow(OM_DF))) {
  if (!OM_DF$Simulate[i]) next

  OMName <- OM_DF$OM[i]
  cli::cli_h1('OM {OMName}')

  file <- if (OM_DF$AllFleets[i]) {
    file.path('objects/OM', paste0(OMName, "_all_fleets.om"))
  } else {
    file.path('objects/OM', paste0(OMName, ".om"))
  }

  if (!file.exists(file)) {
    cli::cli_alert("{file} does not exist. Skipping ")
    next
  }

  OM <- readRDS(file)

  Hist <- MSEtool::Simulate(OM)

  file <- if (OM_DF$AllFleets[i]) {
    file.path('objects/Hist', paste0(OMName, "_all_fleets.hist"))
  } else {
    file.path('objects/Hist', paste0(OMName, ".hist"))
  }

  MSEtool::Save(Hist, file, overwrite = TRUE)
}


