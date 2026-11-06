#' Create an SS3 run with effort creep in the indices of abundance
#'
#' Copies the SS3 input files of an existing run and deflates the index
#' observations for an assumed annual increase in fishing efficiency (effort
#' creep): `obs_y / (1 + creep)^(y - ref_year)`. With a separate catchability
#' for each index, only the trend matters, so `ref_year` only rescales `q`.
#'
#' @param from character. Directory of the SS3 run to copy the input files from
#' @param to character. Directory of the new SS3 run. Created if it doesn't exist
#' @param creep numeric. Annual effort creep, e.g. `0.01` for 1% per year
#' @param fleets optional numeric vector. Fleet numbers of the indices to
#'   adjust. `NULL` (default) adjusts all indices in the data file
#' @param ref_year optional numeric. Year with no adjustment. `NULL` (default)
#'   uses the first year of each index
#'
#' @returns invisibly, a `data.frame` with the adjustment for each fleet
#' @export
AddEffortCreep <- function(from, to, creep, fleets = NULL, ref_year = NULL) {

  starter <- r4ss::SS_readstarter(file.path(from, 'starter.ss'), verbose = FALSE)
  files   <- c('starter.ss', 'forecast.ss', starter$datfile, starter$ctlfile)

  if (!dir.exists(to)) dir.create(to, recursive = TRUE)
  file.copy(file.path(from, files), to, overwrite = TRUE)

  datfile <- file.path(to, starter$datfile)
  dat     <- r4ss::SS_readdat(datfile, verbose = FALSE)

  # negative fleet numbers are observations excluded from the likelihood
  cpue  <- dat$CPUE
  fleet <- abs(cpue$index)
  if (is.null(fleets)) fleets <- unique(fleet)

  adjust <- fleet %in% fleets
  ref    <- if (is.null(ref_year)) stats::ave(cpue$year, fleet, FUN = min) else rep(ref_year, nrow(cpue))
  cpue$obs[adjust] <- cpue$obs[adjust] / (1 + creep)^(cpue$year[adjust] - ref[adjust])

  dat$CPUE <- cpue
  r4ss::SS_writedat(dat, datfile, overwrite = TRUE, verbose = FALSE)

  out <- data.frame(Fleet    = fleets,
                    Name     = dat$fleetnames[fleets],
                    RefYear  = ref[match(fleets, fleet)],
                    LastYear = tapply(cpue$year, fleet, max)[as.character(fleets)],
                    Creep    = creep,
                    row.names = NULL)
  out$Multiplier <- 1 / (1 + creep)^(out$LastYear - out$RefYear)
  invisible(out)
}
