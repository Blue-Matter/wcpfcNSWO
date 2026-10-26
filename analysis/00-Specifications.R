
library(wcpfcNSWO)

# ---- SS3 Output / Operating Models ----
# Path to the SS3 base-case model directory
# from https://github.com/noaa-pifsc/WCNPOSWO-2023
SSDir_Base <- "../WCNPOSWO-2023/Final Base-case"
SSDir_Sens <- file.path(SSDir_Base, 'Sensitivities')
OM_DF      <- ListSS3Runs(SSDir_Base, SSDir_Sens)


# ---- OM Specifications ----
nSim        <- 100    # number of simulations per OM
Seasons     <- 4
Interval    <- 3      # management interval: decided at 2026 NC Meeting
FirstYear   <- 1975   # first assessment data year
CurrentYear <- 2021   # terminal assessment year
MPStartYear <- 2028   # first year TAC advice is set by MP

DataLag     <- 2     # Assuming 2025 data will be available when
                     # TAC is calculated in 2027

IntYrs      <- seq(CurrentYear + 1, by = 1, to = MPStartYear-1)
mpyears     <- 30 # number of years where the MP is active (ie fromMPStartYear )
ProjYrs     <- seq(MPStartYear, by = 1, length.out = mpyears)
pYear       <- length(c(IntYrs, ProjYrs))

# ---- Interim Advice ----
# All catches and TACs in biomass (t); OMs are imported with CatchUnits = 'Biomass'.
# Calculated for each OM in 01-Construct-OMs.R, as the SS3 catch biomass differs among runs

## Real data: Michelle's email 24 Sept 2026 (F3 and F9 in numbers of fish, F19 in t)
RealCatch <- data.frame(Year  = rep(2022:2025, each = 3),
                        Fleet = c("F3_US_WCNPO_LL_shallow_late",
                                  "F9_US_WCNPO_LL_deep",
                                  "F19_WCPFC"),
                        Mean  = c(9611, 3629, 2243,
                                  8567, 4030, 2098,
                                  8500, 4149, 1638,
                                  11715, 4269, NA_real_),
                        CV    = c(rep(0, 11), NA_real_))

#' InterimAdvice (t) for one SS3 run: `RealCatch` where available, otherwise
#' the mean and CV of the last 5 and max of the last 10 years of catch per fleet
MakeInterimAdvice <- function(replist, RealCatch, IntYrs) {
  FleetNames   <- unique(replist$catch$Fleet_Name)
  NumberFleets <- FleetNames[replist$catch_units[replist$IsFishFleet] == 2]

  # SS3 catch biomass, including the predicted biomass of fleets with catch in numbers
  SSCatch <- replist$catch |>
    dplyr::filter(Yr %in% replist$startyr:replist$endyr)

  # t per 1000 fish, last 5 assessment years
  MeanWeight <- SSCatch |>
    dplyr::filter(Yr > replist$endyr - 5, Fleet_Name %in% NumberFleets) |>
    dplyr::group_by(Fleet = Fleet_Name) |>
    dplyr::summarise(MeanWeight = sum(kill_bio) / sum(kill_num), .groups = 'drop')

  RealCatch <- RealCatch |>
    dplyr::left_join(MeanWeight, by = 'Fleet') |>
    dplyr::mutate(Mean = dplyr::if_else(Fleet %in% NumberFleets, Mean / 1000 * MeanWeight, Mean)) |>
    dplyr::select(-MeanWeight)

  InterimAdvice <- data.frame(Year  = rep(IntYrs, each = length(FleetNames)),
                              Fleet = FleetNames,
                              Type  = 'TAC',
                              Mean  = NA_real_,
                              Max   = NA_real_,
                              CV    = NA_real_) |>
    dplyr::rows_update(RealCatch, by = c("Year", "Fleet"), unmatched = "ignore")

  ## Observed catches (t): SS3 + real data
  observed <- SSCatch |>
    dplyr::group_by(Year = Yr, Fleet = Fleet_Name) |>
    dplyr::summarise(Catch = sum(kill_bio), .groups = 'drop') |>
    # years in the SS data with no catch record for a fleet = zero catch
    tidyr::complete(Year = seq(min(Year), max(Year)), Fleet = FleetNames,
                    fill = list(Catch = 0)) |>
    dplyr::rows_upsert(dplyr::select(RealCatch, Year, Fleet, Catch = Mean),
                       by = c("Year", "Fleet")) |>
    # NA = unknown, not zero
    dplyr::filter(!is.na(Catch))

  ## Assumed Mean/CV (last 5 obs years per fleet) and Max (last 10)
  assumed <- observed |>
    dplyr::group_by(Fleet) |>
    dplyr::summarise(
      Mean = mean(Catch[Year > max(Year) - 5]),
      SD   = stats::sd(Catch[Year > max(Year) - 5]),
      Max  = max(Catch[Year > max(Year) - 10]),
      .groups = 'drop'
    ) |>
    dplyr::mutate(CV = dplyr::if_else(Mean > 0, SD / Mean, 0)) |>
    dplyr::select(Fleet, Mean, Max, CV) |>
    dplyr::cross_join(data.frame(Year = IntYrs))

  dplyr::rows_patch(InterimAdvice, assumed, by = c("Year", "Fleet"))
}

OM_Specs <- list(
  nSim          = nSim,
  Name          = "North Pacific Swordfish",
  StockName     = c("Female", "Male"),
  CommonName    = 'Swordfish',
  Species       = "Xiphias gladius",
  Region        = "North Pacific",
  Interval      = Interval,
  MPStartYear   = MPStartYear,
  DataLag       = DataLag,
  pYear         = pYear
)


