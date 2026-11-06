# Compare the historical dynamics of the OMs (objects/Hist) with the SS3 output
# Run after 03-Simulate.R

library(MSEtool)
library(wcpfcNSWO)
library(ggplot2)

FigDir <- 'analysis/figures/OM_SS3_Comparison'
dir.create(FigDir, recursive = TRUE, showWarnings = FALSE)

OM_DF <- ListSS3Runs() |>
  dplyr::mutate(HistFile = file.path('objects/Hist', paste0(OM, '.hist'))) |>
  dplyr::filter(file.exists(HistFile))

#' Annual time series of the OM (simulation 1) and SS3: spawning biomass
#' (female), recruitment, total biomass (start of year) and landings (all fleets)
CompareSS3 <- function(SSDir, Hist) {
  Rep <- ImportSSReport(SSDir, silent = TRUE)[[1]]
  Cmp <- CompareSS(Rep, Hist, silent = TRUE)
  Yrs <- Rep$startyr:Rep$endyr

  # SS3 spawning biomass (female); OM at the start of each year
  dq <- Rep$derived_quants
  SB_SS3 <- data.frame(Year = Yrs, Value = dq$Value[match(paste0('SSB_', Yrs), dq$Label)], Model = 'SS3')
  SB_OM  <- SBiomass(Hist) |>
    dplyr::filter(Sim == 1, Stock == 'Female', as.numeric(as.character(Year)) %in% Yrs) |>
    dplyr::transmute(Year = as.numeric(as.character(Year)), Value, Model = 'OM')

  SB0 <- c(OM = SB0(Hist)[1, 'Female', 1], SS3 = dq$Value[dq$Label == 'SSB_Virgin'])

  Biomass <- Cmp$Biomass$df |>
    dplyr::filter(Year %in% Yrs) |>
    dplyr::group_by(Year, Model) |>
    dplyr::summarise(Value = sum(Value), .groups = 'drop')

  Landings <- Cmp$Landings$df |>
    dplyr::mutate(Year = floor(Year)) |>
    dplyr::filter(Year %in% Yrs) |>
    dplyr::group_by(Year, Model) |>
    dplyr::summarise(Value = sum(Value, na.rm = TRUE), .groups = 'drop')

  Recruits <- Cmp$Recruits$df |>
    dplyr::mutate(Year = floor(Year)) |>
    dplyr::filter(Year %in% Yrs) |>
    dplyr::group_by(Year, Model) |>
    dplyr::summarise(Value = sum(Value), .groups = 'drop')

  Depletion <- dplyr::bind_rows(SB_SS3, SB_OM) |>
    dplyr::mutate(Value = Value / SB0[Model])

  list(SB0 = SB0,
       df = dplyr::bind_rows(`Spawning biomass` = dplyr::bind_rows(SB_SS3, SB_OM),
                             `SB/SB0`           = Depletion,
                             Recruitment        = Recruits,
                             `Total biomass`    = Biomass,
                             Landings           = Landings,
                             .id = 'Variable'))
}


Comparison <- purrr::map(seq_len(nrow(OM_DF)), \(i) {
  cli::cli_alert_info('OM {OM_DF$OM[i]}')
  CompareSS3(OM_DF$Path[i], readRDS(OM_DF$HistFile[i]))
}) |> stats::setNames(OM_DF$OM)

# ---- Summary ----
# Mean and maximum absolute relative error (%) over the historical years
Summary <- purrr::imap_dfr(Comparison, \(x, om) {
  x$df |>
    tidyr::pivot_wider(names_from = Model, values_from = Value) |>
    dplyr::mutate(RE = 100 * abs(OM - SS3) / SS3) |>
    dplyr::group_by(Variable) |>
    dplyr::summarise(MARE = mean(RE, na.rm = TRUE), MaxRE = max(RE, na.rm = TRUE), .groups = 'drop') |>
    dplyr::mutate(OM = om, .before = 1)
})

SB0Table <- purrr::imap_dfr(Comparison, \(x, om)
  data.frame(OM = om, SB0_OM = x$SB0[['OM']], SB0_SS3 = x$SB0[['SS3']],
             RE = 100 * (x$SB0[['OM']] / x$SB0[['SS3']] - 1)))

# utils::write.csv(Summary, file.path(FigDir, 'MARE.csv'), row.names = FALSE)
# utils::write.csv(SB0Table, file.path(FigDir, 'SB0.csv'), row.names = FALSE)

# ---- Plots ----
for (om in names(Comparison)) {
  p <- ggplot(Comparison[[om]]$df, aes(Year, Value, colour = Model, linetype = Model)) +
    geom_line() +
    facet_wrap(~Variable, scales = 'free_y', ncol = 2) +
    expand_limits(y = 0) +
    scale_colour_manual(values = c(OM = 'black', SS3 = 'red')) +
    labs(x = NULL, y = NULL, colour = NULL, linetype = NULL,
         title = paste0('OM ', om, ': ', OM_DF$Description[OM_DF$OM == om])) +
    theme_bw() +
    theme(legend.position = 'bottom')
  ggsave(file.path(FigDir, paste0(om, '.png')), p, width = 8, height = 8, dpi = 200)
}

tidyr::pivot_wider(Summary, id_cols = OM, names_from = Variable, values_from = MARE)
SB0Table
