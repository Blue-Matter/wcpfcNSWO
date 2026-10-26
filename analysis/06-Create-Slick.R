# Build the Slick object for results visualisation
library(wcpfcNSWO)

MSEDir  <- 'objects/MSE'
TuneDir <- 'objects/Tune'
FigDir  <- 'analysis/figures/Slick'

# Reference levels of the candidate management objectives
LRP                <- 0.17       # limit reference point: SB / SB0
FTargetSPR         <- 0.4        # F target of HCRs 1-3: F40%
StabilityThreshold <- 0.1        # maximum TAC change between management cycles for `Stability`
NoChangeThreshold  <- 0.01       # maximum TAC change counted as no management change
CatchRefYears      <- 2008:2010  # reference catch for `Yield_Hist`
PI_Min             <- c(Safety = 0.8, Status = 0.5)  # minimum probabilities of the objectives

# CMPs in the Slick object (NULL for all CMPs retained by 04b-Tune-CMPs.R)
SelectedCMPs <- NULL

TunedCMPs       <- readRDS(file.path(TuneDir, 'TunedCMPs.rds'))
TuneSummary     <- utils::read.csv(file.path(TuneDir, 'Summary.csv'))
ConfigsFile     <- file.path(TuneDir, 'Configs.rds')
SelectedConfigs <- if (file.exists(ConfigsFile)) readRDS(ConfigsFile) else list()

# ---- OMs with MSE results ----
OM_Design <- ListSS3Runs() |>
  dplyr::filter(Include) |>
  dplyr::mutate(HistFile = file.path('objects/Hist', paste0(OM, '.hist')),
                MSEFile  = file.path(MSEDir, paste0(OM, '.mse'))) |>
  dplyr::filter(file.exists(MSEFile))
if (!nrow(OM_Design))
  cli::cli_abort("No MSE results in {.file {MSEDir}}; run 05-Run-Projections.R.")
OM_Design$OM_Name <- paste0(OM_Design$OM, ': ', OM_Design$Description)
nOM <- nrow(OM_Design)

# ---- MP metadata ----
MP_all <- names(TunedCMPs)
if (!is.null(SelectedCMPs)) {
  if (length(setdiff(SelectedCMPs, MP_all)))
    cli::cli_abort("Not tuned CMPs: {.val {setdiff(SelectedCMPs, MP_all)}}.")
  MP_all <- SelectedCMPs
}
nMP    <- length(MP_all)
MPInfo <- TuneSummary[match(MP_all, TuneSummary$Name), ]

MP_Type <- c(IR = 'Index Rate', IT = 'Index Target', SP = 'Surplus Production')

MP_TypeDescription <- c(
  IR = 'Index-based MP: the TAC is a catch/index rate multiplied by the current index. The rate is reduced linearly from the threshold reference point (ThRP) to Fmin (F90%/F40%) at the limit reference point (LRP), with stock status as the index relative to its level at SB0.',
  IT = 'Index-based MP: the TAC is a reference catch, constant above the threshold reference point (ThRP) and reduced linearly to the catch of Fmin (F90%/F40%) at the limit reference point (LRP), with stock status as the index relative to its level at SB0.',
  SP = 'Model-based MP: a surplus production model is fitted to the catch and index, and F is a multiple of the estimated FMSY, reduced linearly from the threshold reference point (ThRP) to Fmin (F90%/F40%) at the limit reference point (LRP), with stock status as the estimated B/K.'
)

ConfigLabel <- vapply(names(MP_Type), \(mp) {
  Config <- SelectedConfigs[[mp]][setdiff(names(SelectedConfigs[[mp]]), 'HCR')]
  if (!length(Config)) return('')
  paste0(' Settings: ', paste(names(Config), vapply(Config, deparse, character(1)),
                              sep = ' = ', collapse = ', '), '.')
}, character(1))

MP_Code  <- MP_all
MP_Label <- sprintf('%s HCR%d %d%%', MP_Type[MPInfo$MP], MPInfo$HCR, round(100 * MPInfo$TACChange))
MP_Description <- sprintf(
  '%s HCR %d: ThRP = %d%% SB0 and LRP = %d%% SB0, with TAC changes limited to %d%% between management cycles.%s Tuned in the base case OM to maximise mean catch subject to P(SB > LRP in every year) >= %.2g and P(F <= F40%%) >= %.2g (tunepar = %.3g).',
  MP_TypeDescription[MPInfo$MP], MPInfo$HCR, round(100 * MPInfo$ThRP), round(100 * LRP),
  round(100 * MPInfo$TACChange), ConfigLabel[MPInfo$MP], PI_Min[['Safety']], PI_Min[['Status']], MPInfo$tunepar)

# ---- Performance indicators ----
# Candidate management objectives (Table 1): Safety, Status, Stability,
# probability of no management change, and Yield
Table1_Codes <- c('Safety', 'Status', 'FTarget_F', 'Stability', 'NoChange', 'Yield_Hist')

PM_Codes <- c(Table1_Codes,
              'Yield_All',
              'Yield_Short',
              'Yield_Medium',
              'Yield_Long',

              'Safety_Short',
              'Safety_Medium',
              'Safety_Long',

              'Depletion_All',
              'Depletion_Short',
              'Depletion_Medium',
              'Depletion_Long',
              'Depletion_Min',

              'Kobe_Green',
              'SB_SBMSY_All',
              'F_FMSY_All',

              'AAVY')

nPI_pm <- length(PM_Codes)

FTargetLabel <- paste0('F', 100 * FTargetSPR, '%')

PM_Label <- c(
  Safety           = 'Probability SB > LRP',
  Status           = paste0('Probability F <= F target (', FTargetLabel, ')'),
  FTarget_F        = 'F target / F',
  Stability        = paste0('Catch stability (TAC change <= ', 100 * StabilityThreshold, '%)'),
  NoChange         = 'Probability of no management change',
  Yield_Hist       = paste0('Probability catch >= ', paste(range(CatchRefYears), collapse = '-'), ' catch'),

  Yield_All        = 'Mean catch (all years)',
  Yield_Short      = 'Mean catch (years 1-10)',
  Yield_Medium     = 'Mean catch (years 11-20)',
  Yield_Long       = 'Mean catch (years 21-30)',

  Safety_Short     = 'Probability SB > LRP (years 1-10)',
  Safety_Medium    = 'Probability SB > LRP (years 11-20)',
  Safety_Long      = 'Probability SB > LRP (years 21-30)',

  Depletion_All    = 'SB/SB0 (all years)',
  Depletion_Short  = 'SB/SB0 (years 1-10)',
  Depletion_Medium = 'SB/SB0 (years 11-20)',
  Depletion_Long   = 'SB/SB0 (years 21-30)',
  Depletion_Min    = 'Minimum SB/SB0',

  Kobe_Green       = 'Probability Kobe green quadrant',
  SB_SBMSY_All     = 'SB/SBMSY (all years)',
  F_FMSY_All       = 'F/FMSY (all years)',

  AAVY             = 'Average TAC variability',

  Rel_Yield        = 'Relative mean catch'
)

Safety_Description <- paste0('Probability that spawning biomass remains above the limit reference point (LRP; ', 100 * LRP,
                             '% SB0) in every year')

PM_Description <- c(
  Safety           = paste0(Safety_Description, ' of the projection period: a simulation that falls below the LRP in any year is a breach.'),
  Status           = paste0('Probability that fishing mortality is at or below the F target (', FTargetLabel, ' of the HCR), over all projection years.'),
  FTarget_F        = paste0('Median ratio of the F target (', FTargetLabel, ') to fishing mortality over the projection years; values above 1 mean F is below the target.'),
  Stability        = paste0('Probability that the TAC changes by no more than ', 100 * StabilityThreshold, '% between consecutive management cycles.'),
  NoChange         = paste0('Probability of no management change: the probability that the TAC changes by no more than ', 100 * NoChangeThreshold, '% between consecutive management cycles.'),
  Yield_Hist       = paste0('Probability that the annual catch is at least the mean historical catch in ',
                            paste(range(CatchRefYears), collapse = '-'), ', over all projection years.'),

  Yield_All        = 'Mean annual catch (t) over all projection years.',
  Yield_Short      = 'Mean annual catch (t) over the first 10 projection years (short-term).',
  Yield_Medium     = 'Mean annual catch (t) over the middle 10 projection years (medium-term).',
  Yield_Long       = 'Mean annual catch (t) over the final 10 projection years (long-term).',

  Safety_Short     = paste0(Safety_Description, ' of the first 10 projection years (short-term).'),
  Safety_Medium    = paste0(Safety_Description, ' of the middle 10 projection years (medium-term).'),
  Safety_Long      = paste0(Safety_Description, ' of the final 10 projection years (long-term).'),

  Depletion_All    = 'Mean spawning biomass relative to the equilibrium unfished spawning biomass (SB/SB0) over all projection years.',
  Depletion_Short  = 'Mean SB/SB0 over the first 10 projection years (short-term).',
  Depletion_Medium = 'Mean SB/SB0 over the middle 10 projection years (medium-term).',
  Depletion_Long   = 'Mean SB/SB0 over the final 10 projection years (long-term).',
  Depletion_Min    = 'Lowest annual SB/SB0 reached during the projection.',

  Kobe_Green       = 'Probability that SB > SBMSY and F < FMSY in the same year, over all projection years.',
  SB_SBMSY_All     = 'Mean annual SB/SBMSY over all projection years.',
  F_FMSY_All       = 'Mean annual F/FMSY over all projection years.',

  AAVY             = 'Mean absolute relative change in the TAC between management cycles.',

  Rel_Yield        = 'Mean annual catch (all years) relative to the highest-yielding MP in the same OM.'
)

Spider_Codes   <- c('Safety', 'Status', 'Stability', 'NoChange', 'Yield_Hist', 'Rel_Yield')
Headline_Codes <- c(Table1_Codes, 'Yield_All')
Tradeoff_Codes <- c('Safety', 'Yield_All', 'Status', 'Stability', 'NoChange', 'Yield_Hist', 'Depletion_All')

TS_Codes <- c('SB_SB0', 'F_FTarget', 'Yield', 'SB_SBMSY', 'F_FMSY')
TS_Label <- c(SB_SB0 = 'SB/SB0', F_FTarget = paste0('F/', FTargetLabel), Yield = 'Yield',
              SB_SBMSY = 'SB/SBMSY', F_FMSY = 'F/FMSY')
TS_Description <- c(
  SB_SB0   = 'Spawning biomass relative to the equilibrium unfished spawning biomass.',
  F_FTarget = paste0('Fishing mortality relative to the F target (', FTargetLabel, ').'),
  Yield     = 'Total annual catch (t) across all fleets.',
  SB_SBMSY  = 'Spawning biomass relative to SBMSY.',
  F_FMSY    = 'Fishing mortality relative to FMSY.'
)
TS_Target <- c(SB_SB0 = NA,  F_FTarget = 1,  Yield = NA, SB_SBMSY = 1,  F_FMSY = NA)
TS_Limit  <- c(SB_SB0 = LRP, F_FTarget = NA, Yield = NA, SB_SBMSY = NA, F_FMSY = 1)

# ---- Sequence through one `.mse` in memory at a time ----
PM_Value <- NULL
TS_Value <- NULL

for (i in seq_len(nOM)) {
  cli::cli_progress_step("[{i}/{nOM}] OM {OM_Design$OM_Name[i]}")

  MSE <- readRDS(OM_Design$MSEFile[i])
  Missing <- setdiff(MP_all, names(MSE@MPs))
  if (length(Missing))
    cli::cli_abort("OM {OM_Design$OM[i]} has no results for {.val {Missing}}; re-run 05-Run-Projections.R.")

  FTarget_FMSY <- Calc_FTarget_FMSY(readRDS(OM_Design$HistFile[i]), SPR = FTargetSPR)
  TS    <- Extract_MSE_Timeseries(MSE, FTarget_FMSY = FTarget_FMSY)
  Stats <- Calc_PM_Stats(MSE, Lim = LRP, StabilityThreshold = StabilityThreshold,
                         NoChangeThreshold = NoChangeThreshold,
                         CatchRefYears = CatchRefYears, TS = TS)
  Stats <- Stats[Stats$MP %in% MP_all, ]

  if (is.null(PM_Value)) {
    nsim        <- MSE@OM@nSim
    MPStartYear <- MSE@OM@MPStartYear
    Time        <- sort(unique(TS$Year))
    TimeNow     <- max(TS$Year[TS$Period == 'Historical'])

    PM_Value <- array(NA_real_, dim = c(nsim, nOM, nMP, nPI_pm),
                      dimnames = list(Sim = seq_len(nsim), OM = OM_Design$OM_Name,
                                      MP = MP_all, PI = PM_Codes))
    TS_Value <- array(NA_real_, dim = c(nsim, nOM, nMP, length(TS_Codes), length(Time)),
                      dimnames = list(Sim = seq_len(nsim), OM = OM_Design$OM_Name,
                                      MP = MP_all, PI = TS_Codes, Time = Time))
  }

  ## ---- PIs (per sim) -> Boxplot / Quilt / Spider / Tradeoff ----
  pi_idx <- match(Stats$PI, PM_Codes)
  if (anyNA(pi_idx))
    cli::cli_abort("`Calc_PM_Stats()` returned PI{?s} not in {.arg PM_Codes}: {.val {unique(Stats$PI[is.na(pi_idx)])}}.")
  PM_Value[cbind(Stats$Sim, i, match(Stats$MP, MP_all), pi_idx)] <- Stats$Value

  ## ---- Time series (per sim, per year) -> Timeseries / Kobe ----
  TS <- TS[TS$Year %in% Time, ]
  Proj <- TS[TS$Period == 'Projection' & TS$MP %in% MP_all, ]
  TS_Value[cbind(Proj$Sim, i, match(Proj$MP, MP_all),
                 match(Proj$Variable, TS_Codes), match(Proj$Year, Time))] <- Proj$Value

  Hist <- TS[TS$Period == 'Historical', ]
  for (mp in seq_len(nMP))
    TS_Value[cbind(Hist$Sim, i, mp, match(Hist$Variable, TS_Codes), match(Hist$Year, Time))] <- Hist$Value

  rm(MSE, TS, Stats, Proj, Hist)
  gc()
}

# ---- OMs ----
OMs_obj <- Slick::OMs()

Slick::Factors(OMs_obj) <- data.frame(
  Factor      = 'OM',
  Level       = OM_Design$OM,
  Description = OM_Design$Description
)

Slick::Design(OMs_obj) <- data.frame(OM = OM_Design$OM)
rownames(Slick::Design(OMs_obj)) <- OM_Design$OM_Name

Slick::Preset(OMs_obj) <- c(
  if ('00' %in% OM_Design$OM) list(`Base Case` = list(match('00', OM_Design$OM))),
  list(All = list(seq_len(nOM)))
)

# ---- MPs ----
MPs_obj <- Slick::MPs(Code = MP_Code, Label = MP_Label, Description = MP_Description)

Slick::Preset(MPs_obj) <- c(
  list(All = seq_len(nMP)),
  split(seq_len(nMP), MP_Type[MPInfo$MP]),
  split(seq_len(nMP), paste0(round(100 * MPInfo$TACChange), '% TAC change'))
)

# ---- Boxplot / Quilt ----
Boxplot_obj <- Slick::Boxplot(Code = PM_Codes, Label = unname(PM_Label[PM_Codes]),
                              Description = unname(PM_Description[PM_Codes]),
                              Value = PM_Value)

Prob01_Codes <- c('Safety', 'Safety_Short', 'Safety_Medium', 'Safety_Long', 'Status',
                  'Stability', 'NoChange', 'Yield_Hist', 'Kobe_Green')

Quilt_Codes <- setdiff(PM_Codes, c('AAVY', 'Depletion_Min'))
nPI_quilt   <- length(Quilt_Codes)

Quilt_MinValue <- rep(NA_real_, nPI_quilt)
Quilt_MaxValue <- rep(NA_real_, nPI_quilt)
Quilt_MinValue[match(Prob01_Codes, Quilt_Codes)] <- 0
Quilt_MaxValue[match(Prob01_Codes, Quilt_Codes)] <- 1

Quilt_obj <- Slick::Quilt(Code = Quilt_Codes, Label = unname(PM_Label[Quilt_Codes]),
                          Description = unname(PM_Description[Quilt_Codes]),
                          Value = PM_Value[, , , Quilt_Codes, drop = FALSE],
                          MinValue = Quilt_MinValue, MaxValue = Quilt_MaxValue)

non_windowed_PM <- c('Depletion_Min', 'AAVY', 'Stability')

WindowPreset <- function(Codes, Keep) {
  list(Summary    = match(intersect(Headline_Codes, Codes), Codes),
       All        = seq_along(Codes),
       ShortTerm  = match(c(grep('_Short$',  Codes, value = TRUE), intersect(Keep, Codes)), Codes),
       MediumTerm = match(c(grep('_Medium$', Codes, value = TRUE), intersect(Keep, Codes)), Codes),
       LongTerm   = match(c(grep('_Long$',   Codes, value = TRUE), intersect(Keep, Codes)), Codes))
}

Slick::Preset(Boxplot_obj) <- WindowPreset(PM_Codes, non_windowed_PM)
Slick::Preset(Quilt_obj)   <- WindowPreset(Quilt_Codes, non_windowed_PM)

# ---- Spider / Tradeoff ----
.Agg_PM <- function(codes) {
  arr <- array(NA_real_, dim = c(nOM, nMP, length(codes)),
               dimnames = list(OM = OM_Design$OM_Name, MP = MP_all, PI = codes))
  for (code in codes) {
    FUN <- if (code == 'Depletion_Min') min else mean
    arr[, , code] <- apply(PM_Value[, , , code, drop = FALSE], c(2, 3), FUN, na.rm = TRUE)
  }
  arr
}

Spider_PM_Codes <- setdiff(Spider_Codes, 'Rel_Yield')
Spider_Value <- array(NA_real_, dim = c(nOM, nMP, length(Spider_Codes)),
                      dimnames = list(OM = OM_Design$OM_Name, MP = MP_all, PI = Spider_Codes))
Spider_Value[, , Spider_PM_Codes] <- .Agg_PM(Spider_PM_Codes)

Yield_OM_MP <- apply(PM_Value[, , , 'Yield_All', drop = FALSE], c(2, 3), mean, na.rm = TRUE)
Spider_Value[, , 'Rel_Yield'] <- Yield_OM_MP / apply(Yield_OM_MP, 1, max, na.rm = TRUE)

Spider_obj <- Slick::Spider(Code = Spider_Codes, Label = unname(PM_Label[Spider_Codes]),
                            Description = unname(PM_Description[Spider_Codes]),
                            Value = Spider_Value)

Tradeoff_obj <- Slick::Tradeoff(Code = Tradeoff_Codes, Label = unname(PM_Label[Tradeoff_Codes]),
                                Description = unname(PM_Description[Tradeoff_Codes]),
                                Value = .Agg_PM(Tradeoff_Codes))

Slick::Preset(Tradeoff_obj) <- list(
  All         = seq_along(Tradeoff_Codes),
  CatchSafety = match(c('Yield_All', 'Safety'), Tradeoff_Codes),
  CatchStatus = match(c('Yield_All', 'Status'), Tradeoff_Codes)
)

# ---- Timeseries / Kobe ----
Timeseries_obj <- Slick::Timeseries(Code = TS_Codes, Label = unname(TS_Label[TS_Codes]),
                                    Description = unname(TS_Description[TS_Codes]),
                                    Value = TS_Value, Time = Time, TimeNow = TimeNow,
                                    Target = unname(TS_Target[TS_Codes]),
                                    Limit  = unname(TS_Limit[TS_Codes]))

KobeTime <- Time[Time >= MPStartYear]
Kobe_obj <- Slick::Kobe(Code = c('SB_SBMSY', 'F_FMSY'),
                        Label = unname(TS_Label[c('SB_SBMSY', 'F_FMSY')]),
                        Description = unname(TS_Description[c('SB_SBMSY', 'F_FMSY')]),
                        Value = TS_Value[, , , c('SB_SBMSY', 'F_FMSY'), as.character(KobeTime), drop = FALSE],
                        Time = KobeTime, Target = c(1, 1))

# ---- Assemble and save ----
Introduction <- "
Preliminary results of the management strategy evaluation (MSE) for North Pacific swordfish, developed for the Northern Committee of the Western and Central Pacific Fisheries Commission (WCPFC).

The operating models (OMs) are conditioned on the base case and sensitivity runs of the 2023 ISC stock assessment. The candidate management procedures (CMPs) are two index-based MPs (Index Rate and Index Target) and a model-based MP (Surplus Production), with a harvest control rule that reduces fishing between a threshold reference point (ThRP = 25%, 30%, or 35% SB0, selected for each MP) and the limit reference point (LRP = 17% SB0, where SB0 is the equilibrium unfished spawning biomass). The variants of each MP limit the change in the TAC between management cycles to 10%, 20%, or 40%; the 10% limit gave the highest catch for every MP, and the 20% variant is shown to illustrate that a looser limit does not increase catch. Each CMP was tuned in the base case OM to maximise mean catch subject to the Safety (probability of at least 0.8) and Status (probability of at least 0.5) objectives.

The performance indicators of the candidate management objectives are: Safety, the probability that spawning biomass remains above the LRP in every year of the projection period; Status, the probability that F is at or below the F target (F40%); Stability, the probability that the TAC changes by no more than 10% between management cycles; the probability of no management change (the TAC changes by no more than 1% between management cycles); and Yield, the probability that the annual catch is at least the mean 2008-2010 catch.

**These results are preliminary and do not necessarily reflect the views of the WCPFC or its members.**
"

slick <- Slick::Slick(Title = 'North Pacific Swordfish',
                      Author = 'Adrian Hordyk',
                      Email = 'adrian@bluematterscience.com',
                      Introduction = Introduction,
                      MPs = MPs_obj,
                      OMs = OMs_obj,
                      Boxplot = Boxplot_obj,
                      Kobe = Kobe_obj,
                      Quilt = Quilt_obj,
                      Spider = Spider_obj,
                      Tradeoff = Tradeoff_obj,
                      Timeseries = Timeseries_obj)

print(Slick::Check(slick))

MSEtool::Save(slick, 'objects/slick.rds', overwrite = TRUE)
# MSEtool::Save(slick, '../SlickLibrary/Slick_Objects/North_Pacific_Swordfish.slick', overwrite = TRUE)

# ---- Run App ----
if (interactive())
  Slick::App(slick = slick)

# ---- Make manual plots ----
# All OMs combined, and each OM (OM00 is the base case)
OM_Sets <- c(list(All = seq_len(nOM)),
             stats::setNames(as.list(seq_len(nOM)), paste0('OM', OM_Design$OM)))

## ---- Timeseries ----
for (om_set in names(OM_Sets)) {
  om_idx <- OM_Sets[[om_set]]
  for (code in TS_Codes) {
    p <- Slick::plotTimeseries(slick, PI = match(code, TS_Codes), OMs = om_idx,
                               byMP = TRUE, includeHist = FALSE)
    ggplot2::ggsave(file.path(FigDir, 'Timeseries', om_set, paste0(code, '.png')),
                    p, width = 10, height = 6, dpi = 300, create.dir = TRUE)
  }
}

## ---- Kobe and Kobe-time ----
for (om_set in names(OM_Sets)) {
  om_idx <- OM_Sets[[om_set]]

  p_kobe <- Slick::plotKobe(slick, xPI = 1, yPI = 2, Time = FALSE, OMs = om_idx, percentile = NULL)
  ggplot2::ggsave(file.path(FigDir, 'Kobe', paste0(om_set, '.png')),
                  p_kobe, width = 8, height = 7, dpi = 300, create.dir = TRUE)

  p_kobe_time <- Slick::plotKobe(slick, xPI = 1, yPI = 2, Time = TRUE, OMs = om_idx)
  ggplot2::ggsave(file.path(FigDir, 'Kobe', paste0(om_set, '_Time.png')),
                  p_kobe_time, width = 10, height = 5, dpi = 300)
}

## ---- Quilt tables ----
Quilt_Primary_Codes  <- intersect(Headline_Codes, Quilt_Codes)
Quilt_Extended_Codes <- c(Quilt_Primary_Codes,
                          'Yield_Short', 'Yield_Medium', 'Yield_Long',
                          'Safety_Short', 'Safety_Medium', 'Safety_Long',
                          'Depletion_All', 'Depletion_Long')

Save_Quilt_Table <- function(pi_codes, om_idx, file_stub) {
  slick_sub <- Slick::FilterSlick(slick, OMs = om_idx, PIs = match(pi_codes, Quilt_Codes), plot = 'Quilt')
  tbl       <- Slick::plotQuilt(slick_sub, kable = TRUE)

  prob_cols <- intersect(Prob01_Codes, pi_codes)
  if (length(prob_cols))
    tbl <- flextable::colformat_double(tbl, j = prob_cols, digits = 2)

  dir <- file.path(FigDir, 'Quilt')
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  htmltools::save_html(flextable::htmltools_value(tbl), file = file.path(dir, paste0(file_stub, '.html')))
  flextable::save_as_docx(tbl, path = file.path(dir, paste0(file_stub, '.docx')))
}

for (om_set in names(OM_Sets)) {
  Save_Quilt_Table(Quilt_Primary_Codes,  OM_Sets[[om_set]], paste0(om_set, '_Primary'))
  Save_Quilt_Table(Quilt_Extended_Codes, OM_Sets[[om_set]], paste0(om_set, '_Extended'))
}

## ---- Comparison across OMs ----
OM_Colours <- scales::hue_pal()(nOM)
OM_Colours[OM_Design$OM == '00'] <- 'black'
names(OM_Colours) <- OM_Design$OM_Name

# Median time series in each OM, one panel per MP
TS_Median <- apply(TS_Value, c(2, 3, 4, 5), stats::median, na.rm = TRUE) |>
  as.data.frame.table(responseName = 'Value', stringsAsFactors = FALSE)
TS_Median$Time <- as.numeric(TS_Median$Time)
TS_Median$MP   <- factor(TS_Median$MP, levels = MP_all)
TS_Median$OM   <- factor(TS_Median$OM, levels = OM_Design$OM_Name)

for (code in TS_Codes) {
  p <- ggplot2::ggplot(TS_Median[TS_Median$PI == code, ], ggplot2::aes(Time, Value, colour = OM)) +
    ggplot2::geom_vline(xintercept = MPStartYear, linetype = 3, colour = 'grey40') +
    ggplot2::geom_line(linewidth = 0.6) +
    ggplot2::facet_wrap(~MP, ncol = 3, scales = 'free_y') +
    ggplot2::scale_colour_manual(values = OM_Colours) +
    ggplot2::expand_limits(y = 0) +
    ggplot2::labs(x = NULL, y = TS_Label[[code]], colour = NULL,
                  caption = 'Median over simulations. Dotted line: first year of MP advice.') +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = 'bottom')
  if (!is.na(TS_Target[[code]]))
    p <- p + ggplot2::geom_hline(yintercept = TS_Target[[code]], linetype = 2, colour = 'darkgreen')
  if (!is.na(TS_Limit[[code]]))
    p <- p + ggplot2::geom_hline(yintercept = TS_Limit[[code]], linetype = 2, colour = 'red')
  ggplot2::ggsave(file.path(FigDir, 'Timeseries', 'ByOM', paste0(code, '.png')),
                  p, width = 12, height = 8, dpi = 300, create.dir = TRUE)
}

# Performance indicators of the candidate management objectives in each OM
Summary_Codes <- c(Table1_Codes, 'Yield_All')
PI_OM <- apply(PM_Value, c(2, 3, 4), mean, na.rm = TRUE) |>
  as.data.frame.table(responseName = 'Value', stringsAsFactors = FALSE)

d <- PI_OM[PI_OM$PI %in% Summary_Codes, ]
d$PI <- factor(PM_Label[d$PI], levels = PM_Label[Summary_Codes])
d$MP <- factor(d$MP, levels = MP_all)
d$OM <- factor(d$OM, levels = OM_Design$OM_Name)
Min  <- data.frame(PI = factor(PM_Label[names(PI_Min)], levels = levels(d$PI)), Value = PI_Min)

p <- ggplot2::ggplot(d, ggplot2::aes(MP, Value, colour = OM)) +
  ggplot2::geom_hline(data = Min, ggplot2::aes(yintercept = Value), linetype = 2, colour = 'red') +
  ggplot2::geom_point(position = ggplot2::position_dodge(width = 0.7), size = 2) +
  ggplot2::facet_wrap(~PI, ncol = 2, scales = 'free_y', labeller = ggplot2::label_wrap_gen(45)) +
  ggplot2::scale_colour_manual(values = OM_Colours) +
  ggplot2::labs(x = NULL, y = NULL, colour = NULL,
                caption = 'Mean over simulations. Dashed lines: minimum probabilities of the Safety and Status objectives.') +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = 'bottom', axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
ggplot2::ggsave(file.path(FigDir, 'Summary', 'PI_by_OM.png'), p, width = 11, height = 12, dpi = 300,
                create.dir = TRUE)

# Table: one row per PI and CMP, one column per OM, and the mean and minimum over OMs
Tab <- stats::reshape(PI_OM, idvar = c('PI', 'MP'), timevar = 'OM', direction = 'wide')
names(Tab) <- sub('^Value\\.', '', names(Tab))
Tab <- Tab[order(match(Tab$PI, PM_Codes), match(Tab$MP, MP_all)), ]
Tab$Mean <- rowMeans(Tab[OM_Design$OM_Name])
Tab$Min  <- apply(Tab[OM_Design$OM_Name], 1, min)
utils::write.csv(Tab, file.path(FigDir, 'Summary', 'PI_by_OM.csv'), row.names = FALSE)

