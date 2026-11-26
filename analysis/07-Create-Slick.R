# Build the Slick object for results visualisation
library(wcpfcNSWO)

MSEDir  <- 'objects/MSE'
TuneDir <- 'objects/Tune'
FigDir  <- 'analysis/figures/Slick'

# Reference levels of the candidate management objectives
source('analysis/00a-PM-Specifications.R')

TunedCMPs       <- readRDS(file.path(TuneDir, 'TunedCMPs.rds'))

# CMPs in the Slick object (NULL for all CMPs tuned by 05d-Tune-CMPs.R): the
# 36 CMPs with all the indices (JPN + US + TWN; CMPIndexSets in 05b-Define-CMPs.R)
SelectedIndexSet <- 'All'
SelectedCMPs     <- grep(paste0('_', SelectedIndexSet, '_'), names(TunedCMPs), value = TRUE)
TuneSummary      <- utils::read.csv(file.path(TuneDir, 'Summary.csv'))

# CMPs selected by default in Slick: the HCRs with the highest yield
# (HCR 5, F35%; HCR 6, F30%) of each MP type, with the 10% TAC change limit
DefaultHCRs      <- c(5, 6)
DefaultTACChange <- 0.1

# ---- MP metadata ----
MP_all <- names(TunedCMPs)
if (!is.null(SelectedCMPs)) {
  if (length(setdiff(SelectedCMPs, MP_all)))
    cli::cli_abort("Not tuned CMPs: {.val {setdiff(SelectedCMPs, MP_all)}}.")
  MP_all <- SelectedCMPs
}
# ordered by MP type, HCR, and TAC change limit
MPInfo <- TuneSummary[match(MP_all, TuneSummary$Name), ]
MP_all <- MP_all[order(match(MPInfo$MP, c('IR', 'IT', 'SP')), MPInfo$HCR, MPInfo$TACChange)]
nMP    <- length(MP_all)
MPInfo <- TuneSummary[match(MP_all, TuneSummary$Name), ]
MPInfo$FTarget <- paste0('F', round(100 * MPInfo$FTargetSPR))
MPInfo$Group   <- paste(MPInfo$MP, MPInfo$IndexSet, sep = '_')
MPFTarget      <- stats::setNames(MPInfo$FTarget, MP_all)
MPThRP         <- stats::setNames(MPInfo$ThRP, MP_all)

# CMPs are saved in groups (MP and index set) by 06-Run-Projections.R
Groups <- split(MP_all, factor(MPInfo$Group, unique(MPInfo$Group)))

MP_Type <- c(IR = 'Index Rate', IT = 'Index Target', SP = 'Surplus Production')
IndexSetLabel <- c(JPN = 'JPN', JPNUS = 'JPN + US', JPNTWN = 'JPN + TWN', All = 'JPN + US + TWN')

# Technical Supporting Document (TSD)
TSD_URL     <- 'https://wcpfcnswo.bluematterscience.com/tsd'
TSD_CMP_URL <- paste0(TSD_URL, '/management-procedures.html')
TSD_PM_URL  <- paste0(TSD_URL, '/performance-metrics.html')
# link that opens in a new tab (only rendered in the Introduction; the app
# shows the MP and PI descriptions as plain text)
TSDLink <- function(Text, URL) sprintf('<a href="%s" target="_blank">%s</a>', URL, Text)

MP_TypeDescription <- c(
  IR = 'Index Rate: TAC = tuned rate x current index',
  IT = 'Index Target: TAC = tuned reference catch',
  SP = 'Surplus Production: F = tuned multiple of the estimated FMSY'
)

# (the index set is in the code and label only if the CMPs have more than one)
OneIndexSet <- length(unique(MPInfo$IndexSet)) == 1
MP_Code  <- if (OneIndexSet) sub(paste0('_', MPInfo$IndexSet[1], '_'), '_', MP_all, fixed = TRUE) else MP_all
MP_Label <- if (OneIndexSet) {
  sprintf('%s HCR%d %d%%', MPInfo$MP, MPInfo$HCR, round(100 * MPInfo$TACChange))
} else {
  sprintf('%s HCR%d %s %d%%', MPInfo$MP, MPInfo$HCR, IndexSetLabel[MPInfo$IndexSet], round(100 * MPInfo$TACChange))
}

# The first CMP of each MP type (and index set) is described in full; the
# others only by how they differ from it
HCRText <- sprintf('HCR %d (F target %s%%, ThRP %d%% SB0)', MPInfo$HCR, MPInfo$FTarget, round(100 * MPInfo$ThRP))
CapText <- sprintf('%d%% TAC change limit', round(100 * MPInfo$TACChange))
First   <- match(MPInfo$Group, MPInfo$Group)
MP_Description <- vapply(seq_len(nMP), \(i) {
  f <- First[i]
  if (i == f)
    return(sprintf(
      '%s. %s; Fmin (F90%%) at the LRP (%d%% SB0); %s. Indices: %s CPUE. Tuned (base case OM) to maximise mean catch with P(SB > LRP) >= %.2g and P(F <= F target) >= %.2g. HCRs: %s#sec-hcrs',
      MP_TypeDescription[[MPInfo$MP[i]]], HCRText[i], round(100 * LRP), CapText[i],
      IndexSetLabel[[MPInfo$IndexSet[i]]], PI_Min[['Safety']], PI_Min[['Status_F']], TSD_CMP_URL))
  Diff <- c(if (MPInfo$HCR[i] != MPInfo$HCR[f]) HCRText[i],
            if (MPInfo$TACChange[i] != MPInfo$TACChange[f]) paste('a', CapText[i]))
  sprintf('As %s, with %s.', MP_Code[f], paste(Diff, collapse = ' and '))
}, character(1))

# ---- OMs with MSE results ----
OM_Design <- ListSS3Runs() |>
  dplyr::filter(Include) |>
  dplyr::mutate(HistFile = file.path('objects/Hist', paste0(OM, '.hist'))) |>
  dplyr::filter(dir.exists(file.path(MSEDir, OM)))
if (!nrow(OM_Design))
  cli::cli_abort("No MSE results in {.file {MSEDir}}; run 06-Run-Projections.R.")
OM_Design$OM_Name <- paste0(OM_Design$OM, ': ', OM_Design$Description)
nOM <- nrow(OM_Design)

# ---- Performance indicators ----
# Candidate management objectives (Table 1): Safety, Status, Stability,
# probability of no management change, and Yield
StatusNames   <- names(StatusFTargetSPR)
StatusSBCodes <- paste0('Status_', names(StatusThRP))
Table1_Codes  <- c('Safety', 'Status_F', StatusSBCodes, 'FTarget_F', 'Stability', 'NoChange', names(CatchRefYears))

# (the Status_* metrics together)
PM_Codes <- c('Safety',
              'Status_F',
              paste0('Status_', StatusNames),
              StatusSBCodes,
              'Status_SB',
              'FTarget_F',
              paste0('FTarget_F_', StatusNames),
              'Stability',
              'NoChange',
              names(CatchRefYears),

              'Yield_All',
              'Yield_Short',
              'Yield_Medium',
              'Yield_Long',

              'Safety_Short',
              'Safety_Medium',
              'Safety_Long',
              'Safety_NoBreach',

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

YearsLabel <- vapply(CatchRefYears, \(yrs) paste(range(yrs), collapse = '-'), character(1))

PM_Label <- c(
  Safety           = 'Probability SB > LRP',
  Status_F         = 'Probability F <= F target (of the HCR)',
  FTarget_F        = 'F target (of the HCR) / F',
  stats::setNames(paste0('Probability SB > ', round(100 * StatusThRP), '% SB0'), StatusSBCodes),
  Status_SB        = 'Probability SB > ThRP (of the HCR)',
  Stability        = paste0('Catch stability (TAC change <= ', 100 * StabilityThreshold, '%)'),
  NoChange         = 'Probability of no management change',
  stats::setNames(paste0('Probability catch >= ', YearsLabel, ' catch'), names(CatchRefYears)),
  stats::setNames(paste0('Probability F <= ', StatusNames, '%'), paste0('Status_', StatusNames)),
  stats::setNames(paste0(StatusNames, '% / F'), paste0('FTarget_F_', StatusNames)),

  Yield_All        = 'Mean catch (all years)',
  Yield_Short      = 'Mean catch (years 1-10)',
  Yield_Medium     = 'Mean catch (years 11-20)',
  Yield_Long       = 'Mean catch (years 21-30)',

  Safety_Short     = 'Probability SB > LRP (years 1-10)',
  Safety_Medium    = 'Probability SB > LRP (years 11-20)',
  Safety_Long      = 'Probability SB > LRP (years 21-30)',
  Safety_NoBreach  = 'Probability SB > LRP in every year',

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

Safety_Description <- paste0('Probability that spawning biomass is above the limit reference point (LRP; ', 100 * LRP,
                             '% SB0) in any given year')

PM_Description <- c(
  Safety           = paste0(Safety_Description, ' of the projection period (proportion of simulations and years).'),
  Status_F         ='Probability that fishing mortality is at or below the F target of the HCR of the CMP (F40%, F35%, or F30%), over all projection years.',
  FTarget_F        = 'Median ratio of the F target of the HCR of the CMP to fishing mortality over the projection years; values above 1 mean F is below the target.',
  stats::setNames(paste0('Probability that spawning biomass is above ', round(100 * StatusThRP),
                         '% SB0 (candidate threshold reference point) in any given year of the projection period.'),
                  StatusSBCodes),
  Status_SB        = 'Probability that spawning biomass is above the threshold reference point of the HCR of the CMP (ThRP; 35%, 30%, or 25% SB0) in any given year of the projection period.',
  Stability        = paste0('Probability that the TAC changes by no more than ', 100 * StabilityThreshold, '% between consecutive management cycles.'),
  NoChange         = paste0('Probability of no management change: the probability that the TAC changes by no more than ', 100 * NoChangeThreshold, '% between consecutive management cycles.'),
  stats::setNames(paste0('Probability that the annual catch is at least the mean historical catch in ', YearsLabel,
                         ', over all projection years.'), names(CatchRefYears)),
  stats::setNames(paste0('Probability that fishing mortality is at or below ', StatusNames,
                         '% (candidate target reference point), over all projection years.'),
                  paste0('Status_', StatusNames)),
  stats::setNames(paste0('Median ratio of ', StatusNames,
                         '% to fishing mortality over the projection years; values above 1 mean F is below ', StatusNames, '%.'),
                  paste0('FTarget_F_', StatusNames)),

  Yield_All        = 'Mean annual catch (t) over all projection years.',
  Yield_Short      = 'Mean annual catch (t) over the first 10 projection years (short-term).',
  Yield_Medium     = 'Mean annual catch (t) over the middle 10 projection years (medium-term).',
  Yield_Long       = 'Mean annual catch (t) over the final 10 projection years (long-term).',

  Safety_Short     = paste0(Safety_Description, ' of the first 10 projection years (short-term).'),
  Safety_Medium    = paste0(Safety_Description, ' of the middle 10 projection years (medium-term).'),
  Safety_Long      = paste0(Safety_Description, ' of the final 10 projection years (long-term).'),
  Safety_NoBreach  = paste0('Probability that spawning biomass remains above the LRP (', 100 * LRP,
                            '% SB0) in every year of the projection period: a simulation that falls below the LRP in any year is a breach.'),

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

Spider_Codes   <- c('Safety', 'Status_F', 'Stability', 'NoChange', names(CatchRefYears), 'Rel_Yield')
Headline_Codes <- c(Table1_Codes, 'Yield_All')
Tradeoff_Codes <- c('Safety', 'Yield_All', 'Status_F', 'Stability', 'NoChange', names(CatchRefYears), 'Depletion_All')

TS_FCodes <- paste0('F_', StatusNames)
TS_Codes  <- c('SB_SB0', TS_FCodes, 'Yield', 'SB_SBMSY', 'F_FMSY')
TS_Label  <- c(SB_SB0 = 'SB/SB0', stats::setNames(paste0('F/', StatusNames, '%'), TS_FCodes), Yield = 'Yield',
               SB_SBMSY = 'SB/SBMSY', F_FMSY = 'F/FMSY')
TS_Description <- c(
  SB_SB0   = 'Spawning biomass relative to the equilibrium unfished spawning biomass.',
  stats::setNames(paste0('Fishing mortality relative to ', StatusNames, '%.'), TS_FCodes),
  Yield     = 'Total annual catch (t) across all fleets.',
  SB_SBMSY  = 'Spawning biomass relative to SBMSY.',
  F_FMSY    = 'Fishing mortality relative to FMSY.'
)
TS_Target <- c(SB_SB0 = NA,  stats::setNames(rep(1, length(TS_FCodes)), TS_FCodes),  Yield = NA, SB_SBMSY = 1,  F_FMSY = NA)
TS_Limit  <- c(SB_SB0 = LRP, stats::setNames(rep(NA, length(TS_FCodes)), TS_FCodes), Yield = NA, SB_SBMSY = NA, F_FMSY = 1)

# ---- Sequence through one `.mse` in memory at a time ----
PM_Value <- NULL
TS_Value <- NULL

for (i in seq_len(nOM)) {
  cli::cli_progress_step("[{i}/{nOM}] OM {OM_Design$OM_Name[i]}")
  FTarget_FMSY <- PM_FTarget_FMSY(readRDS(OM_Design$HistFile[i]))

  for (g in names(Groups)) {
    File <- file.path(MSEDir, OM_Design$OM[i], paste0(g, '.mse'))
    if (!file.exists(File))
      cli::cli_abort("OM {OM_Design$OM[i]} has no results for {.val {g}}; re-run 06-Run-Projections.R.")
    # (adds any slots that are new in the installed MSEtool)
    MSE <- MSEtool::UpdateObject(readRDS(File))
    Missing <- setdiff(Groups[[g]], names(MSE@MPs))
    if (length(Missing))
      cli::cli_abort("OM {OM_Design$OM[i]} has no results for {.val {Missing}}; re-run 06-Run-Projections.R.")

    TS    <- Extract_MSE_Timeseries(MSE, FTarget_FMSY = FTarget_FMSY)
    Stats <- Calc_PM_Stats(MSE, Lim = LRP, StabilityThreshold = StabilityThreshold,
                           NoChangeThreshold = NoChangeThreshold, MPFTarget = MPFTarget,
                           ThRP = StatusThRP, MPThRP = MPThRP, CatchRefYears = CatchRefYears, TS = TS)
    Stats <- Stats[Stats$MP %in% Groups[[g]], ]

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
    TS <- TS[TS$Year %in% Time & TS$Variable %in% TS_Codes, ]
    Proj <- TS[TS$Period == 'Projection' & TS$MP %in% Groups[[g]], ]
    TS_Value[cbind(Proj$Sim, i, match(Proj$MP, MP_all),
                   match(Proj$Variable, TS_Codes), match(Proj$Year, Time))] <- Proj$Value

    Hist <- TS[TS$Period == 'Historical', ]
    for (mp in match(Groups[[g]], MP_all))
      TS_Value[cbind(Hist$Sim, i, mp, match(Hist$Variable, TS_Codes), match(Hist$Year, Time))] <- Hist$Value

    rm(MSE, TS, Stats, Proj, Hist)
    gc()
  }
}
cli::cli_progress_done()

# ---- OMs ----
OMs_obj <- Slick::OMs()

Slick::Factors(OMs_obj) <- data.frame(
  Factor      = 'OM',
  Level       = OM_Design$OM,
  Description = OM_Design$Description
)

Slick::Design(OMs_obj) <- data.frame(OM = OM_Design$OM)
rownames(Slick::Design(OMs_obj)) <- OM_Design$OM_Name

# Preset buttons (rows of Design): the base case (the default, the first
# preset), each group of sensitivity OMs with the base case, and all OMs. The
# app shows at most 8 OM presets
OM_Groups <- c('01'  = 'Natural mortality',
               '02'  = 'Natural mortality',
               '03'  = 'Steepness',
               '04'  = 'Steepness',
               '07'  = 'Growth',
               '08'  = 'Maturity',
               '09'  = 'Maturity',
               '10'  = 'Maturity',
               '20a' = 'Effort creep',
               '20b' = 'Effort creep')
Missing <- setdiff(OM_Design$OM, c('00', names(OM_Groups)))
if (length(Missing))
  cli::cli_abort("Add OM{?s} {.val {Missing}} to {.var OM_Groups}.")
Base <- match('00', OM_Design$OM)
GroupRows <- lapply(split(names(OM_Groups), factor(OM_Groups, unique(OM_Groups))),
                    \(oms) c(Base, which(OM_Design$OM %in% oms)))
GroupRows <- GroupRows[lengths(GroupRows) > 1]
Slick::Preset(OMs_obj) <- c(list(`Base Case` = Base), GroupRows, list(All = seq_len(nOM)))
if (length(Slick::Preset(OMs_obj)) > 8)
  cli::cli_abort("The Slick app shows at most 8 OM presets.")

# ---- MPs ----
MPs_obj <- Slick::MPs(Code = MP_Code, Label = MP_Label, Description = MP_Description)

# Preset buttons: the default selection (the first preset), all CMPs, each MP
# family, each TAC change limit, and each F target (HCRs 1-3, 4-5, 6). The app
# shows at most 10 MP presets
DefaultMPs <- which(MPInfo$HCR %in% DefaultHCRs & MPInfo$TACChange == DefaultTACChange)
Slick::Preset(MPs_obj) <- c(
  list(Default = DefaultMPs,
       All = seq_len(nMP)),
  split(seq_len(nMP), factor(MP_Type[MPInfo$MP], MP_Type)),
  split(seq_len(nMP), paste0(round(100 * MPInfo$TACChange), '% TAC cap')),
  split(seq_len(nMP), factor(paste0(MPInfo$FTarget, '%'), unique(paste0(MPInfo$FTarget, '%')[order(MPInfo$HCR)])))
)
if (length(unique(MPInfo$IndexSet)) > 1)
  Slick::Preset(MPs_obj) <- c(Slick::Preset(MPs_obj),
                              split(seq_len(nMP), paste('Indices:', IndexSetLabel[MPInfo$IndexSet])))
if (length(Slick::Preset(MPs_obj)) > 10)
  cli::cli_abort("The Slick app shows at most 10 MP presets.")

# ---- Boxplot / Quilt ----
Boxplot_obj <- Slick::Boxplot(Code = PM_Codes, Label = unname(PM_Label[PM_Codes]),
                              Description = unname(PM_Description[PM_Codes]),
                              Value = PM_Value)

Prob01_Codes <- c('Safety', 'Safety_Short', 'Safety_Medium', 'Safety_Long', 'Safety_NoBreach', 'Status_F', StatusSBCodes, 'Status_SB',
                  paste0('Status_', StatusNames), 'Stability', 'NoChange', names(CatchRefYears),
                  'Kobe_Green')

Quilt_Codes <- setdiff(PM_Codes, c('AAVY', 'Depletion_Min'))
nPI_quilt   <- length(Quilt_Codes)

Quilt_MinValue <- rep(NA_real_, nPI_quilt)
Quilt_MaxValue <- rep(NA_real_, nPI_quilt)
Quilt_MinValue[match(Prob01_Codes, Quilt_Codes)] <- 0
Quilt_MaxValue[match(Prob01_Codes, Quilt_Codes)] <- 1

# Values shown in the app rounded to 2 decimal places, mean catch to whole
# tonnes. The Quilt gets the mean over simulations in each OM, rounded (the app
# rounds the mean over the selected OMs to 3 significant figures)
Yield_Codes <- paste0('Yield_', c('All', 'Short', 'Medium', 'Long'))
# (PIs in the last dimension of `Arr`)
RoundPI <- function(Arr) {
  d      <- dim(Arr)
  Codes  <- dimnames(Arr)[[length(d)]]
  Arr[]  <- round(Arr, rep(ifelse(Codes %in% Yield_Codes, 0, 2), each = prod(d[-length(d)])))
  Arr
}

Quilt_obj <- Slick::Quilt(Code = Quilt_Codes, Label = unname(PM_Label[Quilt_Codes]),
                          Description = unname(PM_Description[Quilt_Codes]),
                          Value = RoundPI(apply(PM_Value[, , , Quilt_Codes, drop = FALSE], c(2, 3, 4), mean, na.rm = TRUE)),
                          MinValue = Quilt_MinValue, MaxValue = Quilt_MaxValue)

non_windowed_PM <- c('Depletion_Min', 'AAVY', 'Stability', 'Safety_NoBreach')

WindowPreset <- function(Codes, Keep) {
  list(Summary    = match(intersect(Headline_Codes, Codes), Codes),
       Status     = match(intersect(c('Status_F', paste0('Status_', StatusNames), StatusSBCodes, 'Status_SB', 'FTarget_F',
                                      paste0('FTarget_F_', StatusNames)), Codes), Codes),
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
                            Value = RoundPI(Spider_Value))

Tradeoff_obj <- Slick::Tradeoff(Code = Tradeoff_Codes, Label = unname(PM_Label[Tradeoff_Codes]),
                                Description = unname(PM_Description[Tradeoff_Codes]),
                                Value = RoundPI(.Agg_PM(Tradeoff_Codes)))

Slick::Preset(Tradeoff_obj) <- list(
  All         = seq_along(Tradeoff_Codes),
  CatchSafety = match(c('Yield_All', 'Safety'), Tradeoff_Codes),
  CatchStatus = match(c('Yield_All', 'Status_F'), Tradeoff_Codes)
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
Introduction <- paste0("
Preliminary results of the management strategy evaluation (MSE) for North Pacific swordfish, developed for the Northern Committee of the Western and Central Pacific Fisheries Commission (WCPFC).

The operating models (OMs) are conditioned on the base case and sensitivity runs of the 2023 ISC stock assessment. The candidate management procedures (CMPs) are two index-based MPs (Index Rate and Index Target) and a model-based MP (Surplus Production), each with the six candidate harvest control rules (HCRs) of the Northern Committee (NC22 Summary Report, Attachment 7) and a ", round(100 * min(MPInfo$TACChange)), "% or ", round(100 * max(MPInfo$TACChange)), "% limit on TAC changes. CMP codes are MP type, HCR, and TAC change limit (e.g. ", MP_Code[1], ").

By default, the base case OM and the ", length(DefaultMPs), " CMPs with the highest-yield HCRs (", paste0('HCR ', DefaultHCRs, collapse = ' and '), ") and the ", round(100 * DefaultTACChange), "% TAC change limit are selected. Preset buttons select the CMPs of each MP type, TAC change limit (cap), and F target, and the OMs of each group of sensitivities (with the base case).

The Technical Supporting Document (", TSDLink('TSD', TSD_URL), ") describes the ", TSDLink('CMPs', TSD_CMP_URL), ", including the ", TSDLink('shapes of the HCRs', paste0(TSD_CMP_URL, '#sec-hcrs')), ", how they are ", TSDLink('implemented in each CMP type', paste0(TSD_CMP_URL, '#tbl-hcr-translation')), ", and the ", TSDLink('tuning', paste0(TSD_CMP_URL, '#sec-tuning-results')), ", and defines the ", TSDLink('performance metrics', TSD_PM_URL), ".

See the ", TSDLink('North Pacific Swordfish MSE homepage', 'https://wcpfcnswo.bluematterscience.com/'), " for more information on this analysis.

**These results are preliminary and do not necessarily reflect the views of the WCPFC or its members.**
")

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

Slick::Check(slick)

MSEtool::Save(slick, 'objects/slick.rds', overwrite = TRUE)
# MSEtool::Save(slick, '../SlickLibrary/Slick_Objects/North_Pacific_Swordfish.slick', overwrite = TRUE)

# ---- Run App ----
slick <- readRDS('objects/slick.rds')
if (interactive())
  Slick::App(slick = slick)

# ---- Make manual plots ----
# By CMP group (MP and index set), for all OMs combined and each OM individually

OMs_list <- as.list(seq_along(OM_Design$OM))
names(OMs_list) <- paste0('OM', OM_Design$OM)

OM_Sets <- c(list(All = seq_len(nOM)), OMs_list)

## ---- Timeseries ----
for (g in names(Groups)) {
  mp_idx <- match(Groups[[g]], MP_all)
  for (om_set in names(OM_Sets)) {
    slick_sub <- Slick::FilterSlick(slick, MPs = mp_idx, OMs = OM_Sets[[om_set]], plot = 'Timeseries')
    for (code in TS_Codes) {
      p <- Slick::plotTimeseries(slick_sub, PI = match(code, TS_Codes), byMP = TRUE, includeHist = FALSE)
      ggplot2::ggsave(file.path(FigDir, 'Timeseries', om_set, g, paste0(code, '.png')),
                      p, width = 12, height = 8, dpi = 300, create.dir = TRUE)
    }
  }
}

## ---- Kobe and Kobe-time ----
for (g in names(Groups)) {
  mp_idx <- match(Groups[[g]], MP_all)
  for (om_set in names(OM_Sets)) {
    slick_sub <- Slick::FilterSlick(slick, MPs = mp_idx, OMs = OM_Sets[[om_set]], plot = 'Kobe')

    p_kobe <- Slick::plotKobe(slick_sub, xPI = 1, yPI = 2, Time = FALSE, percentile = NULL)
    ggplot2::ggsave(file.path(FigDir, 'Kobe', om_set, paste0(g, '.png')),
                    p_kobe, width = 8, height = 7, dpi = 300, create.dir = TRUE)

    p_kobe_time <- Slick::plotKobe(slick_sub, xPI = 1, yPI = 2, Time = TRUE)
    ggplot2::ggsave(file.path(FigDir, 'Kobe', om_set, paste0(g, '_Time.png')),
                    p_kobe_time, width = 12, height = 8, dpi = 300, create.dir = TRUE)
  }
}

## ---- Quilt tables ----
# All CMPs, for all OMs combined and each OM
Quilt_OM_Sets <- c(list(All = seq_len(nOM)),
                   stats::setNames(as.list(seq_len(nOM)), paste0('OM', OM_Design$OM)))

Quilt_Primary_Codes  <- intersect(Headline_Codes, Quilt_Codes)
Quilt_Extended_Codes <- c(Quilt_Primary_Codes,
                          paste0('Status_', StatusNames),
                          'Yield_Short', 'Yield_Medium', 'Yield_Long',
                          'Safety_Short', 'Safety_Medium', 'Safety_Long', 'Safety_NoBreach',
                          'Depletion_All', 'Depletion_Long')

# (from the unrounded means in each OM, rounded after averaging over the OMs:
# 2 decimal places, mean catch to whole tonnes)
# slick_quilt <- slick
# Slick::Value(slick_quilt@Quilt) <- apply(PM_Value[, , , Quilt_Codes, drop = FALSE], c(2, 3, 4), mean, na.rm = TRUE)

Save_Quilt_Table <- function(pi_codes, om_idx, file_stub) {
  slick_sub <- Slick::FilterSlick(slick_quilt, OMs = om_idx, PIs = match(pi_codes, Quilt_Codes), plot = 'Quilt')
  tbl       <- Slick::plotQuilt(slick_sub, kable = TRUE, signif = 10)

  yield_cols <- intersect(Yield_Codes, pi_codes)
  tbl <- flextable::colformat_double(tbl, j = setdiff(pi_codes, yield_cols), digits = 2)
  if (length(yield_cols))
    tbl <- flextable::colformat_double(tbl, j = yield_cols, digits = 0)

  dir <- file.path(FigDir, 'Quilt')
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  htmltools::save_html(flextable::htmltools_value(tbl), file = file.path(dir, paste0(file_stub, '.html')))
  flextable::save_as_docx(tbl, path = file.path(dir, paste0(file_stub, '.docx')))
}

for (om_set in names(Quilt_OM_Sets)) {
  Save_Quilt_Table(Quilt_Primary_Codes,  Quilt_OM_Sets[[om_set]], paste0(om_set, '_Primary'))
  Save_Quilt_Table(Quilt_Extended_Codes, Quilt_OM_Sets[[om_set]], paste0(om_set, '_Extended'))
}
