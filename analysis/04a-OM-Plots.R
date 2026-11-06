# Diagnostic plots for the operating models
# Chunks are also run in the TSD Operating Models chapter (tsd/operating-models.qmd)

# ---- load_files ----
library(MSEtool)
library(wcpfcNSWO)
library(ggplot2)
here::i_am('analysis/04a-OM-Plots.R')

LRP <- 0.17 # SB / SB0 (equilibrium unfished)

RunDF <- ListSS3Runs(
  SSDir_Base = here::here('SS3', 'Base-case')) |>
  dplyr::mutate(HistFile = here::here('objects', 'Hist', paste0(OM, '.hist')),
                HasHist  = file.exists(HistFile))

# SS3 runs considered as OMs (life-history and catchability runs, including
# those excluded with a reason), and the OMs included in the MSE
SS3Runs <- RunDF[RunDF$Type != 'Assessment', ]
OM_DF   <- RunDF[RunDF$HasHist & RunDF$Include, ]

HistList <- lapply(OM_DF$HistFile, readRDS) |> stats::setNames(OM_DF$OM)
BaseHist <- HistList[['00']]

OM_Label <- stats::setNames(paste0(OM_DF$OM, ': ', OM_DF$Description), OM_DF$OM)

# annual (calendar year) mean over seasons of a MSEtool time series
AnnualMean <- function(df) {
  df$Year <- floor(df$Year)
  stats::aggregate(Value ~ Sim + Year, data = df, FUN = mean)
}

# ---- ss3_runs_calc ----
SS3Pars <- purrr::map_dfr(seq_len(nrow(SS3Runs)), function(i) {
  Rep <- MSEtool::ImportSSReport(SS3Runs$Path[i], silent = TRUE)[[1]]
  Par <- function(Label) Rep$parameters$Value[Rep$parameters$Label == Label]
  M   <- Rep$Natural_Mortality
  M   <- M[M$Sex == 1 & M$Yr == Rep$endyr & M$Seas == 1, ]
  data.frame(OM   = SS3Runs$OM[i],
             h    = Par('SR_BH_steep'),
             M    = mean(unlist(M[1, intersect(as.character(0:100), names(M))])),
             Lmax = Par('L_at_Amax_Fem_GP_1'),
             K    = Par('VonBert_K_Fem_GP_1'),
             L50  = Par('Mat50%_Fem_GP_1'),
             LnR0 = Par('SR_LN(R0)'),
             SB0  = Rep$derived_quants$Value[Rep$derived_quants$Label == 'SSB_unfished'])
})

ss3_par <- function(om, col, digits = 2) round(SS3Pars[[col]][SS3Pars$OM == om], digits)

# ---- ss3_runs_table ----
# short reasons for the excluded runs (explained in full in the TSD text)
ExcludedReason <- c('05' = 'Implausibly large stock',
                    '06' = 'Implausibly large stock')

knitr::kable(
  data.frame(OM          = SS3Runs$OM,
             Description = SS3Runs$Description,
             Included    = ifelse(SS3Runs$Include, 'Yes', 'No'),
             Reason      = dplyr::coalesce(unname(ExcludedReason[SS3Runs$OM]),
                                           ifelse(SS3Runs$Include, '', 'See text'))),
  row.names = FALSE, col.names = c('OM', 'Description', 'Included', 'Reason for exclusion')
) |>
  kableExtra::kable_styling(full_width = FALSE) |>
  kableExtra::column_spec(2, extra_css = 'white-space: nowrap;')

# ---- ss3_pars_table ----
# input parameters: shown where they differ from the base case, a ditto mark otherwise
Base   <- SS3Pars$OM == '00'
Ditto  <- function(x) ifelse(!Base & x == x[Base], '”', as.character(x))

knitr::kable(
  data.frame(OM   = SS3Pars$OM,
             h    = Ditto(SS3Pars$h),
             M    = Ditto(round(SS3Pars$M, 3)),
             Lmax = Ditto(SS3Pars$Lmax),
             K    = Ditto(round(SS3Pars$K, 3)),
             L50  = Ditto(round(SS3Pars$L50, 1)),
             SB0  = vapply(SS3Pars$SB0, \(x) {
               if (x < 1e7)
                 format(round(x), big.mark = ',') else
                 formatC(x, format = 'e', digits = 1)
             }, character(1))),
  row.names = FALSE, align = c('l', rep('r', 6)),
  col.names = c('OM', 'Steepness', 'Mean female M', 'Female length at Amax (cm)',
                'Female K', 'Female L50 (cm)', 'Unfished SB (t)')
)

# ---- life_history ----
LifeHistory <- purrr::imap_dfr(HistList, function(Hist, om) {
  Stock  <- Hist@OM@Stock$Female
  LastTS <- dim(Stock@Length@MeanAtAge)[3]
  AtAge  <- function(arr, Variable) {
    data.frame(OM = OM_Label[[om]], Age = as.numeric(dimnames(arr)$Age),
               Value = as.numeric(arr[1, , LastTS]), Variable = Variable)
  }
  # OM rates are per time step (quarter); x Seasons gives annual rates
  rbind(AtAge(Stock@Length@MeanAtAge, 'Length-at-age (cm)'),
        AtAge(Stock@NaturalMortality@MeanAtAge * Hist@OM@Seasons,
              'Natural mortality-at-age (per year)'),
        AtAge(Stock@Maturity@MeanAtAge, 'Maturity-at-age'))
})

# most OMs share the base-case curve in a panel (e.g. steepness and catchability OMs in all
# of them): draw only the curves that differ from the base case, with the base case in black
BaseLabel <- OM_Label[['00']]
BaseLH    <- LifeHistory[LifeHistory$OM == BaseLabel, c('Variable', 'Age', 'Value')]
names(BaseLH)[3] <- 'Base'
LifeHistory <- LifeHistory |>
  dplyr::left_join(BaseLH, by = c('Variable', 'Age')) |>
  dplyr::group_by(Variable, OM) |>
  dplyr::filter(OM == BaseLabel | any(abs(Value - Base) > 1e-6 * pmax(1, abs(Base)))) |>
  dplyr::ungroup()
LH_OMs    <- unique(LifeHistory$OM)
LH_Colour <- stats::setNames(c('black', grDevices::palette.colors(length(LH_OMs), 'Okabe-Ito')[-1]),
                             c(BaseLabel, setdiff(LH_OMs, BaseLabel)))
LifeHistory$OM <- factor(LifeHistory$OM, levels = names(LH_Colour))

ggplot(LifeHistory, aes(x = Age, y = Value, colour = OM)) +
  geom_line(aes(linewidth = OM == BaseLabel)) +
  scale_colour_manual(values = LH_Colour) +
  scale_linewidth_manual(values = c(`TRUE` = 1.1, `FALSE` = 0.6), guide = 'none') +
  facet_wrap(~Variable, scales = 'free_y', ncol = 1) +
  expand_limits(y = 0) +
  labs(x = 'Age (years)', y = NULL, colour = 'OM',
       caption = 'OMs not shown in a panel are identical to the base case') +
  theme_bw() +
  theme(legend.position = 'right')

# ---- catch_data ----
MSEtool::PlotLandings(MSEtool::Data(BaseHist)[[1]][[1]],
                      byFleet = FALSE, AggregateYear = TRUE) +
  labs(y = 'Catch (t)')

# ---- catch_check ----
# annual catch data vs OM catch, base case
DataLandings <- BaseHist@Data[[1]][[1]]@Landings@Value
DataCatch    <- tapply(rowSums(DataLandings), floor(as.numeric(rownames(DataLandings))), sum)
OMLandings   <- Landings(BaseHist, df = TRUE, Reduce = FALSE)
OMCatch      <- colMeans(tapply(OMLandings$Value, list(OMLandings$Sim, floor(OMLandings$Year)), sum))
CatchDiff    <- max(abs(OMCatch[names(DataCatch)] / DataCatch - 1))

# ---- index_table ----
# S6 is a recruitment index
Survey     <- BaseHist@Data[[1]][[1]]@Survey
BaseRep    <- suppressMessages(MSEtool::ImportSSReport(RunDF$Path[RunDF$OM == '00'], silent = TRUE))[[1]]
LikeFleet  <- BaseRep$likelihoods_by_fleet
SurvLambda <- unlist(LikeFleet[LikeFleet$Label == 'Surv_lambda', Survey@Name])
IndexYear  <- floor(as.numeric(rownames(Survey@Value)))
IndexYears <- vapply(seq_along(Survey@Name), \(i) {
  paste(range(IndexYear[!is.na(Survey@Value[, i])]), collapse = '--')
}, character(1))

knitr::kable(
  data.frame(Index  = Survey@Name,
             Years  = IndexYears,
             Units  = Survey@Units,
             Lambda = unname(SurvLambda)),
  col.names = c('Index', 'Years', 'Units', 'SS3 lambda'),
  row.names = FALSE
)

# ---- index_fits ----
# all indices used by the CMPs (IndexSets in 05b-Define-CMPs.R); S5 and S8 are not fitted in SS3
CMPIndices <- c('S2_JPN_WCNPO_OSDWCOLL_late_Area1', 'S4_JPN_WCNPO_OSDWLL_late_Area2',
                'S5_TWN_WCNPO_DWLL_late', 'S8_US_WCNPO_LL_shallow_late')
MSEtool::PlotIndexFit(BaseHist, 'Survey', nsim = 0, ribbon = TRUE, Fleets = CMPIndices) +
  labs(y = 'Index (relative)')

# ---- stock_status_calc ----
TerminalYear <- max(floor(Years(BaseHist@OM, 'Historical')))

Terminal <- function(df, Stock = NULL) {
  if (!is.null(Stock)) df <- df[df$Stock %in% Stock, ]
  df <- AnnualMean(df)
  stats::median(df$Value[df$Year == TerminalYear])
}

StockStatus <- purrr::imap_dfr(HistList, function(Hist, om) {
  data.frame(
    OM       = om,
    SB_SB0   = Terminal(SB_SB0(Hist, type = 'Equilibrium', df = TRUE, Reduce = FALSE), 'Female'),
    SB_SBMSY = Terminal(SB_SBMSY(Hist, df = TRUE, Reduce = FALSE), 'Female'),
    F_FMSY   = Terminal(F_FMSY(Hist, df = TRUE, Reduce = FALSE)),
    FMSY     = stats::median(FMSY(Hist)) * Hist@OM@Seasons,  # apical F per quarter x Seasons = annual
    SBMSY    = stats::median(SBMSY(Hist)[, 'Female', ]),
    SB0      = stats::median(SB0(Hist, 'Equilibrium', Reduce = FALSE)[, 'Female', ]),
    MSY      = stats::median(apply(MSYLandings(Hist), c(1, 3), sum))
  )
})
StockStatus$SBMSY_SB0 <- StockStatus$SBMSY / StockStatus$SB0

status_range <- function(col, digits = 2) round(range(StockStatus[[col]]), digits)
base_value   <- function(col, digits = 2) round(StockStatus[[col]][StockStatus$OM == '00'], digits)

# ---- stock_status_table ----
knitr::kable(
  data.frame(OM = OM_Label[StockStatus$OM],
             StockStatus[, c('SB_SB0', 'SB_SBMSY', 'F_FMSY', 'SBMSY_SB0')]),
  digits = 2, row.names = FALSE,
  col.names = c('OM', 'SB/SB0', 'SB/SBMSY', 'F/FMSY', 'SBMSY/SB0')
)

# ---- refpoints_table ----
knitr::kable(
  data.frame(OM = OM_Label[StockStatus$OM], FMSY = StockStatus$FMSY,
             SBMSY = round(StockStatus$SBMSY), MSY = round(StockStatus$MSY)),
  digits = 3, row.names = FALSE, format.args = list(big.mark = ',', scientific = FALSE),
  col.names = c('OM', 'FMSY', 'SBMSY (t)', 'MSY (t)')
)

# ---- trajectories ----
Trajectories <- purrr::imap_dfr(HistList, function(Hist, om) {
  SBSB0 <- SB_SB0(Hist, type = 'Equilibrium', df = TRUE, Reduce = FALSE)
  SBMSY <- SB_SBMSY(Hist, df = TRUE, Reduce = FALSE)
  rbind(data.frame(AnnualMean(SBSB0[SBSB0$Stock == 'Female', ]), Variable = 'SB/SB[0]'),
        data.frame(AnnualMean(SBMSY[SBMSY$Stock == 'Female', ]), Variable = 'SB/SB[MSY]'),
        data.frame(AnnualMean(F_FMSY(Hist, df = TRUE, Reduce = FALSE)), Variable = 'F/F[MSY]')) |>
    stats::aggregate(Value ~ Year + Variable, data = _, FUN = stats::median) |>
    dplyr::mutate(OM = OM_Label[[om]])
})
TrajVars <- c('SB/SB[0]', 'SB/SB[MSY]', 'F/F[MSY]')
Trajectories$Variable <- factor(Trajectories$Variable, levels = TrajVars)

# SB/SB0 in the first year and the terminal year, for the TSD text
FirstYear <- min(Trajectories$Year)
first_sb  <- function(om, digits = 2) {
  round(Trajectories$Value[Trajectories$OM == OM_Label[[om]] & Trajectories$Year == FirstYear &
                             Trajectories$Variable == 'SB/SB[0]'], digits)
}
term_sb <- function(om, digits = 2) round(StockStatus$SB_SB0[StockStatus$OM == om], digits)

RefLines <- data.frame(Variable = factor(TrajVars, levels = TrajVars), Value = c(LRP, 1, 1))

# one figure per source of uncertainty, each comparing its OMs with the base case (black)
TrajGroups <- c('01' = 'Natural mortality',
                '02' = 'Natural mortality',
                '03' = 'Steepness',
                '04' = 'Steepness',
                '07' = 'Growth',
                '08' = 'Maturity',
                '09' = 'Maturity',
                '10' = 'Maturity',
                '20a' = 'Catchability',
                '20b' = 'Catchability')

Missing <- setdiff(setdiff(names(HistList), '00'), names(TrajGroups))
if (length(Missing)) stop('Add OM(s) ', paste(Missing, collapse = ', '), ' to TrajGroups')
TrajGroups <- TrajGroups[names(TrajGroups) %in% names(HistList)]

# colours for the base case and the OMs of a group
GroupColours <- function(Group) {
  OMs <- c('00', names(TrajGroups)[TrajGroups == Group])
  stats::setNames(c('black', grDevices::palette.colors(length(OMs), 'Okabe-Ito')[-1]),
                  unname(OM_Label[OMs]))
}

PlotTrajectories <- function(Group) {
  Colour <- GroupColours(Group)
  DF     <- Trajectories[Trajectories$OM %in% names(Colour), ]
  DF$OM  <- factor(DF$OM, levels = names(Colour))

  ggplot(DF, aes(x = Year, y = Value, colour = OM)) +
    geom_hline(data = RefLines, aes(yintercept = Value), linetype = 2, colour = 'darkgray') +
    geom_line(aes(linewidth = OM == names(Colour)[1])) +
    scale_colour_manual(values = Colour) +
    scale_linewidth_manual(values = c(`TRUE` = 1, `FALSE` = 0.7), guide = 'none') +
    facet_wrap(~Variable, scales = 'free_y', nrow = 1, labeller = label_parsed) +
    expand_limits(y = c(0,1)) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    labs(x = 'Year', y = NULL, colour = NULL) +
    theme_bw() +
    theme(legend.position = 'bottom') +
    guides(colour = guide_legend(ncol = 2))
}

# ---- trajectories_m ----
PlotTrajectories('Natural mortality')

# ---- trajectories_h ----
PlotTrajectories('Steepness')

# ---- trajectories_growth ----
PlotTrajectories('Growth')

# ---- trajectories_maturity ----
PlotTrajectories('Maturity')

# ---- trajectories_q ----
PlotTrajectories('Catchability')

# ---- creep_indices ----
# the indices each effort creep run was conditioned on: the observed indices in the base
# case, and in 20a and 20b the observed indices deflated for an assumed annual increase
# in catchability (equal to the observed value in the first year of each index).
# Fitted indices only (lambda > 0 in the base case)
CreepOMs   <- c('00', names(TrajGroups)[TrajGroups == 'Catchability'])
CreepIndex <- purrr::map_dfr(CreepOMs, function(om) {
  Rep <- MSEtool::ImportSSReport(RunDF$Path[RunDF$OM == om], silent = TRUE)[[1]]
  data.frame(OM = OM_Label[[om]], Index = Rep$cpue$Fleet_name, Year = Rep$cpue$Yr,
             Obs = Rep$cpue$Obs)
})
FittedIndex <- names(SurvLambda)[SurvLambda > 0]
CreepIndex  <- CreepIndex[CreepIndex$Index %in% FittedIndex, ]
CreepColour <- GroupColours('Catchability')
CreepIndex$OM <- factor(CreepIndex$OM, levels = names(CreepColour))

ggplot(CreepIndex, aes(x = Year, y = Obs, colour = OM)) +
  geom_line(aes(linewidth = OM == names(CreepColour)[1])) +
  scale_colour_manual(values = CreepColour) +
  scale_linewidth_manual(values = c(`TRUE` = 1, `FALSE` = 0.7), guide = 'none') +
  facet_wrap(~Index, scales = 'free_y', ncol = 2) +
  expand_limits(y = 0) +
  ggplot2::scale_x_continuous(expand = c(0, 0)) +
  ggplot2::scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(x = 'Year', y = 'Index', colour = NULL) +
  theme_bw() +
  theme(legend.position = 'bottom') +
  guides(colour = guide_legend(ncol = 1))
