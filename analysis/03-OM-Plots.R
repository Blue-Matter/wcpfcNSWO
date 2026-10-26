# Diagnostic plots for the operating models
# Chunks are also run in the TSD Operating Models chapter (tsd/operating-models.qmd)

# ---- load_files ----
library(MSEtool)
library(wcpfcNSWO)
library(ggplot2)
here::i_am('analysis/03-OM-Plots.R')

LRP <- 0.17 # SB / SB0 (equilibrium unfished)

RunDF <- ListSS3Runs(
  SSDir_Base = here::here('..', 'WCNPOSWO-2023', 'Final Base-case')) |>
  dplyr::mutate(HistFile = here::here('objects', 'Hist', paste0(OM, '.hist')),
                HasHist  = file.exists(HistFile))

# SS3 runs considered as OMs (with an OM, or excluded with a reason), and the
# OMs included in the MSE
SS3Runs <- RunDF[RunDF$HasHist | nzchar(RunDF$Note), ]
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
ExcludedReason <- c('03' = 'Output identical to base case',
                    '05' = 'Implausibly large stock',
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

ggplot(LifeHistory, aes(x = Age, y = Value, colour = OM)) +
  geom_line() +
  facet_wrap(~Variable, scales = 'free_y', ncol = 1) +
  expand_limits(y = 0) +
  labs(x = 'Age (years)', y = NULL, colour = 'OM') +
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
# SS3 lambdas from the base case control file; S6 is a recruitment index
Survey     <- BaseHist@Data[[1]][[1]]@Survey
IndexYear  <- floor(as.numeric(rownames(Survey@Value)))
IndexYears <- vapply(seq_along(Survey@Name), \(i) {
  paste(range(IndexYear[!is.na(Survey@Value[, i])]), collapse = '--')
}, character(1))

knitr::kable(
  data.frame(Index  = Survey@Name,
             Years  = IndexYears,
             Units  = Survey@Units,
             Lambda = c(1, 1, 1, 1, 0, 1, 1, 0),
             CMPs   = c('SP', 'IR, IT, SP', 'SP', 'IR, IT, SP', '', '', '', '')),
  col.names = c('Index', 'Years', 'Units', 'SS3 lambda', 'Used by CMPs'),
  row.names = FALSE
)

# ---- index_fits ----
MSEtool::PlotIndexFit(BaseHist, 'Survey', nsim = 0, ribbon = TRUE,
                      Fleets = c('S2_JPN_WCNPO_OSDWCOLL_late_Area1', 'S4_JPN_WCNPO_OSDWLL_late_Area2')) +
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

RefLines <- data.frame(Variable = factor(TrajVars, levels = TrajVars), Value = c(LRP, 1, 1))

ggplot(Trajectories, aes(x = Year, y = Value, colour = OM)) +
  geom_hline(data = RefLines, aes(yintercept = Value), linetype = 2, colour = 'darkgray') +
  geom_line() +
  facet_wrap(~Variable, scales = 'free_y', ncol = 1, labeller = label_parsed) +
  expand_limits(y = 0) +
  labs(x = 'Year', y = NULL, colour = 'OM') +
  theme_bw()
