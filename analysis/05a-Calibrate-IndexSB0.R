# Calibrate the index level at SB0 (IndexSB0) of each index for the index-based
# CMPs (IR, IT), so their status, index / IndexSB0, equals SB/SB0 on average in
# the projection, where the HCR operates, and the HCR control points are in
# units of SB/SB0.
#
# Calibrated on the historical years, IndexSB0 gave a CMP status of only ~0.77 x
# OM SB/SB0 in the projection (all HCRs): the
# projected indices sit lower relative to SB/SB0 than the historical indices, as
# SB grows faster than the biomass the indices track. So IndexSB0 is calibrated
# on a reference projection of the base case OM with a constant catch (MSEtool
# CurrentCatch), which does not depend on any CMP or its tuning:
#
#   IndexSB0_j = median over sims of [mean index_j / mean SB/SB0], CalibYears
#
# Calculated once, from the base case OM, and used, as MP arguments, with every
# OM. Saved to objects/IndexSB0.rds, read by 05b-Define-CMPs.R
library(wcpfcNSWO)

HistFile   <- 'objects/Hist/00.hist'
CalibYears <- 2022:2051          # the first 30 projection years
OutFile    <- 'objects/IndexSB0.rds'

Hist <- readRDS(HistFile)
MSE  <- MSEtool::Project(Hist, MPs = list(CurrentCatch = MSEtool::CurrentCatch),
                         parallel = FALSE, silent = TRUE)

# SB/SB0 (equilibrium, female) by sim and year: annual mean over time steps
Dep <- as.data.frame(MSEtool::SB_SB0(MSE, type = 'Equilibrium', df = TRUE, Reduce = FALSE))
Dep <- Dep[Dep$Stock == 'Female', ]
Dep$Y <- floor(as.numeric(Dep$Year) + 1e-6)
Dep <- stats::aggregate(Value ~ Sim + MP + Y, Dep, mean)

# observed indices by sim, year and index (posterior predictive data)
Index <- purrr::imap_dfr(MSEtool::PPD(MSE)[['CurrentCatch']], \(SimList, sim) {
  s <- SimList[[1]]@Survey
  v <- s@Value
  data.frame(Sim = as.numeric(sim), Y = floor(rep(as.numeric(rownames(v)), ncol(v)) + 1e-6),
             Index = rep(s@Name, each = nrow(v)), Value = as.vector(v))
})
Index <- Index[is.finite(Index$Value) & Index$Value > 0, ]

Calib <- function(Years, MP) {
  d  <- Dep[Dep$Y %in% Years & Dep$MP == MP, ]
  DepSim <- tapply(d$Value, d$Sim, mean)
  ix <- Index[Index$Y %in% Years, ]
  if (!nrow(ix)) return(NULL)
  r <- stats::aggregate(Value ~ Sim + Index, ix, mean)
  r$Ratio <- r$Value / DepSim[as.character(r$Sim)]
  out <- stats::aggregate(Ratio ~ Index, r, stats::median)
  stats::setNames(out$Ratio, out$Index)
}

IndexSB0 <- Calib(CalibYears, 'CurrentCatch')
# for comparison: the historical calibration (each index over its observed years)
HistIndexSB0 <- vapply(names(IndexSB0), \(nm) {
  Yrs <- unique(Index$Y[Index$Index == nm & Index$Y <= max(MSEtool::Years(MSE, 'H'))])
  Calib(Yrs, 'Historical')[[nm]]
}, numeric(1))

Result <- list(IndexSB0 = IndexSB0, HistIndexSB0 = HistIndexSB0, CalibYears = CalibYears,
               MP = 'CurrentCatch', OM = HistFile, nSim = MSE@OM@nSim,
               MSEtool = as.character(utils::packageVersion('MSEtool')), Date = Sys.time())
saveRDS(Result, OutFile)
print(data.frame(Index = names(IndexSB0), Projection = signif(IndexSB0, 4),
                 Historical = signif(HistIndexSB0, 4), Ratio = round(IndexSB0 / HistIndexSB0, 3)),
      row.names = FALSE)
