### Script for Regression Analysis
#' @param D_RISK_NGW (numeric) the risk for nitrate leaching to groundwater given field properties
#' @param D_RISK_NSW (numeric) the risk for nitrate leaching and runoff to surface water given field properties
#' @param D_RISK_PSW (numeric) the risk for phosphorus leaching and runoff to surface water given field properties
#' @param D_RISK_NUE (numeric) the efficiency of nitrogen and phosphorus fertilizer use given field properties
#' @param D_RISK_WB  (numeric) the potential to buffer and store water and efficiently use water for plant growth given field properties

# Read required R packages
require(data.table)

# Read subset with risk scores
subset_amstelland1_riskscores <- readRDS("../subset_amstelland1_riskscores.rds")

# Read final data tables with risk scores
dt.sw <- readRDS("../final_dt_sw_scores.rds")
dt.gw <- readRDS("../final_dt_gw_scores.rds")

# Ensure right years and filter missing values
dt.sw <- dt.sw[year >= 2000 & year <= 2019]
dt.gw <- dt.gw[year >= 2000 & year <= 2019]
# dt.sw.cleaned <- dt.sw[!is.na(NH4) & !is.na(NKj) & !is.na(NO2) & !is.na(NO3) & !is.na(Ntot)& !is.na(PO4) & !is.na(Ptot)]
# dt.gw.cleaned <- dt.gw[!is.na(NH4) & !is.na(NKj) & !is.na(NO2) & !is.na(NO3) & !is.na(Ntot)& !is.na(PO4) & !is.na(Ptot)]

# simple linear regression for nitrogen
lm_n <- lm(Ntot.median ~ D_RISK_NSW, data = dt.sw)
lm_n <- lm(Ntot.median ~ D_RISK_NGW, data = dt.gw)
summary(lm_n)

lm_n <- lm(Ntot.median ~ B_SOILTYPE_AGR + A_FE_OX + A_AL_OX ...., data = dt.sw)
lm_n <- lm(Ntot.median ~ B_SOILTYPE_AGR + A_FE_OX + A_AL_OX ...., data = dt.gw)

# simple linear regression for phosphorus
lm_p <- lm(Ptot.median ~ D_RISK_PSW, data = dt.sw)
summary(lm_p)

# Fit multiple regression
lm_n_multi <- lm(Ntot.median ~ D_RISK_NSW + Soil_type + ..., data = dt.sw.cleaned)
lm_n_multi <- lm(Ntot.median ~ D_RISK_NGW + Soil_type + ..., data = dt.sw.cleaned)
summary(lm_n_multi)

# Fit regression with Ntot.n as weighing factor
lm_model <- lm(Ntot.sw ~ D_RISK_NSW, data = dt.n.sw, weights = Ntot.n)
lm_model <- lm(Ntot.gw ~ D_RISK_NGW, data = dt.n.gw, weights = Ntot.n)
lm_model <- lm(Ptot.sw ~ D_RISK_PSW, data = dt.p.sw, weights = Ntot.n)
summary(lm_model)
