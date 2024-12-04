### Script for calculating FSWP risk scores, for further regression analysis

# Install required packages
# install.packages("devtools")
library("devtools")
# devtools::install_github("AgroCares/BedrijfsBodemWaterPlanCalculator")
library("BBWPC")

# Calculate risk scores
bbwp_field_scores()
?bbwp_field_scores
?bbwp_farm_score
?bbwp_field_indicators


# Read subset merged data summer nitrogen values and matched farms in Amstelland1
f1 <- readRDS("Subset_n_sw_Amstelland1.rds")
colnames(f1)
bbwp_field_scores(f1)

bbwp
# Calculate risk indicators (D_RISK_NGW, D_RISK_NSW, D_RISK_PSW, D_RISK_NUE, D_RISK_WB)
f1[, risk_score := bbwp_field_indicators()]

# Calculate field scores:
f1[, field_score := bbwp_field_scores(
  B_SOILTYPE_AGR = B_SOILTYPE_AGR,
  B_GWL_CLASS = B_GWL_CLASS,
  A_P_SG = A_P_SG,
  B_SLOPE_DEGREE = B_SLOPE_DEGREE,
  B_LU_BBWP = B_LU_BBWP,
  B_AER_CBS = B_AER_CBS,
  M_DRAIN = M_DRAIN,
  D_SA_W = D_SA_W,
  D_RISK_NGW = D_RISK_NGW,
  D_RISK_NSW = D_RISK_NSW,
  D_RISK_PSW = D_RISK_PSW,
  D_RISK_NUE = D_RISK_NUE,
  D_RISK_WB = D_RISK_WB,
  B_GWP = B_GWP,
  B_AREA_DROUGHT = B_AREA_DROUGHT,
  B_CT_PSW = B_CT_PSW,
  B_CT_NSW = B_CT_NSW,
  B_CT_PSW_MAX = 0.5,  # Defaults can be hard-coded
  B_CT_NSW_MAX = 5,    # Defaults can be hard-coded
  #measures = measures,
  sector = "arable",
  #penalty = TRUE,       # Optional argument
  #B_LS_HYDROCAT = B_LS_HYDROCAT # For differentiating effect of measures on water buffering
), by = fieldid]
  
