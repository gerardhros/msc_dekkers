### Script for calculating FSWP risk scores, for further regression analysis

# Install required packages
# install.packages("devtools")
library("devtools")
# devtools::install_github("AgroCares/BedrijfsBodemWaterPlanCalculator")
library("BBWPC")


# Read subset merged data summer nitrogen values and matched farms in Amstelland1
f1 <- fread("Subset_n_sw_Amstelland1.rds")

colnames(f1)
bbwp_field_scores(f1)

# in your case you do not have LSW data, so assume it is NULL

# set LSW to country mean properties
LSW <- bbwp_lsw_properties(B_LSW_ID = f1$B_LSW_ID)
LSW <- unique(LSW)

# Set datatable to dataset in question
dt<- f1


dt <- bbwp_field_properties(B_SOILTYPE_AGR = f1$B_SOILTYPE_AGR, 
                            B_LU_BBWP = f1$B_LU_BBWP,
                            B_GWL_CLASS = f1$B_GWL_CLASS, 
                            B_SC_WENR = f1$B_SC_WENR, 
                            B_HELP_WENR = f1$B_HELP_WENR,
                            B_SLOPE_DEGREE = f1$B_SLOPE_DEGREE,
                            B_AER_CBS = f1$B_AER_CBS,
                            A_CLAY_MI = f1$A_CLAY_MI, 
                            A_SAND_MI = f1$A_SAND_MI, 
                            A_SILT_MI = f1$A_SILT_MI, 
                            A_SOM_LOI = f1$A_SOM_LOI, 
                            A_N_RT = f1$A_N_RT,
                            A_FE_OX = f1$A_FE_OX, 
                            A_AL_OX = f1$A_AL_OX, 
                            A_P_CC = f1$A_P_CC, 
                            A_P_AL = f1$A_P_AL, 
                            A_P_WA = f1$A_P_WA, 
                            A_P_SG = f1$A_P_SG,
                            D_SA_W = f1$D_SA_W, 
                            D_RO_R =  f1$D_RO_R, 
                            B_LSW_ID = f1$B_LSW_ID,
                            LSW = LSW)

# Calculate risk indicators (D_RISK_NGW, D_RISK_NSW, D_RISK_PSW, D_RISK_NUE, D_RISK_WB)
# Aggregate BBWP risk indicators into five indicators
dt.ind <- bbwp_field_indicators(D_NGW_SCR = dt$ngw_scr,
                                D_NGW_LEA = dt$ngw_lea,
                                D_NGW_NLV = dt$ngw_nlv,
                                D_NSW_SCR = dt$nsw_scr,
                                D_NSW_GWT = dt$nsw_gwt,
                                D_NSW_RO = dt$nsw_ro,
                                D_NSW_SLOPE = dt$nsw_slope,
                                D_NSW_WS = dt$nsw_ws,
                                D_NSW_NLV = dt$nsw_nlv,
                                D_PSW_SCR = dt$psw_scr,
                                D_PSW_GWT= dt$psw_gwt,
                                D_PSW_RO = dt$psw_ro,
                                D_PSW_SLOPE = dt$psw_slope,
                                D_PSW_WS = dt$psw_ws,
                                D_PSW_PCC = dt$psw_pcc,
                                D_PSW_PSG = dt$psw_psg,
                                D_PSW_PRET = dt$psw_pret,
                                D_NUE_WRI = dt$npe_wri,
                                D_NUE_PBI = dt$npe_pbi,
                                D_NUE_WDRI = dt$npe_wdri,
                                D_NUE_NLV = dt$npe_nlv,
                                D_WUE_WWRI = dt$wue_wwri,
                                D_WUE_WDRI = dt$wue_wdri,
                                D_WUE_WHC = dt$wue_whc,
                                penalty = TRUE
)

# Add fieldid column before merging
dt.ind[, fieldid := f1$fieldid]

# Merge riskscores with dataset by fieldid column  
subset_amstelland1_riskscores <- merge(dt.ind, f1, by = "fieldid", allow.cartesian = TRUE)
saveRDS(subset_amstelland1_riskscores, "../subset_amstelland1_riskscores.rds")
