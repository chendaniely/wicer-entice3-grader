
# Fruit_CK
Fruit_CK <- gle(PT_ONLY_DATA_DF$fruit_wk_sm_perday, PT_ONLY_DATA_DF$pt_recommended_fruit)

# Veg_CK
Veg_CK <- gle(PT_ONLY_DATA_DF$veganddarkvege_wk_perday, PT_ONLY_DATA_DF$pt_recommended_vege)

# VigPA_CK
VigPA_CK <- gle(PT_ONLY_DATA_DF$vig_amount_minwk, PT_ONLY_DATA_DF$pt_recommended_vigorous_exercise)

# ModPA_CK
ModPA_CK <- gle(PT_ONLY_DATA_DF$mod_amount_minwk, PT_ONLY_DATA_DF$pt_recommended_moderate_exercise)

# 30Dep_CK
"30Dep_CK" <- gle(PT_ONLY_DATA_DF$cdc30_depressed, PT_SEX_AGE_30DEP)

# 30Anx_CK
"30Anx_CK" <- gle(PT_ONLY_DATA_DF$cdc30_anxious, PT_SEX_AGE_30ANX)

# BevWk_CK
comp_values <- c(PT_ONLY_DATA_DF$soda_week_sm, PT_ONLY_DATA_DF$juice_week_sm, PT_ONLY_DATA_DF$sugaryfruitdrink_week_sm)
min_value <- max(comp_values)
num_values <- sum(comp_values == min_value)
if (num_values > 1) {
    BevWk_CK <- 4
} else if (min_value == PT_ONLY_DATA_DF$soda_week_sm) {
    BevWk_CK <- 1
} else if (min_value == PT_ONLY_DATA_DF$juice_week_sm) {
    BevWk_CK <- 2
} else if (min_value == PT_ONLY_DATA_DF$sugaryfruitdrink_week_sm) {
    BevWk_CK <- 3
} else {
    BevWk_CK <- NULL
}

# 30Comp_CK
comp_values <- c(PT_ONLY_DATA_DF$cdc30_energy, PT_ONLY_DATA_DF$cdc30_depressed, PT_ONLY_DATA_DF$cdc30_anxious)
min_value <- min(comp_values)
num_values <- sum(comp_values == min_value)
if (num_values > 1) {
    "30Comp_CK" <- 4
} else if (min_value == PT_ONLY_DATA_DF$cdc30_energy) {
    "30Comp_CK" <- 1
} else if (min_value == PT_ONLY_DATA_DF$cdc30_depressed) {
    "30Comp_CK" <- 2
} else if (min_value == PT_ONLY_DATA_DF$cdc30_anxious) {
    "30Comp_CK" <- 3
} else {
    "30Comp_CK" <- NULL
}

# RecPA_CK
"RecPA_CK" <- 2 # constant

# BevRec_CK
"BevRec_CK" <- 2 # constant

# DepA_CK
DepA_CK <- gle(PT_ONLY_DATA_DF$pt_depression_gauge, PT_SEX_AGE_DEPA_GAUGE)

# Stress_CK
Stress_CK <- gle(PT_ONLY_DATA_DF$pt_stress_gauge, PT_SEX_AGE_STRESS_GAUGE)

# Anx_CK
Anx_CK <- gle_range(PT_ONLY_DATA_DF$promis_anxiety_t, 50)

# DepB_CK
DepB_CK <- gle_range(PT_ONLY_DATA_DF$proms_depression_t, 50)

# BMI_CK
BMI_CK <- PT_ONLY_DATA_DF$pt_bmi_cat

# Waist_CK
Waist_CK <- PT_ONLY_DATA_DF$pt_waist_cat

# OvHea_CK
OvHea_CK <- gle(PT_ONLY_DATA_DF$sf_1, PT_SEX_AGE_OVHEA, less = 3, greater = 1)

# RunD_CK
RunD_CK <- gle(PT_ONLY_DATA_DF$fatexp41, PT_SEX_AGE_RUND, less = 3, greater = 1)

# RiskBP_CK1
RiskBP_CK1 <- PT_ONLY_DATA_DF$pt_bp_s_cat

# RiskBP_CK2
RiskBP_CK2 <- 1 # constant

# RiskBMI_CK
RiskBMI_CK <- 1 # constant

# Panel_CK
Panel_CK <- calculate_panel_colver_ck(PT_ONLY_DATA_DF$pt_Panel_CK_unadjusted)

# Clover_CK
Clover_CK <- calculate_panel_colver_ck(PT_ONLY_DATA_DF$Clover_CK_unadjusted)
