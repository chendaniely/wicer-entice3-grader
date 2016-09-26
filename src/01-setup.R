# RESPONDENT_DATA_DF <- read.csv(RESPONDENT_DATA)
# RESPONDENT_DEMO_DF <- read.csv(RESPONDENT_DEMO)

# calculate addtional values
FULL_DATA_DF$pt_age_group <- sapply(X = FULL_DATA_DF$Age, FUN = calculate_age_group)
FULL_DATA_DF$pt_recommended_fruit <- mapply(FUN = calculate_fruit_servings_recommended, pt_sex = FULL_DATA_DF$Sex, pt_age = FULL_DATA_DF$Age)
FULL_DATA_DF$pt_recommended_vege <- mapply(FUN = calculate_vegetable_servings_recommended, pt_sex = FULL_DATA_DF$Sex, pt_age = FULL_DATA_DF$Age)
FULL_DATA_DF$pt_recommended_vigorous_exercise <- mapply(FUN = calculate_vigorous_exercise_recommended, pt_sex = FULL_DATA_DF$Sex, pt_age = FULL_DATA_DF$Age)
FULL_DATA_DF$pt_recommended_moderate_exercise <- mapply(FUN = calculate_moderate_exercise_recommended, pt_sex = FULL_DATA_DF$Sex, pt_age = FULL_DATA_DF$Age)
FULL_DATA_DF$pt_depression_gauge <- sapply(X = FULL_DATA_DF$phq30_sum, FUN = calculate_depression_gauge)
FULL_DATA_DF$pt_stress_gauge <- sapply(X = FULL_DATA_DF$chr_sum, FUN = calculate_stress_gauge) ########################## what are valid values for chr_sum
FULL_DATA_DF$pt_bmi_cat <- sapply(X = FULL_DATA_DF$bmi_kgm2, FUN = calculate_pt_bmi_cat)
FULL_DATA_DF$pt_waist_cat <- mapply(FUN = calculate_pt_waist_in_cat, pt_waist_in = FULL_DATA_DF$waistcirc_inches_1, pt_sex = FULL_DATA_DF$Sex)
FULL_DATA_DF$pt_bp_s_cat <- sapply(X = FULL_DATA_DF$sbp, FUN = calculate_pt_bp_s_cat)
FULL_DATA_DF$pt_Panel_CK_unadjusted <- mapply(FUN = get_health_summary_colors,
                                   prolonged_stress_value = FULL_DATA_DF$chr_sum,
                                   bmi_value = FULL_DATA_DF$bmi_kgm2,
                                   oral_health_value = FULL_DATA_DF$o11,
                                   blood_pressure_s_value = FULL_DATA_DF$sbp,
                                   blood_pressure_d_value = FULL_DATA_DF$dbp)
FULL_DATA_DF$pt_cloverleaf_fruit <- mapply(FUN = calculate_cloverleaf_fruit_value,
                                           fruit_value = FULL_DATA_DF$fruit_wk_sm_perday,
                                           vegetable_value = FULL_DATA_DF$veganddarkvege_wk_perday,
                                           pt_sex = FULL_DATA_DF$Sex,
                                           pt_age = FULL_DATA_DF$Age)
FULL_DATA_DF$pt_cloverleaf_physical <- mapply(FUN = calculate_cloverleaf_physical_value,
                                              moderate_value = FULL_DATA_DF$mod_amount_minwk,
                                              vigorous_value = FULL_DATA_DF$vig_amount_minwk)
FULL_DATA_DF$pt_cloverleaf_overall <- mapply(FUN = calculate_cloverleaf_overall_value,
                                             overall_value = FULL_DATA_DF$sf_1)
FULL_DATA_DF$pt_cloverleaf_mental <- mapply(FUN = calculate_cloverleaf_mental_value,
                                            mental_value = FULL_DATA_DF$cdc30_mental)
FULL_DATA_DF$Clover_CK_unadjusted <- mapply(FUN = calculate_cloverleaf_ck,
                                            fruit = FULL_DATA_DF$pt_cloverleaf_fruit,
                                            physical = FULL_DATA_DF$pt_cloverleaf_physical,
                                            overall = FULL_DATA_DF$pt_cloverleaf_overall,
                                            mental = FULL_DATA_DF$pt_cloverleaf_mental)
# subset only patient of interest
PT_ONLY_DATA_DF <- FULL_DATA_DF[FULL_DATA_DF[, 1] == PT_ID, ]
expect_equal(nrow(PT_ONLY_DATA_DF), 1)

PT_AGE <- PT_ONLY_DATA_DF$Age
PT_AGE_GROUP <- PT_ONLY_DATA_DF$pt_age_group
PT_SEX <- PT_ONLY_DATA_DF$Sex

full_data_age_sex_grouped_df <- FULL_DATA_DF[FULL_DATA_DF$pt_age_group == PT_AGE_GROUP & FULL_DATA_DF$Sex == PT_SEX, ] %>%
    group_by(pt_age_group, Sex)

## Fruit_CK
PT_SEX_AGE_FRUIT_DF <-  full_data_age_sex_grouped_df %>%
    summarise(avg_sex_age = mean(fruit_wk_sm_perday))
PT_SEX_AGE_FRUIT <- PT_SEX_AGE_FRUIT_DF$avg_sex_age

## Veg_CK
PT_SEX_AGE_VEG_DF <- full_data_age_sex_grouped_df %>%
    summarise(avg_sex_age = mean(veganddarkvege_wk_perday))
PT_SEX_AGE_VEG <- PT_SEX_AGE_VEG_DF$avg_sex_age

## VigPA_CK
PT_SEX_AGE_VIGPA_DF <- full_data_age_sex_grouped_df %>%
    summarise(avg_sex_age = mean(vig_amount_minwk))
PT_SEX_AGE_VIGPA <- PT_SEX_AGE_VIGPA_DF$avg_sex_age

## ModPA_CK
PT_SEX_AGE_MODPA_DF <- full_data_age_sex_grouped_df %>%
    summarise(avg_sex_age = mean(mod_amount_minwk))
PT_SEX_AGE_MODPA <- PT_SEX_AGE_MODPA_DF$avg_sex_age

## 30Dep_CK
PT_SEX_AGE_30DEP_DF <- full_data_age_sex_grouped_df %>%
    summarise(avg_sex_age = mean(cdc30_depressed))
PT_SEX_AGE_30DEP <- PT_SEX_AGE_30DEP_DF$avg_sex_age

## 30Anx_CK
PT_SEX_AGE_30ANX_DF <- full_data_age_sex_grouped_df %>%
    summarise(avg_sex_age = mean(cdc30_anxious))
PT_SEX_AGE_30ANX <- PT_SEX_AGE_30ANX_DF$avg_sex_age

## BevWk_CK
PT_SEX_AGE_BEVWK_DF <- full_data_age_sex_grouped_df %>%
    summarise(avg_sex_age = mean(sugaryfruitdrink_week_sm))
PT_SEX_AGE_BEVWK <- PT_SEX_AGE_BEVWK_DF$avg_sex_age

## 30Comp_CK
PT_SEX_AGE_30COMP_DF <- full_data_age_sex_grouped_df %>%
    summarise(avg_sex_age = mean(cdc30_mental))
PT_SEX_AGE_30COMP <- PT_SEX_AGE_30COMP_DF$avg_sex_age

## RecPA_CK
PT_SEX_AGE_RECPA_DF <- full_data_age_sex_grouped_df %>%
    summarise(avg_sex_age = mean(mod_amount_minwk))
PT_SEX_AGE_RECPA <- PT_SEX_AGE_RECPA_DF$avg_sex_age

## BevRec_CK
PT_SEX_AGE_BEVREC_DF <- full_data_age_sex_grouped_df %>%
    summarise(avg_sex_age = mean(juice_week_sm))
PT_SEX_AGE_BEVREC <- PT_SEX_AGE_BEVREC_DF$avg_sex_age

## DepA_CK
PT_SEX_AGE_DEPA_DF <- full_data_age_sex_grouped_df %>%
    summarise(avg_sex_age = mean(phq30_sum))
PT_SEX_AGE_DEPA <- PT_SEX_AGE_DEPA_DF$avg_sex_age

PT_SEX_AGE_DEPA_GAUGE <- calculate_depression_gauge(PT_SEX_AGE_DEPA)

## Stress_CK
PT_SEX_AGE_STRESS_DF <- full_data_age_sex_grouped_df %>%
    summarise(avg_sex_age = mean(chr_sum))
PT_SEX_AGE_STRESS <- PT_SEX_AGE_STRESS_DF$avg_sex_age

PT_SEX_AGE_STRESS_GAUGE <- calculate_depression_gauge(PT_SEX_AGE_STRESS)

## Anx_CK
PT_SEX_AGE_ANX_DF <- full_data_age_sex_grouped_df %>%
    summarise(avg_sex_age = mean(promis_anxiety_t))
PT_SEX_AGE_ANX <- PT_SEX_AGE_ANX_DF$avg_sex_age

## DepB_CK
PT_SEX_AGE_DEPB_DF <- full_data_age_sex_grouped_df %>%
    summarise(avg_sex_age = mean(proms_depression_t))
PT_SEX_AGE_DEPB <- PT_SEX_AGE_DEPB_DF$avg_sex_age

## BMI_CK
PT_SEX_AGE_BMI_DF <- full_data_age_sex_grouped_df %>%
    summarise(avg_sex_age = mean(bmi_kgm2))
PT_SEX_AGE_BMI <- PT_SEX_AGE_BMI_DF$avg_sex_age

## Waist_CK
PT_SEX_AGE_WAIST_DF <- full_data_age_sex_grouped_df %>%
    summarise(avg_sex_age = mean(waistcirc_inches_1))
PT_SEX_AGE_WAIST <- PT_SEX_AGE_WAIST_DF$avg_sex_age

## OvHea_CK
PT_SEX_AGE_OVHEA_DF <- full_data_age_sex_grouped_df %>%
    summarise(avg_sex_age = mean(sf_1))
PT_SEX_AGE_OVHEA <- PT_SEX_AGE_OVHEA_DF$avg_sex_age

## RunD_CK
PT_SEX_AGE_RUND_DF <- full_data_age_sex_grouped_df %>%
    summarise(avg_sex_age = mean(fatexp41))
PT_SEX_AGE_RUND <- PT_SEX_AGE_RUND_DF$avg_sex_age

## RiskBP_CK1
## RiskBP_CK2
#### correct answer is 1 only
## RiskBMI_CK
## Panel_CK
## Clover_CK

