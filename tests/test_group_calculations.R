library(testthat)

rm(list = ls())

source('src/helper_group_calculations.R')

test_that('Recode age group', {
    expect_equal(calculate_age_group(17), NULL)

    expect_equal(calculate_age_group(18), "18-30")
    expect_equal(calculate_age_group(24), "18-30")
    expect_equal(calculate_age_group(30), "18-30")

    expect_equal(calculate_age_group(31), "31-50")
    expect_equal(calculate_age_group(42), "31-50")
    expect_equal(calculate_age_group(50), "31-50")

    expect_equal(calculate_age_group(51), "51-64")
    expect_equal(calculate_age_group(55), "51-64")
    expect_equal(calculate_age_group(64), "51-64")

    expect_equal(calculate_age_group(65), "65+")
    expect_equal(calculate_age_group(99), "65+")
    expect_equal(calculate_age_group(999), "65+")
})

# Recommendation: 2 for men and for women 18-30; 1.5 for women >30
test_that('recommended fruit servings', {
    expect_equal(calculate_fruit_servings_recommended(pt_sex = 'M', pt_age = 18), 2)
    expect_equal(calculate_fruit_servings_recommended(pt_sex = 'M', pt_age = 31), 2)
    expect_equal(calculate_fruit_servings_recommended(pt_sex = 'M', pt_age = 51), 2)
    expect_equal(calculate_fruit_servings_recommended(pt_sex = 'M', pt_age = 65), 2)

    expect_equal(calculate_fruit_servings_recommended(pt_sex = 'F', pt_age = 18), 2)
    expect_equal(calculate_fruit_servings_recommended(pt_sex = 'F', pt_age = 24), 2)
    expect_equal(calculate_fruit_servings_recommended(pt_sex = 'F', pt_age = 30), 2)

    expect_equal(calculate_fruit_servings_recommended(pt_sex = 'F', pt_age = 31), 1.5)
    expect_equal(calculate_fruit_servings_recommended(pt_sex = 'F', pt_age = 42), 1.5)
    expect_equal(calculate_fruit_servings_recommended(pt_sex = 'F', pt_age = 65), 1.5)
})

# Recommendation: 3 for men <50; 2.5 for men >50; 2.5 for women <50; 2 for
# women >50.
test_that('calculate_vegetable_servings_recommended', {
    expect_equal(calculate_vegetable_servings_recommended(pt_sex = 'M', pt_age = 17), 3) # invalid age
    expect_equal(calculate_vegetable_servings_recommended(pt_sex = 'M', pt_age = 49), 3)
    expect_equal(calculate_vegetable_servings_recommended(pt_sex = 'M', pt_age = 50), 2.5)
    expect_equal(calculate_vegetable_servings_recommended(pt_sex = 'M', pt_age = 99), 2.5)

    expect_equal(calculate_vegetable_servings_recommended(pt_sex = 'F', pt_age = 17), 2.5)
    expect_equal(calculate_vegetable_servings_recommended(pt_sex = 'F', pt_age = 49), 2.5)
    expect_equal(calculate_vegetable_servings_recommended(pt_sex = 'F', pt_age = 50), 2)
    expect_equal(calculate_vegetable_servings_recommended(pt_sex = 'F', pt_age = 99), 2)
})

# Recommendation: 75 minutes
test_that('calculate_vigorous_exercise_recommended', {
    expect_equal(calculate_vigorous_exercise_recommended('M', 42), 75)
    expect_equal(calculate_vigorous_exercise_recommended('F', 42), 75)
})

# Recommendation: 150 minutes
test_that('calculate_moderate_exercise_recommended', {
    expect_equal(calculate_moderate_exercise_recommended(pt_sex = 'M', pt_age = 42), 150)
    expect_equal(calculate_moderate_exercise_recommended(pt_sex = 'F', pt_age = 42), 150)
})

# Very Low = 0-4; Low = 5-9; Moderate = 10-14; High = 15-19; Very High = 20-
# 27
test_that('calculate_depression_gauge', {
    expect_equal(calculate_depression_gauge(depression_value = 0), 1) # very low
    expect_equal(calculate_depression_gauge(depression_value = 3), 1) # very low
    expect_equal(calculate_depression_gauge(depression_value = 4), 1) # very low

    expect_equal(calculate_depression_gauge(depression_value = 5), 2)
    expect_equal(calculate_depression_gauge(depression_value = 7), 2)
    expect_equal(calculate_depression_gauge(depression_value = 9), 2)

    expect_equal(calculate_depression_gauge(depression_value = 10), 3)
    expect_equal(calculate_depression_gauge(depression_value = 12), 3)
    expect_equal(calculate_depression_gauge(depression_value = 14), 3)

    expect_equal(calculate_depression_gauge(depression_value = 15), 4)
    expect_equal(calculate_depression_gauge(depression_value = 17), 4)
    expect_equal(calculate_depression_gauge(depression_value = 19), 4)

    expect_equal(calculate_depression_gauge(depression_value = 20), 5)
    expect_equal(calculate_depression_gauge(depression_value = 24), 5)
    expect_equal(calculate_depression_gauge(depression_value = 27), 5)

    expect_equal(calculate_depression_gauge(depression_value = 28), NA)
})

# Very Low = 0-1; Low = 2; Moderate = 3; High = 4; Very High = 5
test_that('calculate_stress_gauge', {
    expect_equal(calculate_stress_gauge(stress_value = 0), 1)
    expect_equal(calculate_stress_gauge(stress_value = 1), 1)
    expect_equal(calculate_stress_gauge(stress_value = 2), 2)
    expect_equal(calculate_stress_gauge(stress_value = 3), 3)
    expect_equal(calculate_stress_gauge(stress_value = 4), 4)
    expect_equal(calculate_stress_gauge(stress_value = 5), 5)
    expect_equal(calculate_stress_gauge(stress_value = 6), NA)
})

# <18.5 is underweight; 18.5-24.9 normal; 25-29.9 overweight; >30.0 obese
test_that('calculate_pt_bmi_cat', {
    expect_equal(calculate_pt_bmi_cat(18), 1)
    expect_equal(calculate_pt_bmi_cat(18.5), 2)
    expect_equal(calculate_pt_bmi_cat(24.9), 2)
    expect_equal(calculate_pt_bmi_cat(25), 3)
    expect_equal(calculate_pt_bmi_cat(29.9), 3)
    expect_equal(calculate_pt_bmi_cat(30), 4)
    expect_equal(calculate_pt_bmi_cat(42), 4)
})

# Female: <35 is lower risk to heart health; ≥35 higher risk to heart health
# Male: <40 is lower risk to heart health; ≥40 higher risk to heart health
test_that('calculate_pt_waist_in_cat', {
    expect_equal(calculate_pt_waist_in_cat(pt_waist_in = 30, pt_sex = 'F'), 1)
    expect_equal(calculate_pt_waist_in_cat(pt_waist_in = 35, pt_sex = 'F'), 2)
    expect_equal(calculate_pt_waist_in_cat(pt_waist_in = 42, pt_sex = 'F'), 2)

    expect_equal(calculate_pt_waist_in_cat(pt_waist_in = 39, pt_sex = 'M'), 1)
    expect_equal(calculate_pt_waist_in_cat(pt_waist_in = 40, pt_sex = 'M'), 2)
    expect_equal(calculate_pt_waist_in_cat(pt_waist_in = 42, pt_sex = 'M'), 2)
})

# Systolic ≤. 120, diastolic ≤ 80 = healthy. 120 < systolic < 140, 80 < diastolic <90
# = borderline. Systolic ≥ 140 or diastolic ≥ 90 = high
test_that('calculate_pt_bp_s_cat', {
    expect_equal(calculate_pt_bp_s_cat(pt_bp_s = 110), 1)
    expect_equal(calculate_pt_bp_s_cat(pt_bp_s = 120), 1)
    expect_equal(calculate_pt_bp_s_cat(pt_bp_s = 121), 2)
    expect_equal(calculate_pt_bp_s_cat(pt_bp_s = 139), 2)
    expect_equal(calculate_pt_bp_s_cat(pt_bp_s = 140), 3)
})

test_that('calculate_pt_bp_d_cat', {
    expect_equal(calculate_pt_bp_d_cat(75), 1)
    expect_equal(calculate_pt_bp_d_cat(80), 1)
    expect_equal(calculate_pt_bp_d_cat(89), 2)
    expect_equal(calculate_pt_bp_d_cat(90), 3)
    expect_equal(calculate_pt_bp_d_cat(95), 3)
})

test_that('is_between', {
    expect_true(is_between(3, 1, 5))
    expect_false(is_between(3, 3, 3))
    expect_false(is_between(3, 2, 3))
    expect_false(is_between(3, 3, 5))
})

test_that('get_health_summary_colors', {
    ## 1 from prolonged stress
    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 1, bmi_value = 42,   oral_health_value = 1, blood_pressure_s_value = 120, blood_pressure_d_value = 80), 1)
    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 2, bmi_value = 18.5, oral_health_value = 1, blood_pressure_s_value = 120, blood_pressure_d_value = 80), 1)
    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 1, bmi_value = 24.9, oral_health_value = 1, blood_pressure_s_value = 120, blood_pressure_d_value = 80), 1)
    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 2, bmi_value = 30,   oral_health_value = 1, blood_pressure_s_value = 120, blood_pressure_d_value = 80), 1)

    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 1, bmi_value = 42,   oral_health_value = 1, blood_pressure_s_value = 120, blood_pressure_d_value = 80), 1)
    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 2, bmi_value = 18.5, oral_health_value = 2, blood_pressure_s_value = 120, blood_pressure_d_value = 80), 1)
    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 1, bmi_value = 24.9, oral_health_value = 4, blood_pressure_s_value = 120, blood_pressure_d_value = 80), 1)
    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 2, bmi_value = 30,   oral_health_value = 2, blood_pressure_s_value = 120, blood_pressure_d_value = 80), 1)

    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 1, bmi_value = 42,   oral_health_value = 1, blood_pressure_s_value = 120, blood_pressure_d_value = 80), 1)
    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 2, bmi_value = 18.5, oral_health_value = 2, blood_pressure_s_value = 140, blood_pressure_d_value = 80), 1)
    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 1, bmi_value = 24.9, oral_health_value = 4, blood_pressure_s_value = 100, blood_pressure_d_value = 80), 1)
    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 2, bmi_value = 30,   oral_health_value = 2, blood_pressure_s_value = 160, blood_pressure_d_value = 80), 1)

    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 1, bmi_value = 42,   oral_health_value = 1, blood_pressure_s_value = 120, blood_pressure_d_value = 80), 1)
    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 2, bmi_value = 18.5, oral_health_value = 2, blood_pressure_s_value = 140, blood_pressure_d_value = 90), 1)
    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 1, bmi_value = 24.9, oral_health_value = 4, blood_pressure_s_value = 100, blood_pressure_d_value = 70), 1)
    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 2, bmi_value = 30,   oral_health_value = 2, blood_pressure_s_value = 160, blood_pressure_d_value = 100), 1)

    ## 1 from bmi
    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 0, bmi_value = 18,   oral_health_value = 1, blood_pressure_s_value = 120, blood_pressure_d_value = 80), 1)
    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 3, bmi_value = 18.4, oral_health_value = 1, blood_pressure_s_value = 120, blood_pressure_d_value = 80), 1)
    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 3, bmi_value = 25,   oral_health_value = 1, blood_pressure_s_value = 120, blood_pressure_d_value = 80), 1)
    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 4, bmi_value = 29.9, oral_health_value = 1, blood_pressure_s_value = 120, blood_pressure_d_value = 80), 1)
    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 5, bmi_value = 29.9, oral_health_value = 1, blood_pressure_s_value = 120, blood_pressure_d_value = 80), 1)

    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 5, bmi_value = 18,   oral_health_value = 1, blood_pressure_s_value = 120, blood_pressure_d_value = 80), 1)
    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 0, bmi_value = 18.4, oral_health_value = 2, blood_pressure_s_value = 120, blood_pressure_d_value = 80), 1)
    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 3, bmi_value = 25,   oral_health_value = 4, blood_pressure_s_value = 120, blood_pressure_d_value = 80), 1)
    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 3, bmi_value = 29.9, oral_health_value = 2, blood_pressure_s_value = 120, blood_pressure_d_value = 80), 1)

    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 4, bmi_value = 18,   oral_health_value = 1, blood_pressure_s_value = 120, blood_pressure_d_value = 80), 1)
    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 5, bmi_value = 18.4, oral_health_value = 2, blood_pressure_s_value = 140, blood_pressure_d_value = 80), 1)
    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 0, bmi_value = 29.9, oral_health_value = 4, blood_pressure_s_value = 100, blood_pressure_d_value = 80), 1)
    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 3, bmi_value = 25,   oral_health_value = 2, blood_pressure_s_value = 160, blood_pressure_d_value = 80), 1)

    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 3, bmi_value = 18,   oral_health_value = 1, blood_pressure_s_value = 120, blood_pressure_d_value = 80), 1)
    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 4, bmi_value = 18.4, oral_health_value = 2, blood_pressure_s_value = 140, blood_pressure_d_value = 90), 1)
    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 5, bmi_value = 25,   oral_health_value = 4, blood_pressure_s_value = 100, blood_pressure_d_value = 70), 1)
    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 0, bmi_value = 29.9, oral_health_value = 2, blood_pressure_s_value = 160, blood_pressure_d_value = 100), 1)

    ## 1 from oral health
    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 0, bmi_value = 42,   oral_health_value = 3, blood_pressure_s_value = 120, blood_pressure_d_value = 80), 1)
    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 3, bmi_value = 18.5, oral_health_value = 3, blood_pressure_s_value = 120, blood_pressure_d_value = 80), 1)
    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 3, bmi_value = 24.9, oral_health_value = 3, blood_pressure_s_value = 120, blood_pressure_d_value = 80), 1)
    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 4, bmi_value = 30,   oral_health_value = 3, blood_pressure_s_value = 120, blood_pressure_d_value = 80), 1)

    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 3, bmi_value = 42,   oral_health_value = 3, blood_pressure_s_value = 120, blood_pressure_d_value = 80), 1)
    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 3, bmi_value = 18.5, oral_health_value = 3, blood_pressure_s_value = 120, blood_pressure_d_value = 80), 1)
    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 4, bmi_value = 24.9, oral_health_value = 3, blood_pressure_s_value = 120, blood_pressure_d_value = 80), 1)
    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 5, bmi_value = 30,   oral_health_value = 3, blood_pressure_s_value = 120, blood_pressure_d_value = 80), 1)

    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 0, bmi_value = 42,   oral_health_value = 3, blood_pressure_s_value = 120, blood_pressure_d_value = 80), 1)
    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 3, bmi_value = 18.5, oral_health_value = 3, blood_pressure_s_value = 140, blood_pressure_d_value = 80), 1)
    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 3, bmi_value = 24.9, oral_health_value = 3, blood_pressure_s_value = 100, blood_pressure_d_value = 80), 1)
    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 4, bmi_value = 30,   oral_health_value = 3, blood_pressure_s_value = 160, blood_pressure_d_value = 80), 1)

    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 3, bmi_value = 42,   oral_health_value = 3, blood_pressure_s_value = 120, blood_pressure_d_value = 80), 1)
    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 3, bmi_value = 18.5, oral_health_value = 3, blood_pressure_s_value = 140, blood_pressure_d_value = 90), 1)
    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 4, bmi_value = 24.9, oral_health_value = 3, blood_pressure_s_value = 100, blood_pressure_d_value = 70), 1)
    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 5, bmi_value = 30,   oral_health_value = 3, blood_pressure_s_value = 160, blood_pressure_d_value = 100), 1)

    ## 1 from bp_s
    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 0, bmi_value = 42,   oral_health_value = 1, blood_pressure_s_value = 121, blood_pressure_d_value = 80), 1)
    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 3, bmi_value = 18.5, oral_health_value = 2, blood_pressure_s_value = 139, blood_pressure_d_value = 90), 1)
    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 3, bmi_value = 24.9, oral_health_value = 4, blood_pressure_s_value = 122, blood_pressure_d_value = 70), 1)
    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 4, bmi_value = 30,   oral_health_value = 2, blood_pressure_s_value = 138, blood_pressure_d_value = 100), 1)

    ## 1 from bp_d
    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 0, bmi_value = 42,   oral_health_value = 1, blood_pressure_s_value = 120, blood_pressure_d_value = 81), 1)
    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 3, bmi_value = 18.5, oral_health_value = 2, blood_pressure_s_value = 140, blood_pressure_d_value = 89), 1)
    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 3, bmi_value = 24.9, oral_health_value = 4, blood_pressure_s_value = 100, blood_pressure_d_value = 82), 1)
    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 4, bmi_value = 30,   oral_health_value = 2, blood_pressure_s_value = 160, blood_pressure_d_value = 88), 1)

    ## 4
    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 1, bmi_value = 18.4, oral_health_value = 3, blood_pressure_s_value = 121, blood_pressure_d_value = 81), 4)
    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 2, bmi_value = 29.9, oral_health_value = 3, blood_pressure_s_value = 139, blood_pressure_d_value = 89), 4)

    ## 3
    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 0, bmi_value = 18.4, oral_health_value = 3, blood_pressure_s_value = 121, blood_pressure_d_value = 81), 3)
    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 5, bmi_value = 18.4, oral_health_value = 3, blood_pressure_s_value = 121, blood_pressure_d_value = 81), 3)

    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 1, bmi_value = 18.5, oral_health_value = 3, blood_pressure_s_value = 121, blood_pressure_d_value = 81), 3)
    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 2, bmi_value = 30,   oral_health_value = 3, blood_pressure_s_value = 121, blood_pressure_d_value = 81), 3)

    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 1, bmi_value = 18.4, oral_health_value = 2, blood_pressure_s_value = 121, blood_pressure_d_value = 81), 3)
    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 2, bmi_value = 18.4, oral_health_value = 4, blood_pressure_s_value = 121, blood_pressure_d_value = 81), 3)

    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 1, bmi_value = 18.4, oral_health_value = 3, blood_pressure_s_value = 120, blood_pressure_d_value = 80), 3)
    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 2, bmi_value = 18.4, oral_health_value = 3, blood_pressure_s_value = 140, blood_pressure_d_value = 90), 3)

    ## 2
    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 3, bmi_value = 18.5, oral_health_value = 3, blood_pressure_s_value = 121, blood_pressure_d_value = 81), 2)
    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 5, bmi_value = 24.9, oral_health_value = 3, blood_pressure_s_value = 121, blood_pressure_d_value = 81), 2)
    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 3, bmi_value = 18.4, oral_health_value = 2, blood_pressure_s_value = 121, blood_pressure_d_value = 81), 2)
    expect_equal(get_health_summary_colors_yellow(prolonged_stress_value = 4, bmi_value = 18.4, oral_health_value = 3, blood_pressure_s_value = 120, blood_pressure_d_value = 80), 2)
})

test_that('calculate_cloverleaf_fruit_value_excellent', {
    expect_equal(calculate_cloverleaf_fruit_value_excellent(fruit_value = 3, vegetable_value = 3, pt_sex = 'M', pt_age = 18), 'Excellent')
    expect_equal(calculate_cloverleaf_fruit_value_excellent(fruit_value = 2, vegetable_value = 3, pt_sex = 'F', pt_age = 31), 'Excellent')

    expect_equal(calculate_cloverleaf_fruit_value_excellent(fruit_value = 2, vegetable_value = 2, pt_sex = 'M', pt_age = 18), NA)
    expect_equal(calculate_cloverleaf_fruit_value_excellent(fruit_value = 1, vegetable_value = 2, pt_sex = 'F', pt_age = 31), NA)
})

test_that('calculate_cloverleaf_mental_value_excellent', {
    expect_equal(calculate_cloverleaf_mental_value_excellent(mental_value = 0), 'Excellent')
    expect_equal(calculate_cloverleaf_mental_value_excellent(mental_value = 1), NA)
})

test_that('calculate_cloverleaf_overall_value_excellent', {
    expect_equal(calculate_cloverleaf_overall_value_excellent(overall_value = 5), 'Excellent') ## TODO SHOULD BE 1
    expect_equal(calculate_cloverleaf_overall_value_excellent(overall_value = 2), NA)
})

test_that('calculate_cloverleaf_physical_value_excellent', {
    expect_equal(calculate_cloverleaf_physical_value_excellent(moderate_value = 10, vigorous_value = 70), 'Excellent')
    expect_equal(calculate_cloverleaf_physical_value_excellent(moderate_value = 150, vigorous_value = 0), 'Excellent')
    expect_equal(calculate_cloverleaf_physical_value_excellent(moderate_value = 10, vigorous_value = 10), NA)
})

test_that('calculate_cloverleaf_ck', {
    expect_equal(calculate_cloverleaf_ck_excellent(fruit = 'Excellent', physical = NA, overall = NA, mental = NA), 1)
    expect_equal(calculate_cloverleaf_ck_excellent(fruit = NA, physical = 'Excellent', overall = NA, mental = NA), 1)
    expect_equal(calculate_cloverleaf_ck_excellent(fruit = NA, physical = NA, overall = 'Excellent', mental = NA), 1)
    expect_equal(calculate_cloverleaf_ck_excellent(fruit = NA, physical = NA, overall = NA, mental = 'Excellent'), 1)

    expect_equal(calculate_cloverleaf_ck_excellent(fruit = 'Excellent', physical = 'Excellent', overall = NA, mental = NA), 2)
    expect_equal(calculate_cloverleaf_ck_excellent(fruit = 'Excellent', physical = NA, overall = 'Excellent', mental = NA), 2)
    expect_equal(calculate_cloverleaf_ck_excellent(fruit = 'Excellent', physical = NA, overall = NA, mental = 'Excellent'), 2)

    expect_equal(calculate_cloverleaf_ck_excellent(fruit = NA, physical = 'Excellent', overall = 'Excellent', mental = NA), 2)
    expect_equal(calculate_cloverleaf_ck_excellent(fruit = NA, physical = 'Excellent', overall = NA, mental = 'Excellent'), 2)

    expect_equal(calculate_cloverleaf_ck_excellent(fruit = NA, physical = 'Excellent', overall = 'Excellent', mental = 'Excellent'), 3)

    expect_equal(calculate_cloverleaf_ck_excellent(fruit = 'Excellent', physical = 'Excellent', overall = 'Excellent', mental = 'Excellent'), 4)
})
