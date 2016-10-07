library(testthat)
library(readxl)
library(dplyr)

source('src/helper_group_calculations.R')
source('src/helper_ck.R')

testthat::test_that("Grouped Sex Age Averages", {
    FULL_DATA <- 'data/dummy_sex_age.xlsx'

    FULL_DATA_DF <- read_excel(FULL_DATA)
    all_ids <- FULL_DATA_DF[, 1]
    CK_PATTERN <- "_CK.?$"

    PT_ID <- 'Sample1'
    source('src/01-setup.R')
    source('src/02-correct_values.R')

    #
    # 01-setup
    #
    expect_equal(PT_SEX_AGE_FRUIT, 0.33, tolerance = 0.01)
    expect_equal(PT_SEX_AGE_VEG, 0.6, tolerance = 0.01)
    expect_equal(PT_SEX_AGE_VIGPA, 4)
    expect_equal(PT_SEX_AGE_MODPA, 1.67, tolerance = 0.01)
    expect_equal(PT_SEX_AGE_30DEP, 0.67, tolerance = 0.01)
    expect_equal(PT_SEX_AGE_30ANX, 0.67, tolerance = 0.01)
    expect_equal(PT_SEX_AGE_BEVWK, 2)
    expect_equal(PT_SEX_AGE_30COMP, 2.33, tolerance = 0.01)
    expect_equal(PT_SEX_AGE_RECPA, 1.67, tolerance = 0.01)
    expect_equal(PT_SEX_AGE_BEVREC, 1.67, tolerance = 0.01)
    expect_equal(PT_SEX_AGE_DEPA, 3.67, tolerance = 0.01)
    expect_equal(PT_SEX_AGE_STRESS, 1)
    expect_equal(PT_SEX_AGE_ANX, 53.167, tolerance = 0.01)
    expect_equal(PT_SEX_AGE_DEPB, 54.73, tolerance = 0.01)
    expect_equal(PT_SEX_AGE_BMI, 21.8)
    expect_equal(PT_SEX_AGE_WAIST, 37.67, tolerance = 0.01)
    expect_equal(PT_SEX_AGE_OVHEA, 3.33, tolerance = 0.01)
    expect_equal(PT_SEX_AGE_RUND, 2.33, tolerance = 0.01)
})
