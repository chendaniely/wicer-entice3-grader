library(readxl)
library(testthat)
library(dplyr)
library(stringr)
rm(list = ls())

source('src/helper_group_calculations.R')
source('src/helper_ck.R')

FULL_DATA <- 'data/Dummy infographics data.xlsx'

FULL_DATA_DF <- read_excel(FULL_DATA)
all_ids <- FULL_DATA_DF[, 1]
CK_PATTERN <- "_CK.?$"

PT_ID <- 'Sample1'
source('src/01-setup.R')
source('src/02-correct_values.R')

#
# 01-setup
#
expect_equal(PT_SEX_AGE_FRUIT, 0.8)
expect_equal(PT_SEX_AGE_VEG, 0.3)
expect_equal(PT_SEX_AGE_VEG, 0.3)
expect_equal(PT_SEX_AGE_VIGPA, 10)
expect_equal(PT_SEX_AGE_MODPA, 35)
expect_equal(PT_SEX_AGE_30DEP, 5)
expect_equal(PT_SEX_AGE_30ANX, 5)
expect_equal(PT_SEX_AGE_BEVWK, 0)
expect_equal(PT_SEX_AGE_30COMP, 3)
expect_equal(PT_SEX_AGE_RECPA, 35)
expect_equal(PT_SEX_AGE_BEVREC, 5)
expect_equal(PT_SEX_AGE_DEPA, 27)
expect_equal(PT_SEX_AGE_STRESS, 5)
expect_equal(PT_SEX_AGE_ANX, 82)
expect_equal(PT_SEX_AGE_DEPB, 79)
expect_equal(PT_SEX_AGE_BMI, 14.6)
expect_equal(PT_SEX_AGE_WAIST, 34)
expect_equal(PT_SEX_AGE_OVHEA, 1)
expect_equal(PT_SEX_AGE_RUND, 5)
