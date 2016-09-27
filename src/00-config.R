library(readxl)
library(testthat)
library(dplyr)
library(stringr)
rm(list = ls())
source('src/helper_group_calculations.R')
source('src/helper_ck.R')
###############################################################################



#
# Just need to change the file directory here
#
###############################################################################
#
FULL_DATA <- '../test_data/WICER comprehension test.xlsx'
# FULL_DATA <- 'data/Dummy infographics data.xlsx'
#
###############################################################################




#
# DO NOT EDIT BELOW THIS LINE
#

FULL_DATA_DF <- read_excel(FULL_DATA)
FULL_DATA_DF <- FULL_DATA_DF[complete.cases(FULL_DATA_DF), ]
all_ids <- FULL_DATA_DF[, 1]
CK_PATTERN <- "_CK.?$"

for (PT_ID in as.data.frame(all_ids)[, 1]) {
    # PT_ID <- "Sample6"
    print(PT_ID)
    source('src/01-setup.R')
    source('src/02-correct_values.R')
    source('src/03-write_single_answers.R')
}
source('src/04-stack_ck.R')
