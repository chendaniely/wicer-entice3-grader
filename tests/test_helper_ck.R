library(testthat)

rm(list = ls())

source('src/helper_ck.R')

testthat::test_that('gle function', {
    expect_equal(gle(1, 1), 2)
    expect_equal(gle(1, 2), 1)
    expect_equal(gle(2, 1), 3)
    expect_true(is.na(gle(NA, 10)))
    expect_true(is.na(gle(10, NA)))
})

test_that('gle_range function', {
    expect_equal(gle_range(1, 5, 3), 1)
    expect_equal(gle_range(5, 1, 3), 3)
    expect_equal(gle_range(7, 5, 3), 2)
    expect_equal(gle_range(8, 5, 3), 2)
    expect_equal(gle_range(2, 5, 3), 2)
    expect_true(is.na(gle_range(NA, 5)))
    expect_true(is.na(gle_range(NA, NA)))
    expect_true(is.na(gle_range(3, NA)))
})

test_that('calculate_panel_clover', {
    expect_equal(calculate_panel_colver_ck(0), 1)
    expect_equal(calculate_panel_colver_ck(1), 2)
    expect_equal(calculate_panel_colver_ck(2), 3)
    expect_equal(calculate_panel_colver_ck(3), 4)
    expect_equal(calculate_panel_colver_ck(4), 5)
    expect_equal(calculate_panel_colver_ck(5), NA)
    expect_true(is.na(calculate_panel_colver_ck(5)))
})
