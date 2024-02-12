library(testthat)
library(dplyr)

test_that("tetracorr() gives correct errors", {
  ############################## TEST TOY DATASET ###############################
  # load datasets
  data(trt1)
  data(trt2)
  data(trt3)
  vars <- c("trt1","trt2","trt3")

  # merge datasets
  data <- merge(
    dplyr::select(trt1, markname, pval),
    dplyr::select(trt2, markname, pval),
    by='markname',
    all = TRUE
  ) %>% merge(
    dplyr::select(trt3, markname, pval),
    by='markname',
    all = TRUE
  )
  colnames(data)[-1] = vars

  expect_error(tetracorr())
  expect_error(tetracorr(data))
  expect_error(tetracorr(data[,-1], vars))
})
