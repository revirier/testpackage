context("running far_functions")

dir=getwd()

setwd("..")

test_that("load file",{
  result<-fars_read("accident_2013.csv.bz2")
  expect_that(result, is_a("data.frame"))
  expect_that(dim(result)[2],equals(50))
  expect_gt(dim(result)[1],0)
})

test_that("summarize year",{
  result<-fars_summarize_years(c("2013"))
  expect_that(result, is_a("data.frame"))
  expect_that(dim(result)[2],equals(2))
  expect_gt(dim(result)[1],0)
})

setwd(dir)
