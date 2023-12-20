library(ST403climr)

test_that("load_climr produces valid data", {
  expect_error(load_climr("ST403"))
  expect_s3_class(load_climr("NH"), "climr")
  expect_output(str(load_climr("SH")), "List of 3")
  # Here is one that fails (note: expect_error() above is one we KNOW should fail, and it does)
    # expect_output(str(load_climr("SH")), "List of 7")
})
