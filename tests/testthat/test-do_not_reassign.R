test_that("multiplication works", {
  dir <- tempdir()
  set_save_folder(dir)

  a %<~% 1
  a %<~% 2
  expect_true(a == 1)
  .rename(a, "b")
  expect_true(b == 1)
  expect_false(exists("a"))
  a %<~% 2
  expect_true(a == 2)
  rm(a)
  expect_false(exists("a"))
  a %<~% 3
  expect_true(a == 2)
  .delete(a)
  a %<~% 3
  expect_true(a == 3)
  c <- 1
  c %<~% 2
  expect_true(c == 1)
  rm(c)
  c %<~% 2
  expect_true(c == 2)

  expect_true(get_runtime(a) < 0.01)
  d %<~% {Sys.sleep(1); 1}
  expect_true(abs(get_runtime(d) - 1) < 0.1)
  expect_s3_class(get_runtime(a), "difftime")
})
