library(tibble)

df <- tibble(
  Date = "2020-07-04",
  Confirmed = 14:20,
  Deaths = 0:6,
  Recovered = 1:7
)

test_that("expect sum of vector", {
  expect_equal(sum(14:20),
               get_case_count(data_frame = df, date = "2020-07-04", case_type = "Confirmed"))
})
