values_vector_input <- c(37,42,75,29,46,85)
text_input <- "Confirmed Cases"
ui_valuebox <- generate_value_box(counts_vector=values_vector, text=text_input, icon="head-side-mask", color="green")

test_that("expect sum of vector", {
  expect_equal(sum(values_vector_input),
               ui_valuebox$children[[1]]$children[[1]]$children[[1]]$children[[1]]
  )
})


test_that("expect matching text", {
  expect_match(text_input,
               ui_valuebox$children[[1]]$children[[1]]$children[[2]]$children[[1]]
  )
})