test_that("plot_framework", {

  # build example coin
  coin <- build_example_coin(up_to = "new_coin", quietly = TRUE)

  # plot framework as sunburst, colouring at level 2 upwards
  plt <- plot_framework(coin, colour_level = 2, transparency = TRUE, type = "sunburst", text_colour = "white")
  expect_s3_class(plt, "ggplot")
  plt <- plot_framework(coin, colour_level = 3, transparency = FALSE, type = "stack", text_colour = "black", text_size = 4)
  expect_s3_class(plt, "ggplot")

})
