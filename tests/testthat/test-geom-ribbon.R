context("geom_ribbon")

test_that("geom_ribbon same results", {
  df <- data.frame(x = 1:5, y = c(1, 1, NA, 1, 1))

  p1 <- ggplot2::ggplot(df, ggplot2::aes(x)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = y - 1, ymax = y + 1))

  p2 <- ggplot(df, aes(x)) %>%
    geom_ribbon(aes(ymin = y - 1, ymax = y + 1))

  expect_equal(p1, p2)
})
