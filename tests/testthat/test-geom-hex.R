context("geom_hex")

test_that("geom_hex same results", {
  df <- data.frame(x = c(1, 1, 1, 2), y = c(1, 1, 1, 2))
  base1 <- ggplot(df, aes(x, y)) %>% geom_hex()
  base2 <- ggplot2::ggplot(df, ggplot2::aes(x, y)) + ggplot2::geom_hex()
  expect_equal(base1, base2)
})
