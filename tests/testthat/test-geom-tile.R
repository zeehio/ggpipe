context("geom_tile")

test_that("geom_tile gives equal results", {
  df <- data.frame(x = c("a", "b"), y = c("a", "b"))
  p1 <- ggplot(df, aes(x, y)) %>% geom_tile()
  p2 <- ggplot2::ggplot(df, aes(x, y)) + ggplot2::geom_tile()
  expect_equal(p1, p2)
  p1 <- ggplot(df, aes(x, y)) %>% geom_tile(width = 0.5, height = 0.5)
  p2 <- ggplot2::ggplot(df, aes(x, y)) + ggplot2::geom_tile(width = 0.5, height = 0.5)
  expect_equal(p1, p2)
})
