context("geom_boxplot")

test_that("geom_boxplot same results", {
  df <- data.frame(x = 1, y = c(1:5, 100))
  p1 <- ggplot(df, aes(x, y)) %>% geom_boxplot(outlier.color = "red")
  p2 <- ggplot2::ggplot(df, aes(x, y)) + ggplot2::geom_boxplot(outlier.color = "red")
  expect_equal(p1, p2)
})
