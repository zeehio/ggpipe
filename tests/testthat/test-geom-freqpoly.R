context("freqpoly")

test_that("can do frequency polygon with categorical x", {
  df <- data.frame(x = rep(letters[1:3], 3:1))

  p1 <- ggplot(df, aes(x)) + geom_freqpoly(stat = "count")
  p2 <- ggplot2::ggplot(df, ggplot2::aes(x)) + ggplot2::geom_freqpoly(stat = "count")
  expect_equal(p1, p2)
})
