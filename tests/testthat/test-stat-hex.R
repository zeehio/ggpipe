context("stat-hex")

test_that("stat_binhex gives same results", {
  df <- data.frame(x = c(1, 1, 2), y = c(1, 1, 2))
  p1 <- ggplot(df, aes(x, y)) %>% stat_binhex(binwidth = 1)
  p2 <- ggplot2::ggplot(df, ggplot2::aes(x, y)) + ggplot2::stat_binhex(binwidth = 1)

  expect_equal(p1, p2)
})
