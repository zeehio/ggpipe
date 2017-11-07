context("Aes mappings")

test_that("aes() captures input expressions", {
  out1 <- aes(mpg, wt + 1)
  out2 <- ggplot2::aes(mpg, wt + 1)
  expect_equal(out1, out2)
})

test_that("aes_q() same results", {
  out1 <- aes_q(quote(mpg), ~ wt + 1)
  out2 <- ggplot2::aes_q(quote(mpg), ~ wt + 1)
  expect_equal(out1, out2)
})

test_that("aes_string() same results", {
  expect_equal(ggplot2::aes_string("a + b")$x, ggpipe::aes_string("a + b")$x)
})

test_that("aes_all() same results", {
  expect_equal(
    ggpipe::aes_all(c("x", "y", "col", "pch")),
    ggplot2::aes_all(c("x", "y", "col", "pch"))
  )
})

test_that("aes evaluated in environment where plot created", {
  df <- data.frame(x = 1, y = 1)
  p1 <- ggplot(df, aes(foo, y)) %>% geom_point()
  p2 <- ggplot(df, aes(foo, y)) %>% geom_point()
  expect_equal(p1, p2)
})
