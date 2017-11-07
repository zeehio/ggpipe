context("same results")

test_that("get same results", {
  plt1 <- ggplot(mtcars) + geom_point(aes(x = mpg, y = cyl))
  plt2 <- ggplot(mtcars) %>% geom_point(aes(x = mpg, y = cyl))
  expect_equal(plt1, plt2)
})
