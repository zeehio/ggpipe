context("Adding plot elements")

test_that("aes gives the same in both APIs", {
  p <- ggplot(mtcars) + aes(wt, mpg)
  p2 <- ggplot(mtcars) %>% aes(wt, mpg)
  p3 <- aes(ggplot(mtcars), wt, mpg)
  expect_equal(p, p2)
  expect_equal(p2, p3)
})
