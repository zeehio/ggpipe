context("Labels")

test_that("give same result", {
  p1 <- ggplot(mtcars) %>% geom_point(aes(mpg, cyl)) %>% xlab("my label")
  p2 <- ggplot2::ggplot(mtcars) + ggplot2::geom_point(aes(mpg, cyl)) + ggplot2::xlab("my label")
  expect_equal(p1, p2)

  p1 <- ggplot(mtcars) %>% geom_point(aes(mpg, cyl)) %>% ylab("my label")
  p2 <- ggplot2::ggplot(mtcars) + ggplot2::geom_point(aes(mpg, cyl)) + ggplot2::ylab("my label")
  expect_equal(p1, p2)

  p1 <- ggplot(mtcars) %>% geom_point(aes(mpg, cyl)) %>% labs(y = "my label")
  p2 <- ggplot2::ggplot(mtcars) + ggplot2::geom_point(aes(mpg, cyl)) + ggplot2::labs(y = "my label")
  expect_equal(p1, p2)

  p1 <- ggplot(mtcars) %>% geom_point(aes(mpg, cyl)) %>% ggtitle("my label")
  p2 <- ggplot2::ggplot(mtcars) + ggplot2::geom_point(aes(mpg, cyl)) + ggplot2::ggtitle("my label")
  expect_equal(p1, p2)

})
