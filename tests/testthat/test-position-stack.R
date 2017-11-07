context("position-stack")

test_that("geom_area same result", {
  df <- data.frame(
    x = rep(c(1:10), 3),
    var = rep(c("a", "b", "c"), 10),
    y = round(runif(30, 1, 5))
  )
  p1 <- ggplot(df, aes(x = x, y = y, fill = var)) %>%
    geom_area(position = "stack")
  p2 <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, fill = var)) +
    ggplot2::geom_area(position = "stack")

  expect_equal(p1, p2)
})

test_that("negative and positive values are handled separately", {
  df <- data.frame(
    x = c(1,1,1,2,2),
    g = c(1,2,3,1,2),
    y = c(1,-1,1,2,-3)
  )
  p1 <- ggplot(df, aes(x, y, fill = factor(g))) %>% geom_col()
  p2 <- ggplot2::ggplot(df, ggplot2::aes(x, y, fill = factor(g))) + ggplot2::geom_col()
  expect_equal(p1, p2)
})

test_that("can request reverse stacking", {
  df <- data.frame(
    y = c(-2, 2, -1, 1),
    g = c("a", "a", "b", "b")
  )
  p1 <- ggplot2::ggplot(df, ggplot2::aes(1, y, fill = g)) +
    ggplot2::geom_col(position = position_stack(reverse = TRUE))
  p2 <- ggplot(df, aes(1, y, fill = g)) %>%
    geom_col(position = position_stack(reverse = TRUE))
  expect_equal(p1, p2)
})

test_that("data with no extent is stacked correctly", {
  df = data.frame(
    x = c(1, 1),
    y = c(-40, -75),
    group = letters[1:2]
  )
  base <- ggplot(df, aes(x, y, group = group))
  p0 <- base + ggplot2::geom_text(aes(label = y), position = position_stack(vjust = 0))
  p0b <- base %>% geom_text(aes(label = y), position = position_stack(vjust = 0))
  expect_equal(p0, p0b)
  p1 <- base + ggplot2::geom_text(aes(label = y), position = position_stack(vjust = 1))
  p1b <- base %>% geom_text(aes(label = y), position = position_stack(vjust = 1))
  expect_equal(p1, p1b)
})
