context("Aes - in functions")

test_that("aes inside functions", {
  df <- data.frame(x = 1:5, y = 1:5)
  p <- ggplot(df, aes(x, y))

  set_colours1 <- function(colours) {
    layer_data(p + geom_point(colour = colours))
  }

  set_colours2 <- function(colours) {
    layer_data(p %>% geom_point(colour = colours))
  }

  expect_equal(set_colours1("red"), set_colours2("red"))
})
