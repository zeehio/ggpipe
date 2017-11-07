context("Scales")

test_that("buidling a plot does not affect its scales", {
  dat <- data.frame(x = rnorm(20), y = rnorm(20))

  p <- ggplot(dat, aes(x, y)) + geom_point()

  p2 <- ggplot(dat, aes(x, y)) %>% geom_point()
  expect_equal(p, p2)
})

test_that("ranges update only for variables listed in aesthetics", {
  sc <- scale_alpha()
  sc2 <- ggplot2::scale_alpha()
  expect_equal(sc, sc2)
})

test_that("mapping works", {
  sc <- scale_alpha(range = c(0, 1), na.value = 0)
  sc2 <- ggplot2::scale_alpha(range = c(0, 1), na.value = 0)
  expect_equal(sc, sc2)
})

test_that("both + and pipe give the same result", {
  df <- data.frame(x = 1:3, z = letters[1:3])

  p0 <- ggplot2::ggplot(df,
                        ggplot2::aes(x, z, colour = z, fill = z, shape = z,
                                     size = x, alpha = x)) +
    ggplot2::geom_point() +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_shape_identity() +
    ggplot2::scale_size_identity() +
    ggplot2::scale_alpha_identity()


  p1 <- ggplot(df,
    aes(x, z, colour = z, fill = z, shape = z, size = x, alpha = x)) +
    geom_point() +
    scale_colour_identity() +
    scale_fill_identity() +
    scale_shape_identity() +
    scale_size_identity() +
    scale_alpha_identity()

  p2 <- ggplot(df,
               aes(x, z, colour = z, fill = z, shape = z, size = x, alpha = x)) %>%
    geom_point() %>%
    scale_colour_identity() %>%
    scale_fill_identity() %>%
    scale_shape_identity() %>%
    scale_size_identity() %>%
    scale_alpha_identity()

  expect_equal(p0, p1)
  expect_equal(p1, p2)

})

test_that("position scales updated by all position aesthetics", {
  df <- data.frame(x = 1:3, y = 1:3)

  aesthetics <- list(
    aes(xend = x, yend = x),
    aes(xmin = x, ymin = x),
    aes(xmax = x, ymax = x),
    aes(xintercept = x, yintercept = y)
  )

  base <- ggplot(df, aes(x = 1, y = 1)) + geom_point()
  plots <- lapply(aesthetics, function(x) base %+% x)
  ranges <- lapply(plots, pranges)

  lapply(ranges, function(range) {
    expect_equal(range$x[[1]], c(1, 3))
    expect_equal(range$y[[1]], c(1, 3))
  })

})

