context("Facetting")

df <- data.frame(x = 1:3, y = 3:1, z = letters[1:3])

test_that("facets equal results", {
  l0 <- ggplot2::ggplot(df, aes(x, y)) +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(~z)
  l1 <- ggplot(df, aes(x, y)) %>% geom_point() %>% facet_wrap(~z)
  l2 <- ggplot(df, aes(x, y)) %>% geom_point() %>% facet_grid(. ~ z)
  l3 <- ggplot(df, aes(x, y)) %>% geom_point() %>% facet_grid(z ~ .)

  d0 <- layer_data(l0)
  d1 <- layer_data(l1)
  d2 <- layer_data(l2)
  d3 <- layer_data(l3)

  expect_equal(d0, d1)
  expect_equal(d0, d2)
  expect_equal(d0, d3)
})

test_that("facets with free scales scale independently", {
  l1 <- ggplot(df, aes(x, y)) %>% geom_point() %>%
    facet_wrap(~z, scales = "free")
  l2 <- ggplot2::ggplot(df, aes(x, y)) + ggplot2::geom_point() +
    ggplot2::facet_wrap(~z, scales = "free")
  expect_equal(l1, l2)
})


test_that("facet_wrap() gives same result", {
  p1 <- ggplot2::ggplot(mtcars, aes(disp, drat)) + ggplot2::geom_point() + ggplot2::facet_wrap(~cyl)
  p2 <- ggplot(mtcars, aes(disp, drat)) %>% geom_point() %>% facet_wrap(~cyl)
  expect_equal(p1, p2)

})

