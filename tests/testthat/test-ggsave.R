context("ggsave")

test_that("ggsave creates file", {
  path <- tempfile()
  on.exit(unlink(path))

  expect_false(file.exists(path))
  p <- ggplot(mpg, aes(displ, hwy)) %>%
    geom_point() %>%
    ggsave(path, device = "pdf", width = 5, height = 5)
  expect_true(file.exists(path))
})
