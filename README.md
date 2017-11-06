# ggpipe

[![Travis-CI Build Status](https://travis-ci.org/zeehio/ggpipe.svg?branch=master)](https://travis-ci.org/zeehio/ggpipe)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/ggpipe)](https://cran.r-project.org/package=ggpipe)

*ggplot with the pipe*

This package wraps most ggplot functions so they work with the pipe "%>%".

    # as ggpipe wraps ggplot2 functions, do not use library(ggplot2) in your scripts
    library(ggpipe)
    ggplot(mtcars) %>% 
      geom_point(aes(x = mpg, y = cyl)) %>%
      ggsave("demo.png")

It may have some rough corners, but works fine most of the time. If it fails
open an issue with a small reproducible example.
