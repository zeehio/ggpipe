#' ggplot with the pipe
#'
#' Use ggplot functions with the pipe instead of the "+" operator
#'
#' @examples
#' ggplot(mtcars) %>% geom_point(aes(mpg, cyl))
"_PACKAGE"

.pipe_decorator <- function(fun) {
  fun_args <- names(formals(fun))
  output_formals <- as.pairlist(c(alist(gplt = ), formals(fun)))
  argpass <- lapply(X = fun_args, as.name)
  names(argpass) <- fun_args
  output_body <- substitute({
      if (missing(gplt) || !inherits(gplt, "ggplot")) {
        call <- sys.call()
        call[[1]] <- x
        eval.parent(call)
      } else {
        gplt + do.call(x, y)
      }
    }, list(x = fun, y = argpass))
  as.function(c(output_formals, output_body))
}

.ggplot2ns <- asNamespace("ggplot2")
.ggplot2funs <- sort(getNamespaceExports("ggplot2"))
.funs_to_pipe <- sort(c(
  .ggplot2funs[c(grep("^aes.*", .ggplot2funs),
                 grep("^coord_.*", .ggplot2funs),
                 grep("^facet_.*", .ggplot2funs),
                 grep("^geom_.*", .ggplot2funs),
                 grep("^scale_[^.]*$", .ggplot2funs),
                 grep("^stat_.*", .ggplot2funs),
                 grep("^theme.*", .ggplot2funs))],
  "ggtitle", "annotate"))
.funs_to_pipe <- .funs_to_pipe[vapply(X = .funs_to_pipe,
                                      function(x) {
                                        fun <- get(x, envir = .ggplot2ns)
                                        is.function(fun)
                                        },
                                      logical(1))]
for (.fun_name in .ggplot2funs) {
    .fun <- get(.fun_name, envir = .ggplot2ns)
    if (.fun_name %in% .funs_to_pipe) {
      assign(.fun_name, .pipe_decorator(.fun))
    }
}

.document_funs1 <- function() {
  paste0("@aliases ", paste(.funs_to_pipe, collapse = " "))
}

.document_funs2 <- function() {
  output <- c(
    "@details",
    "These functions are imported from ggplot2. They are adapted to",
    "have a first argument called \"gplt\", that is the \\code{ggplot}",
    "object. Follow the links below to see their documentation.")

  output <- c(output, "\\describe{")
  for (fun_name in .funs_to_pipe) {
    output <- c(
      output,
      paste0("  \\item{", fun_name, "}{\\code{\\link[", "ggplot2", "]{", fun_name, "}}}"))
  }
  output <- c(output, "}")
  output
}

#' Save a ggplot
#'
#' With respect to [ggplot2::ggsave()], this function checks the types
#' and swaps the filename and plot arguments if needed, so it is pipeable
#' @inheritDotParams ggplot2::ggsave
#'
#' @seealso ggplot2::ggsave
#'
ggsave <- function(filename, plot, ...) {
  if (inherits(filename, "ggplot") || inherits(filename, "grob")) {
    gplt <- filename
    filename <- plot
  } else {
    gplt <- plot
  }
  ggplot2::ggsave(filename, plot = gplt, ...)
}


#' @title ggplot2 functions modified to work with the pipe
#' @name ggplot-pipeified
#' @eval .document_funs1()
#' @keywords internal
#'
#' @eval .document_funs2()
#'
.ggplot_pipeified_funs <- NULL


#' @docType import
#' @name reexports
#' @aliases reexports %>%
#' @keywords internal
magrittr::`%>%`
