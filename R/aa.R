#' ggplot with the pipe
#'
#' Use ggplot functions with the pipe instead of the "+" operator
#'
#' @examples
#' ggplot(mtcars) %>% geom_point(aes(mpg, cyl))
"_PACKAGE"

.pipe_decorator <- function(f) {
  output_formals <- append(formals(f), alist(gplt = ), after = 0)
  output_body <- quote({
    needs_piping <- tryCatch({
      # Detect if the gplt argument or the first argument are of ggplot type:
      # If it is true, get the ggplot object and remove it from the arguments
      # in the call:
      call <- sys.call()
      if ("gplt" %in% names(call)) {
        gplt <- eval.parent(call[["gplt"]])
        to_remove <- "gplt"
      } else {
        gplt <- eval.parent(call[[2]])
        to_remove <- 2
      }
      stopifnot(inherits(gplt, "ggplot"))
      call[[to_remove]] <- NULL
      TRUE
    }, error = function(e) {
      FALSE
    })
    # Replace the called function by the ggplot one
    call[[1]] <- f
    # Evaluate
    if (needs_piping) {
      gplt + eval.parent(call)
    } else {
      eval.parent(call)
    }
    })
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
  "ggtitle", "annotate", "xlab", "ylab", "labs"))
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
#' @param filename File name to create on disk
#' @param plot Plot to save
#' @inheritDotParams ggplot2::ggsave
#' @return the ggplot object
#' @seealso ggplot2::ggsave
#'
ggsave <- function(filename, plot, ...) {
  if (inherits(filename, "ggplot") || inherits(filename, "grob")) {
    gplt <- filename
    filename <- plot
  } else if (is.character(filename)) {
    if (missing(plot)) {
      gplt <- ggplot2::last_plot()
    } else {
      gplt <- plot
    }
  }
  ggplot2::ggsave(filename, plot = gplt, ...)
  invisible(gplt)
}

#' Returns the data from the ggplot
#'
#' Returns the data frame from the \code{gplt} object
#'
#' @param gplt A ggplot object
#' @param layer ggplot plots can include different data in other layers. This is
#'              the index of the layer to return (by default the global data
#'              is returned)
#' @return a data frame
#'
#' @examples
#'
#' data(iris)
#' ggplot(iris) %>%
#'   geom_point(aes(x = Sepal.Length, y = Sepal.Width, color = Species)) %>%
#'   ggsave("demo.png") %>%
#'   unggplot() %>%
#'   head()
unggplot <- function(gplt, layer = NULL) {
  if (is.null(layer)) {
    return(gplt$data)
  } else {
    if (inherits(gplt$layers[[layer]]$data, "waiver")) {
      return(gplt$data)
    } else {
      return(gplt$layers[[layer]]$data)
    }
  }
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
