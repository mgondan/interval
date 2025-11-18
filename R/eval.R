## File containing internal functions

# Evaluate a string expression via rolog
.intarith_string <- function(expr, flags = NULL, env = globalenv()) {
  expr <- .parse_input(expr)
  flags <- c(flags, list(cat = FALSE))
  t <- rolog::once(call("interval", expr, expression(X), flags), env = env)
  return(.get_result(t))
}

# Convert a string to a prolog term via rolog
.parse_input <- function(x) {
  expr <- rolog::once(call("term_string", expression(T), x))
  if (!is.list(expr)) {
    stop(paste("Could not parse input string:", x), call. = FALSE)
  }
  return(expr$T)
}

# Format the result from Prolog
.get_result <- function(t) {
  if (is.list(t) && t$X[[1]] == "...") {
    return(interval(t$X[[2]], t$X[[3]]))
  }
  if (is.list(t)) {
    return(t$X)
  }
  return(t)
}

# General function used by S3 methods
.eval <- function(op, ...) {
  args <- Filter(Negate(is.null), list(...))
  args <- sapply(args, .arg2char)
  args <- paste(args, collapse = ",")
  expr <- paste0(op, "(", args, ")")
  .intarith_string(expr)
}

.arg2char <- function(x) {
  if (.is_interval(x))
    return(paste0("...(", x$l, ",", x$u, ")"))
  return(as.character(x))
}

.is_interval <- function(x) {
  inherits(x, "interval")
}

# Manual dispatching instead of S3 system.
# Requires that a function ".%name%_interval" and
# ".%name%_default" is defined with at least the same number of arguments.
.dispatch <- function(name, ...) {
  dots <- list(...)
  if (any(sapply(dots, .is_interval))) {
    do.call(paste0(".", name, "_interval"), dots)
  } else {
    do.call(paste0(".", name, "_default"), dots)
  }
}