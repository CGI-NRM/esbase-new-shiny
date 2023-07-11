# nolint start: object_name_linter.
dataHolder <- function(...) {
  args <- rlang::list2(...)
  if ((length(args) > 0) && (is.null(names(args)) || any(names(args) == "")))
    rlang::abort("All arguments passed to dataHolder() must be named.")

  values <- structure(
    list(
      impl = fastmap::fastmap()
    ),
    class = "dataholder"
  )

  lapply(names(args),
         \(name) {
           .subset2(values, "impl")$set(name, args[[name]])
         })

  values
}

checkName <- function(x) {
  if (!is.character(x) || length(x) != 1) {
    rlang::abort("Must use single string to index into dataholder.")
  }
}

print.dataholder <- function(x, ...) {
  cat("<DataHolder>", "\n")
  cat("  Values:   ", paste0(.subset2(x, "impl")$keys(sort = TRUE), collapse = ", "), "\n")
}

is.dataholder <- function(x) inherits(x, "dataholder")

`$.dataholder` <- function(x, name) {
  checkName(name)
  .subset2(x, "impl")$get(name)
}

`[[.dataholder` <- `$.dataholder`

`$<-.dataholder` <- function(x, name, value) {
  checkName(name)
  .subset2(x, "impl")$set(name, value)
  x
}

`[[<-.dataholder` <- `$<-.dataholder`

`[.dataholder` <- function(values, name) {
  rlang::abort("Can't index dataholder with `[`.")
}

`[<-.dataholder` <- function(values, name, value) {
  rlang::abort("Can't index dataholder with `[`.")
}
# nolint end
