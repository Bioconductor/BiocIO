### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Import
###

#' @export
setGeneric("import",
           function(con, format, text, ...) standardGeneric("import"))

setMethod("import", c("connection", "character"),
          function(con, format, text, ...)
          {
              import(FileForFormat(con, format), ...)
          })
setMethod("import", c("connection", "missing"),
          function(con, format, text, ...)
          {
              format <- file_ext(summary(con)$description)
              import(con, format, ...)
          })
setMethod("import", c("character", "missing"),
          function(con, format, text, ...)
          {
              import(FileForFormat(con), ...)
          })
setMethod("import", c("character", "character"),
          function(con, format, text, ...)
          {
              import(FileForFormat(con, format), ...)
          })
setMethod("import", c(con = "missing", text = "character"),
          function(con, format, text, ...)
          {
              con <- file()
              on.exit(close(con))
              writeLines(text, con)
              obj <- import(FileForFormat(con, format), ...)
              obj
          })
