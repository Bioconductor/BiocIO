### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Export
###

#' @export
setGeneric("export",
           function(object, con, format, ...) standardGeneric("export"))

setMethod("export", c(con = "connection", format = "character"),
          function(object, con, format, ...)
          {
            export(object, FileForFormat(con, format), ...)
          })

setMethod("export", c(con = "connection", format = "missing"),
          function(object, con, format, ...)
          {
            format <- file_ext(summary(con)$description)
            export(object, con, format, ...)
          })

setMethod("export", c(con = "missing", format = "character"),
          function(object, con, format, ...)
          {
            con <- file()
            on.exit(close(con))
            export(object, con, format, ...)
            text <- readLines(con, warn = FALSE)
            text
          })
setMethod("export", c(con = "character", format = "missing"),
          function(object, con, format, ...)
          {
            export(object, FileForFormat(con), ...)
          })
setMethod("export", c(con = "character", format = "character"),
          function(object, con, format, ...)
          {
            export(object, FileForFormat(con, format), ...)
          })
