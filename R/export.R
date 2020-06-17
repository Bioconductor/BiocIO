### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Export
###

#' @title Import
#' @description
#'  The function \code{export} save objects from particular file formats. The
#'  rtracklayer package implements support for a number of annotation and
#'  sequence formats.
#' @alias export export,ANY,connection,character-method
#'        export,ANY,connection,missing-method
#'        export,ANY,CompressedFile,missing-method
#'        export,ANY,missing,character-method
#'        export,ANY,character,missing-method
#'        export,ANY,character,character-method
#' @usage export(object, con, format, ...)
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
