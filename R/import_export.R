#' @name IO
#'
#' @title Import and export
#'
#' @description The functions `import` and `export` load and save objects from
#'   and to particular file formats.
#'
#' @aliases export export,ANY,connection,character-method
#'   export,ANY,connection,missing-method
#'   export,ANY,missing,character-method export,ANY,character,missing-method
#'   export,ANY,character,character-method
#'   import
#'   import,connection,character,ANY-method import,connection,missing,ANY-method
#'   import,character,missing,ANY-method import,character,character,ANY-method
#'   import,missing,ANY,character-method
#'
#' @param object The object to export.
#'
#' @param con The connection from which data is loaded or to which data is
#'   saved. If this is a `character` vector, it is assumed to be a file name and
#'   a corresponding file connection is created and then closed after exporting
#'   the object. If it is a [BiocFile-class] derivative, the data is loaded from
#'   or saved to the underlying resource. If missing, the function will return
#'   the output as a character vector, rather than writing to a connection.
#'
#' @param format The format of the output. If missing and `con` is a
#' file name, the format is derived from the file extension. This argument is
#' unnecessary when `con` is a derivative of [BiocFile-class].
#'
#' @param text If `con` is missing, this can be a character vector directly
#'   providing the string data to import.
#'
#' @param ... Parameters to pass to the format-specific method.
#'
#' @return If `con` is missing, a character vector containing the string output.
#'   Otherwise, nothing is returned.
#'
#' @author Michael Lawrence
#'
#' @seealso Format-specific options for the popular formats:
#' \acronym{\link[rtracklayer:GFFFile]{GFF}},
#' \acronym{\link[rtracklayer:BEDFile]{BED}},
#' \acronym{\link[rtracklayer:BED15File]{Bed15}},
#' \acronym{\link[rtracklayer:BEDGraphFile]{bedGraph}},
#' \acronym{\link[rtracklayer:WIGFile]{WIG}},
#' \acronym{\link[rtracklayer:BigWigFile]{BigWig}}
#'
#' @keywords IO
#' @examples
#' ## To illustrate export(), import(), and yeild(), we create a class, CSVFILE
#' .CSVFile <- setClass("CSVFile", contains = "BiocFile")
#'
#' ## Constructor
#' CSVFile <- function(resource) {
#'     .CSVFile(resource = resource)
#' }
#'
#' ## Define import
#' setMethod("import", "CSVFile",
#'     function(con, format, text, ...) {
#'         read.csv(resource(con), ...)
#'     }
#' )
#'
#' ## Define export
#' setMethod("export", c("data.frame", "CSVFile"),
#'     function(object, con, format, ...) {
#'         write.csv(object, resource(con), ...)
#'     }
#' )
#'
#' ## Usage
#' temp <- tempfile(fileext = ".csv")
#' csv <- CSVFile(temp)
#'
#' export(mtcars, csv)
#' df <- import(csv)
#'
NULL

# Import ------------------------------------------------------------------

#' @rdname IO
#' @export
setGeneric(
    "import", function(con, format, text, ...) standardGeneric("import")
)

#' @rdname IO
#' @export
setMethod("import", c("connection", "character"),
    function(con, format, text, ...) {
        import(FileForFormat(con, format), ...)
    }
)

#' @rdname IO
#' @export
setMethod("import", c("connection", "missing"),
    function(con, format, text, ...) {
        format <- file_ext(summary(con)$description)
        import(con, format, ...)
    }
)

#' @rdname IO
#' @export
setMethod("import", c("character", "missing"),
    function(con, format, text, ...) {
        import(FileForFormat(con), ...)
    }
)

#' @rdname IO
#' @export
setMethod("import", c("character", "character"),
    function(con, format, text, ...) {
        import(FileForFormat(con, format), ...)
    }
)

#' @rdname IO
#' @export
setMethod("import", c(con = "missing", text = "character"),
    function(con, format, text, ...) {
        con <- file()
        on.exit(close(con))
        writeLines(text, con)
        obj <- import(FileForFormat(con, format), ...)
        obj
    }
)

# Export ------------------------------------------------------------------

#' @rdname IO
#' @export
setGeneric(
    "export", function(object, con, format, ...) standardGeneric("export")
)

#' @rdname IO
#' @export
setMethod("export", c(con = "connection", format = "character"),
    function(object, con, format, ...) {
        export(object, FileForFormat(con, format), ...)
    }
)

#' @rdname IO
#' @export
setMethod("export", c(con = "connection", format = "missing"),
    function(object, con, format, ...) {
        format <- file_ext(summary(con)$description)
        export(object, con, format, ...)
    }
)

#' @rdname IO
#' @export
setMethod("export", c(con = "missing", format = "character"),
    function(object, con, format, ...) {
        con <- file()
        on.exit(close(con))
        export(object, con, format, ...)
        text <- readLines(con, warn = FALSE)
        text
    }
)

#' @rdname IO
#' @export
setMethod("export", c(con = "character", format = "missing"),
    function(object, con, format, ...) {
        export(object, FileForFormat(con), ...)
    }
)

#' @rdname IO
#' @export
setMethod("export", c(con = "character", format = "character"),
    function(object, con, format, ...) {
        export(object, FileForFormat(con, format), ...)
    }
)
