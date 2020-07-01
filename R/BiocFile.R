### =========================================================================
### Import/export support
### -------------------------------------------------------------------------

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Classes files and connections
###

### BiocFile is a base class for high-level file abstractions, where
### subclasses are associated with a particular file format/type. It
### wraps a low-level representation of a file, currently either a
### path/URL or connection.

#' @importFrom BiocGenerics path
#' @importFrom GenomicRanges GRanges GRangesList score strand
#' @importFrom IRanges RleList RangesList
#' @importFrom RCurl getURL
#' @importFrom S4Vectors SimpleList isSingleString
#' @importFrom methods findMethods getClass getClassDef is new packageSlot show
#' @importFrom tools file_ext file_path_as_absolute file_path_sans_ext
#' @export
setClass("BiocFile", representation(resource = "character_OR_connection"),
         contains = "VIRTUAL")

#' @export
setClass("BiocFileList",
         prototype = prototype(elementType = "BiocFile"),
         contains = "SimpleList")

#' @export
BiocFileList <- function(files) {
    new("BiocFileList", listData = files)
}

###' @export
###setMethod("showAsCell", "BiocFileList", function(object) {
###    showAsCell(vapply(object, path, character(1L)))
###})

#' @export
resource <- function(x) x@resource

#' @export
`resource<-` <- function(x, value) {
    x@resource <- value
    x
}

#' @export
setGeneric("fileFormat", function(x) NULL)

#' @export
setMethod("fileFormat", "character", function(x) fileFormat(FileForFormat(x)))

#' @export
setMethod("fileFormat", "BiocFile", function(x)
    tolower(sub("File$", "", class(x))))

#' @export
setMethod("path", "BiocFile", function(object, ...) {
    r <- resource(object)
    if (!is.character(r))
      stop("Connection resource requested as a path")
    r
})

#' @export
setMethod("show", "BiocFile", function(object) {
    r <- resource(object)
    if (!isSingleString(r))
        r <- summary(r)$description
    cat(class(object), "object\nresource:", r, "\n")
})

#' @export
FileForFormat <- function(path, format = file_ext(path)) {
    if (!(isSingleString(path) || is(path, "connection")))
        stop("'path' must be a single string or a connection object")
    if (!isSingleString(format))
        stop("'format' must be a single string")
    if (format == "")
        stop("Cannot detect format (no extension found in file name)")
    fileClassName <- paste0(format, "File")
    signatureClasses <- function(fun, pos) {
        matrix(unlist(findMethods(fun)@signatures), 3)[pos,]
    }
    fileClassNames <- unique(c(signatureClasses(export, 2),
                               signatureClasses(import, 1)))
    fileClassNames <- fileClassNames[grepl("File$", fileClassNames)]
    fileSubClassNames <- unlist(lapply(fileClassNames, function(x) {
        names(getClassDef(x)@subclasses)
    }), use.names = FALSE)
    fileClassNames <- c(fileClassNames, fileSubClassNames) 
    fileClassIndex <- match(tolower(fileClassName),
                            tolower(fileClassNames))
    if (is.na(fileClassIndex))
        stop("Format '", format, "' unsupported")
    fileClassName <- fileClassNames[fileClassIndex]
    fileClass <- getClass(fileClassName)
    pkg <- packageSlot(fileClass)
    if (is.null(pkg) || identical(pkg, ".GlobalEnv"))
        ns <- topenv()
    else ns <- getNamespace(pkg[1])
    constructorName <- fileClassName
    if(!exists(constructorName, ns)) {
        parentClassNames <- names(getClass(constructorName)@contains)
        constructorName <- names(which(sapply(parentClassNames, exists, ns)))[1]
        if (is.na(constructorName))
            stop("No constructor found for ", fileClassName)
     }
    get(constructorName, ns)(path)
}

#' @export
setMethod("as.character", "BiocFile", function(x) path(x))

### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
### Utilities
###

#' @export
setGeneric("bestFileFormat",
function(x, dest, ...) standardGeneric("bestFileFormat"))

#' @export
setMethod("bestFileFormat", c("GenomicRanges", "ANY"),
function(x, dest) {
    ## have numbers on a single strand, use BigWig
    if (is.numeric(score(x)) && length(unique(strand(x))) == 1L)
    "bw"
    else "bed"
})

#' @export
setMethod("bestFileFormat", c("GRangesList", "ANY"), function(x, dest) {
    "bed" # need hierarchical structure
})

#' @export
setMethod("bestFileFormat", c("RleList", "ANY"), function(x, dest) {
    "bw" # e.g., coverage
})

#' @export
setMethod("bestFileFormat", c("IntegerRangesList", "ANY"), function(x, dest) {
    "bed" # just ranges...
})

isURL <- function(uri) {
    if (!isSingleString(uri))
        return(FALSE)
    windowsDriveLetter <- .Platform$OS.type == "windows" &&
        grepl("^[A-Za-z]:[/\\]", uri)
    grepl("^[A-Za-z]+:", uri) && !windowsDriveLetter
}

.parseURI <- function(uri) {
    if (!isURL(uri)) {
        list(scheme = "", path = uri)
    } else {
        parsed <- list(scheme = "", path = uri)
        if (any(startsWith(uri, c("http", "ftp"))))
            parsed$scheme <- "con"
        else
            parsed$scheme <- "file"
        if (parsed$scheme == "file" && .Platform$OS.type == "windows") 
            parsed$path <- substring(parsed$path, 2) # trim '/' from '/C:/foo/bar.txt'
  }
  parsed
}

resourceDescription <- function(x) {
    r <- resource(x)
    if (is(r, "connection"))
        r <- summary(r)$description
    r
}

.ConnectionManager <- setRefClass("ConnectionManager",
                                  fields = c(connections = "list"))

manager <- function() .ConnectionManager()

connection <- function(manager, x, open = "") {
    connectionForResource(manager, resource(x), open = open)
}

## Connection management (similar to memory management)

manage <- function(manager, con) {
    manager$connections <- unique(c(manager$connections, list(con)))
    attr(con, "manager") <- manager
    con
}

managed <- function(manager, con) {
    con %in% manager$connections
}

unmanage <- function(manager, con) {
    manager$connections <- setdiff(manager$connections, con)
    attr(con, "manager") <- NULL
    con
}

release <- function(manager, con) {
    if (managed(manager, con)) {
        unmanage(manager, con)
        close(con)
    }
    con
}
