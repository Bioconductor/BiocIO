# Import/export support ---------------------------------------------------

# Classes files and connections -------------------------------------------

#' @name BiocFile-class
#'
#' @title BiocFile class objects
#'
#' @description `BiocFile` is the base virtual class for high-level file
#'   abstractions where subclasses are associated with a particular file format
#'   or type. It wraps a low-level representation of a file, currently either a
#'   path, URL, or connection object. We can represent a list of `BiocFile`
#'   objects with a `BiocFileList`.
#'
#' @aliases BiocFile BiocFileList-class show,BiocFile-method
#'   as.character,BiocFile-method
#'   path path,BiocFile-method
#'   resource resource<-
#'   fileFormat fileFormat,BiocFile-method fileFormat,character-method
#'   FileForFormat
#'   BiocFileList
#'
#' @section Accessor Methods:
#'   In the code snippets below, `x` represents a `BiocFile` object.
#'
#' - `path(x)`: Gets the path, as a `character` vector, to the resource
#'   represented by the `BiocFile` object, if possible.
#' - `resource(x)`: Gets the low-level resource, either a character vector
#'   (a path or URL) or a connection.
#' - `fileFormat(x)`: Gets a string identifying the file format. Can also
#'   be called directly on a character file path, in which case it uses a
#'   heuristic based on the file extension.
#'
#' @author Michael Lawrence
#'
#' @seealso Implementing classes include:
#'   [BigWigFile][rtracklayer::BigWigFile-class],
#'   [TwoBitFile][rtracklayer::TwoBitFile-class],
#'   [BEDFile][rtracklayer::BEDFile-class],
#'   [GFFFile][rtracklayer::GFFFile-class],
#'   [WIGFile][rtracklayer::WIGFile-class]
#'
#' @docType class
#'
#' @keywords methods classes
#'
#' @return For constructors, an instance of that class. For extractors such as
#'   `resource` and `path`, typically a `character` vector of the file path.
#'   For `FileForFormat`, a convenient instance of the class for which the
#'   input file corresponds to.
#'
#' @examples
#' ## For our examples, we create a class called CSVFILE that extends BiocFile
#' .CSVFile <- setClass("CSVFile", contains = "BiocFile")
#'
#' ## Constructor
#' CSVFile <- function(resource) {
#'     .CSVFile(resource = resource)
#' }
#'
#' setMethod("import", c(con = "CSVFile"), function(con, format, text, ...) {
#'     read.csv(resource(con), ...)
#' })
#'
#' ## Define export
#' setMethod("export", c(object = "data.frame", con = "CSVFile"),
#'     function(object, con, format, ...) {
#'         write.csv(object, resource(con), ...)
#'     }
#' )
#'
#' ## Recommend CSVFile class for .csv files
#' temp <- tempfile(fileext = ".csv")
#' FileForFormat(temp)
#'
#' ## Create CSVFile
#' csv <- CSVFile(temp)
#'
#' ## Display path of file
#' path(csv)
#'
#' ## Display resource of file
#' resource(csv)
#'
#' @importFrom BiocGenerics path
#' @importFrom S4Vectors SimpleList isSingleString
#' @importFrom methods findMethods getClass getClassDef is new packageSlot show
#' @importFrom tools file_ext file_path_as_absolute file_path_sans_ext
#'
#' @exportClass BiocFile
setClass(
    "BiocFile",
    representation(resource = "character_OR_connection"),
    contains = "VIRTUAL"
)

#' @rdname BiocFile-class
#' @exportClass BiocFileList
setClass(
    "BiocFileList",
    prototype = prototype(elementType = "BiocFile"),
    contains = "SimpleList"
)

#' @rdname BiocFile-class
#' @param files `character()` A vector of file paths for the `BiocFileList`
#'   constructor
#' @export
BiocFileList <- function(files) new("BiocFileList", listData = files)

#' @rdname BiocFile-class
#'
#' @export
setGeneric("resource", function(x) standardGeneric("resource"))

#' @rdname BiocFile-class
#' @export
setGeneric("resource<-", function(x, value) standardGeneric("resource<-"))

#' @rdname BiocFile-class
#' @export
setMethod("resource", "BiocFile", function(x) x@resource)

#' @rdname BiocFile-class
#'
#' @param x A `BiocFile` instance
#'
#' @param path,value Either a `character` or `connection` object to replace the
#'   original resource
#'
#' @export
setReplaceMethod("resource", c("BiocFile", "character_OR_connection"),
    function(x, value) {
        x@resource <- value
        x
    }
)

#' @rdname BiocFile-class
#' @export
setGeneric("fileFormat", function(x) NULL)

#' @rdname BiocFile-class
#' @export
setMethod("fileFormat", "character", function(x) fileFormat(FileForFormat(x)))

#' @rdname BiocFile-class
#' @export
setMethod(
    "fileFormat", "BiocFile", function(x) tolower(sub("File$", "", class(x)))
)

#' @rdname BiocFile-class
#'
#' @param object A `BiocFile` instance
#'
#' @param ... additional arguments to lower-level functions, not used.
#'
#' @export
setMethod("path", "BiocFile", function(object, ...) {
    resource <- resource(object)
    if (!is.character(resource))
        stop("Connection resource requested as a path")
    resource
})

#' @rdname BiocFile-class
#' @export
setMethod("show", "BiocFile", function(object) {
    resource <- resource(object)
    if (!isSingleString(resource))
        resource <- summary(resource)$description
    cat(class(object), "object\nresource:", resource, "\n")
})

#' @rdname BiocFile-class
#'
#' @param format `character(1)` The file extension conducive to a file class
#'   name, e.g., `CSVFile`
#'
#' @param suffix `character(1)` The suffix to append to the format class name,
#'   e.g., `File` for a class `CSVFile`.
#'
#' @param prefix `character(1)` The prefix to prepend to the format class name,
#'   e.g., `Spatial` for a class `SpatialCSV`.
#'
#' @section FileForFormat:
#'   The `prefix` and `suffix` arguments are used to filter the class names to
#'   those that match the pattern `paste0(prefix, format, suffix)`. If either
#'   `prefix` or `suffix` are `NULL`, they are ignored. Note that the search is
#'   case insensitive and does require the `format` to be in the name of the
#'   class.
#'
#' @export
FileForFormat <-
    function(path, format = file_ext(path), prefix = NULL, suffix = "File")
{
    if (!(isSingleString(path) || is(path, "connection")))
        stop("'path' must be a single string or a connection object")
    if (!isSingleString(format))
        stop("'format' must be a single string")
    if (format == "")
        stop("Cannot detect format (no extension found in file name)")
    fileClassName <- paste0(prefix, format, suffix)
    signatureClasses <- function(fun, pos) {
        matrix(unlist(findMethods(fun)@signatures), 3)[pos,]
    }
    fileClassNames <- unique(
        c(signatureClasses(export, 2), signatureClasses(import, 1))
    )
    cpattern <-
        if (!missing(prefix)) paste0("^", prefix) else paste0(suffix, "$")
    classHits <- grepl(cpattern, fileClassNames, ignore.case = TRUE)
    fileClassNames <- fileClassNames[classHits]
    fileSubClassNames <- unlist(lapply(fileClassNames, function(x) {
        names(getClassDef(x)@subclasses)
    }), use.names = FALSE)
    fileClassNames <- c(fileClassNames, fileSubClassNames)
    fileClassIndex <- match(tolower(fileClassName), tolower(fileClassNames))
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
        constructorName <-
            names(which(vapply(parentClassNames, exists, logical(1), ns)))[1]
        if (is.na(constructorName))
            stop("No constructor found for ", fileClassName)
    }
    get(constructorName, ns)(path)
}

#' @rdname BiocFile-class
#' @export
setMethod("as.character", "BiocFile", function(x) path(x))
