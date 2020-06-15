## implement import/export
## Define CSVFile class

CSVFile <- setClass("CSVFile", contains = "BiocFile")

CSVFile <-
    function(resource)
{
    .CSVFile(resource = resource)
}

setMethod("import", "CSVFile",
    function(con, format, text, ...)
{
    read.csv(resource(con), ...)
})

setMethod("export", "CSVFile",
    function(object, con, format, ...)
{
    write.csv(object, resource(con), ...)
})

setGeneric(
    "yield",
    function(con, format, text, ...)
        standardGeneric("yield"),
    signature = "con"
)

setMethod("yield", "CSVFile",
    function(con, format, text, ...)
{
    con <- resource(con)
    if (!is(con, "connection"))
        con <- file(con)
    if (!identical("opened", summary(con)$opened))
        open(con, "rt")
    tryCatch({
        read.csv(con, ...)
    }, error = function(e) {
        cond <- simpleCondition(e)
        class(cond) <- c("EOF", class(cond))
        signalCondition(cond)
    })
})

setGeneric(
    "iyield",
    function(con, format, text, ..., filter, select)
        standardGeneric("yield"),
    signature = "con"
)

setMethod("iyield", "CSVFile",
    function(con, format, text, ..., yield_size, select)
{
    con <- resource(con)
    if (!is(con, "connection"))
        con <- file(con)
    if (!identical("opened", summary(con)$opened))
        open(con, "rt")
    tryCatch({
        read.csv(con, nrow = yield_size, ...)
    }, error = function(e) {
        cond <- simpleCondition(e)
        class(cond) <- c("EOF", class(cond))
        signalCondition(cond)
    })
})

setGeneric(
    "ifilter",
    function(con, format, text, filter) standardGeneric("iofilter")
    signature("con")
)
