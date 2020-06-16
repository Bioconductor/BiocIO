## implement import/export
## Define CSVFile class

.CSVFile <- setClass("CSVFile", contains = "BiocFile")

#' @export
CSVFile <-
    function(resource)
{
    .CSVFile(resource = resource)
}

#' @export
setMethod("import", "CSVFile",
    function(con, format, text, ...)
{
    read.csv(resource(con), ...)
})

#' @export
setMethod("export", "CSVFile",
    function(object, con, format, ...)
{
    write.csv(object, resource(con), ...)
})

#' @export
setMethod("yield", "CSVFile",
    function(con, format, text, size, ...)
{
    con <- resource(con)
    if (!is(con, "connection"))
        con <- file(con)
    if (!identical("opened", summary(con)$opened))
        tryCatch({
            open(con, "rt")
        }, error = function(e) {
            cond <- simpleCondition(e)
            class(cond) <- c("File not found", class(cond))
            signalCondition(cond)
        })
    tryCatch({
        read.csv(con, ...)
    }, error = function(e) {
        cond <- simpleCondition(e)
        class(cond) <- c("EOF", class(cond))
        signalCondition(cond)
    })
})
